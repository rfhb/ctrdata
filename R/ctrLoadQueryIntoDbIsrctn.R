### ctrdata package

#' ctrLoadQueryIntoDbIsrctn
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_query
#' @importFrom utils URLdecode
#' @importFrom httr with_config config
#'
ctrLoadQueryIntoDbIsrctn <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    documents.path,
    documents.regexp,
    annotation.text,
    annotation.mode,
    only.count,
    con,
    verbose,
    queryupdateterm) {

  ## isrctn api ---------------------------------------------------------------

  # ISRCTN translation to API v0.4 2021-02-04
  # - limit can be set to arbitrarily high number
  # - no pagination or batching
  # - internal means XML
  queryIsrctnRoot <- "https://www.isrctn.com/"
  queryIsrctnType1 <- "api/query/format/internal?limit="
  queryIsrctnType2 <- "api/query/format/internal?limit=0&"
  #
  # convert parameters from search queryterm such as
  # "q=neuroblastoma+OR+lymphoma&filters=phase%3APhase+III%2CLE+lastEdited%3A2021-01-01"
  # "q=&filters=phase%3APhase+III%2CLE+lastEdited%3A2021-01-01"
  # into to api format such as
  # "q=(neuroblastoma OR lymphoma) AND phase:"Phase+III" AND lastEdited LE 2021-01-01T00:00:00.000Z"
  #
  # - ensure we can use text processing
  queryterm <- utils::URLdecode(queryterm)
  # - generate api terms
  apiterm <- queryterm
  apiterm <- sub("&filters=", ",", apiterm)
  apiterm <- strsplit(apiterm, ",")[[1]]
  # - remove naked q
  apiterm <- apiterm[!grepl("^q=$", apiterm)]
  # - translate "LE+lastEdited:2021-04-01"
  #   into      "lastEdited LE 2021-04-01T00:00:00.000Z"
  apiterm <- vapply(
    apiterm,
    function(a) sub("^(.*?)[+](.*?)[:](.*)$", "\\2 \\1 \\3", a),
    character(1L),
    USE.NAMES = FALSE
  )
  # - add time if date does not end with it
  apiterm <- vapply(
    apiterm,
    function(a) sub("(.+[0-9]{4}-[0-9]{2}-[0-9]{2})$", "\\1T00:00:00.000Z", a),
    character(1L),
    USE.NAMES = FALSE
  )
  #
  # - quote anything right of colon; this is an advanced search URL:
  #   https://www.isrctn.com/search?q=&filters=phase%3APhase+III
  #   which needs to be changed to phase:"Phase III", noting
  #   `+` is interpreted by the API as space, thus unchanged
  termstoquote <- grepl("[ +]", sub("^.*?[:](.+)$", "\\1", apiterm))
  apiterm[termstoquote] <- vapply(
    apiterm[termstoquote],
    function(a) sub("^(.*?)[:](.+)$", "\\1:\"\\2\"", a),
    character(1L),
    USE.NAMES = FALSE
  )
  # - put q in brackets to respect logical operators
  qtoquote <- grepl("^q=.+$", apiterm)
  apiterm[qtoquote] <- sub("^q=(.+)$", "q=(\\1)", apiterm[qtoquote])
  # - collapse
  apiterm <- paste0(apiterm, collapse = " AND ")
  # - add empty q if q is missing
  if (!startsWith(apiterm, "q=")) apiterm <- paste0("q=", apiterm)
  # - inform user
  if (verbose) message("DEBUG: apiterm is ", apiterm)

  ## checks -------------------------------------------------------------------

  message("* Checking trials in ISRCTN...")

  # - check number of trials to be downloaded
  isrctnfirstpageurl <- paste0(
    queryIsrctnRoot, queryIsrctnType2, apiterm, queryupdateterm
  )
  #
  tmp <- try(
    suppressWarnings(
      xml2::read_xml(
        x = url(utils::URLencode(isrctnfirstpageurl))
      )
    ),
    silent = TRUE
  )
  #
  if (inherits(tmp, "try-error")) {
    stop("Host ", queryIsrctnRoot, " not working as expected, ",
         "cannot continue: ", tmp[[1]],
         call. = FALSE
    )
  }
  #
  tmp <- try(xml2::xml_attr(tmp, "totalCount"), silent = TRUE)
  #
  # safeguard against no or unintended large numbers
  tmp <- suppressWarnings(as.integer(tmp))
  if (is.na(tmp) || !length(tmp)) {
    message("No trials or number of trials could not be determined: ", tmp)
    return(invisible(emptyReturn))
  }
  #
  if (tmp == 0L) {
    message("Search result page empty - no (new) trials found?")
    return(invisible(emptyReturn))
  }
  # otherwise continue

  # inform user
  message(
    "Retrieved overview, records of ", tmp, " ",
    "trial(s) are to be downloaded (estimate: ",
    format(tmp * 0.018, digits = 2), " MB)"
  )

  # only count?
  if (only.count) {
    # return
    return(list(
      n = tmp,
      success = NULL,
      failed = NULL
    ))
  }

  # exit if too many records
  if (tmp > 10000L) {
    stop(
      "These are ", tmp, " (more than 10,000) trials, this may be ",
      "unintended. Downloading more than 10,000 trials may not be supported ",
      "by the register; consider correcting or splitting queries"
    )
  }

  ## download -----------------------------------------------------------------

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  # prepare a file handle for temporary directory
  f <- paste0(tempDir, "/", "isrctn.xml")

  # inform user
  message("(1/3) Downloading trial file... ")

  # construct API call setting limit to number found above
  isrctndownloadurl <- paste0(
    queryIsrctnRoot, queryIsrctnType1, tmp, "&", apiterm, queryupdateterm
  )

  # get (download) trials in single file f
  tmp <- ctrMultiDownload(isrctndownloadurl, f, verbose = verbose)

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    message(
      "No studies downloaded. Please check 'queryterm' ",
      " or run again with verbose = TRUE"
    )
  }

  ## run conversion
  message("(2/3) Converting to JSON...", appendLF = FALSE)
  ctrConvertToJSON(tempDir, "isrctn2ndjson.php", verbose)

  ## run import
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(
    dir = tempDir,
    con = con,
    verbose = verbose
  )

  ## documents -----------------------------------------------------

  if (!is.null(documents.path)) {

    # check and create directory
    createdDir <- try(
      dir.create(documents.path, recursive = TRUE, showWarnings = FALSE),
      silent = TRUE)
    if (inherits(createdDir, "try-errror")) {
      warning("Directory could not be created for 'documents.path' ",
              documents.path, ", cannot download files", call. = FALSE)
    } else {

      # continue after if
      message("* Downloading documents into 'documents.path' = ", documents.path)

      # canonical directory path
      documents.path <- normalizePath(documents.path, mustWork = TRUE)
      if (createdDir) message("- Created directory ", documents.path)

      # get temporary file for trial ids and file names
      downloadsNdjson <- file.path(tempDir, "isrctn_downloads.ndjson")
      suppressMessages(unlink(downloadsNdjson))
      downloadsNdjsonCon <- file(downloadsNdjson, open = "at")

      # extract trial ids and file name and save in temporary file
      for (ndjsonFile in dir(
        path = tempDir, pattern = "^.+_trials_.*.ndjson$", full.names = TRUE)) {
        jqr::jq(
          file(ndjsonFile), # use digit prefix from trial as fileref
          '._id as $trialid |
          ([.attachedFiles.attachedFile[] | .name |
            capture("(?<n>^[0-9]+)[ _]").n][0]) as $fileprefix |
          .attachedFiles.attachedFile[] |
          {_id: $trialid, filename: .name, fileref1: .id, fileref2: $fileprefix}',
          flags = jqr::jq_flags(pretty = FALSE),
          out = downloadsNdjsonCon)
        message(". ", appendLF = FALSE)
      }
      close(downloadsNdjsonCon)

      # get document trial id and file name
      dlFiles <- jsonlite::stream_in(file(downloadsNdjson), verbose = FALSE)

      # documents download
      message("\n- Creating subfolder for each trial")

      # add destination file directory path
      dlFiles$filepath <- file.path(documents.path, dlFiles$`_id`)

      # create subdirectories by trial
      invisible(sapply(
        unique(dlFiles$filepath), function(i) if (!dir.exists(i))
          dir.create(i, showWarnings = FALSE, recursive = TRUE)
      ))

      # check if destination document exists
      dlFiles$filepathname <- file.path(dlFiles$filepath, dlFiles$filename)
      dlFiles$fileexists <- file.exists(dlFiles$filepathname) &
        file.size(dlFiles$filepathname) > 10L

      # calculate urls
      dlFiles$url <- sprintf(
        "https://www.isrctn.com/editorial/retrieveFile/%s/%s",
        dlFiles$fileref1, dlFiles$fileref2)

      # finally download

      # apply regexp
      if (is.null(documents.regexp)) {

        message("- Creating empty document placeholders (max. ", nrow(dlFiles), ")")

        # create empty files
        tmp <-
          sapply(
            dlFiles$filepathname,
            function(i) if (!file.exists(i))
              file.create(i, showWarnings = TRUE),
            USE.NAMES = FALSE)

        tmp <- sum(unlist(tmp), na.rm = TRUE)

      } else {

        # inform
        message("- Applying 'documents.regexp' to ",
                nrow(dlFiles), " documents")

        dlFiles <- dlFiles[
          grepl(documents.regexp, dlFiles$filename, ignore.case = TRUE), ,
          drop = FALSE]

        # inform
        message("- Downloading ",
                nrow(dlFiles[!dlFiles$fileexists, , drop = FALSE]),
                " missing documents")

        # specific: if no fileref2, the url is incomplete, thus remove
        dlFiles <- dlFiles[!is.na(dlFiles$fileref2), , drop = FALSE]

        # do download, adding a configuration which if absent has
        # the error: Unrecognized content encoding type
        httr::with_config(
          config = httr::config("http_content_decoding" = 0), {
            tmp <- ctrMultiDownload(
              urls = dlFiles$url[!dlFiles$fileexists],
              destfiles = dlFiles$filepathname[!dlFiles$fileexists],
              verbose = verbose)},
          override = FALSE)

        if (!nrow(tmp)) tmp <- 0L else {

          # handle failures despite success is true
          suppressMessages(invisible(sapply(
            tmp[tmp$status_code != 200L, "destfile", drop = TRUE], unlink
          )))
          tmp <- nrow(tmp[tmp$status_code == 200L, , drop = FALSE])

        }
      }

      # inform user
      message(sprintf(paste0(
        "= Newly saved %i ",
        ifelse(is.null(documents.regexp), "placeholder ", ""),
        "document(s) for %i trial(s); ",
        "%i document(s) for %i trial(s) already existed in %s"),
        tmp,
        length(unique(dlFiles$`_id`)),
        sum(dlFiles$fileexists),
        length(unique(dlFiles$`_id`[dlFiles$fileexists])),
        documents.path
      ))

    } # directory created

  } # if download files

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbIsrctn
