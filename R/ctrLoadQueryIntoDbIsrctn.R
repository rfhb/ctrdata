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
#' @importFrom utils URLdecode URLencode
#' @importFrom httr2 req_perform req_user_agent request
#' @importFrom V8 JS
#' @importFrom rlang hash
#'
ctrLoadQueryIntoDbIsrctn <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    ctgov2history,
    ctishistory,
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
  # - remove naked q elsewhere
  apiterm <- sub("(^|&)q=($|&)", "", apiterm)
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
  # - cleanup
  apiterm <- apiterm[apiterm != ""]
  apiterm <- sub("^q=", "", apiterm)
  # - check if fields are repeated, which would
  #   indicate they are alternatives, boolean or
  field <- sub("^(.+):.+$", "\\1", apiterm)
  field <- ifelse(duplicated(field), " OR ", ") AND (")
  numF <- length(field)
  if (numF > 1L) {
    apiterm[-c(1, numF)] <- paste0(field[-c(1, numF)], apiterm[-c(1, numF)])
    apiterm[1] <- paste0("(", apiterm[1])
    apiterm[numF] <- paste0(field[numF], apiterm[numF], ")")
  }
  # - concat
  apiterm <- trimws(paste0(trimws(apiterm), collapse = " "))
  # - prefix with q removed above
  apiterm <- paste0("q=", apiterm)
  # - inform user
  if (verbose) message("DEBUG: apiterm is ", apiterm)

  ## checks -------------------------------------------------------------------

  message("* Checking trials in ISRCTN...")

  # - check number of trials to be downloaded
  isrctnfirstpageurl <- utils::URLencode(paste0(
    queryIsrctnRoot, queryIsrctnType2, apiterm, queryupdateterm
  ))
  #
  # TODO
  # tmp <- try(
  #   suppressWarnings(
  #     xml2::read_xml(
  #       x = url(utils::URLencode(isrctnfirstpageurl))
  #     )
  #   ),
  #   silent = TRUE
  # )
  # #
  # if (inherits(tmp, "try-error")) {
  #   stop("Host ", queryIsrctnRoot, " not working as expected, ",
  #        "cannot continue: ", tmp[[1]],
  #        call. = FALSE
  #   )
  # }
  #
  # tmp <- try(xml2::xml_attr(tmp, "totalCount"), silent = TRUE)
  #

  tmp <- try(sub(
    '.*totalCount=\"([0-9]+)\" .*', "\\1",
    rawToChar(
      httr2::req_perform(
        httr2::req_user_agent(
          httr2::request(
            isrctnfirstpageurl),
          ctrdataUseragent
        ))$body)), silent = TRUE)

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
    signif(tmp * 0.018, 1L), " MB)"
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

  # inform user
  message("(1/3) Downloading trial file... ")

  # construct API call setting limit to number found above
  isrctndownloadurl <- paste0(
    queryIsrctnRoot, queryIsrctnType1, tmp, "&", apiterm, queryupdateterm
  )

  # prepare a file handle for temporary directory
  f <- file.path(
    tempDir, paste0(
      "isrctn_",
      # include query in file name for potential re-download
      sapply(isrctndownloadurl, rlang::hash),
      ".xml"))

  # get (download) trials into single file f
  ctrMultiDownload(
    urls = isrctndownloadurl,
    destfiles = f,
    verbose = verbose
  )

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    message(
      "No studies downloaded. Please check 'queryterm' ",
      " or run again with verbose = TRUE"
    )
  }

  ## convert to json ------------------------------------------------

  if (length(.ctrdataenv$ct) == 0L) initTranformers()

  # run conversion
  importDateTime <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message("(2/3) Converting to NDJSON (estimate: ",
          signif(tmp * 1.7 / 290, 1L), " s)...")

  jqr::jq(
    # input
    textConnection(
      .ctrdataenv$ct$call(
        "parsexml",
        # read source xml file
        paste0(readLines(f, warn = FALSE), collapse = ""),
        # important parameters
        V8::JS("{trim: true, ignoreAttrs: true, explicitArray: false}"))
    ),
    # processing
    paste0(
      # extract trial record(s)
      ' .allTrials.fullTrial | (if type != "array" then .trial else .[].trial end) ',
      # add elements
      '| .["_id"] = .isrctn
       | .["ctrname"] = "ISRCTN"
       | .["record_last_import"] = "', importDateTime, '"'
    ),
    flags = jqr::jq_flags(pretty = FALSE),
    out = file.path(tempDir, "isrctn_trials_.ndjson")
  )

  ## import json -----------------------------------------------------

  ## run import
  message("(3/3) Importing records into database...")
  if (verbose) message("DEBUG: ", tempDir)

  # do import
  imported <- dbCTRLoadJSONFiles(
    dir = tempDir,
    con = con,
    verbose = verbose
  )

  ## documents -----------------------------------------------------

  if (!is.null(documents.path)) {

    # user info
    message(
      "* Checking for documents...\n",
      "- Getting links to documents")

    # temporary file for trial ids and file names
    downloadsNdjson <- file.path(tempDir, "isrctn_downloads.ndjson")
    unlink(downloadsNdjson)
    downloadsNdjsonCon <- file(downloadsNdjson, open = "at")
    on.exit(try(close(downloadsNdjsonCon), silent = TRUE), add = TRUE)
    on.exit(unlink(downloadsNdjson), add = TRUE)

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
    message("\r", appendLF = FALSE)

    # get document trial id and file name
    dlFiles <- jsonlite::stream_in(
      file(downloadsNdjson), pagesize = 5000L, verbose = FALSE)

    # check if any documents
    if (!nrow(dlFiles)) {

      message("= No documents identified for downloading.")

    } else {

      # calculate urls
      dlFiles$url <- sprintf(
        "https://www.isrctn.com/editorial/retrieveFile/%s/%s",
        dlFiles$fileref1, dlFiles$fileref2)

      # TODO
      # do download with special config to avoid error
      # "Unrecognized content encoding type.
      #  libcurl understands deflate, gzip content encodings."
      # httr::with_config(
      #   config = httr::config("http_content_decoding" = 0), {
      ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path,
        documents.regexp,
        verbose = verbose)
      # }, override = FALSE)

    } # if (!nrow(dlFiles))

  } # !is.null(documents.path)

  ## delete for any re-downloads
  unlink(dir(
    path = tempDir, pattern = "isrctn_trials_.*.ndjson",
    full.names = TRUE))

  ## inform user -----------------------------------------------------

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbIsrctn
