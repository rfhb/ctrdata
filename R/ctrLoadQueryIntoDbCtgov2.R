### ctrdata package

#' ctrLoadQueryIntoDbCtgov2
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jqr jq jq_flags
#' @importFrom utils URLencode
#' @importFrom jsonlite stream_in
#' @importFrom httr GET status_code content
#' @importFrom stringi stri_replace_all_regex
#'
ctrLoadQueryIntoDbCtgov2 <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    documents.path,
    documents.regexp,
    annotation.text,
    annotation.mode,
    only.count,
    con, verbose,
    queryupdateterm) {

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  ## ctgov api ---------------------------------------------------------

  ctgovEndpoints <- c(
    # pageSize 0 delivers default 10
    "https://www.clinicaltrials.gov/api/v2/studies?format=json&countTotal=true&pageSize=1&%s",
    "https://www.clinicaltrials.gov/api/v2/studies?format=json&countTotal=true&pageSize=1000&%s",
    "https://storage.googleapis.com/ctgov2-large-docs/%s/%s/%s"
  )

  ## process parameters ------------------------------------------------

  # append if to update
  queryterm <- paste0(queryterm, "&", queryupdateterm)
  queryterm <- gsub("&$", "", queryterm)
  queryterm <- gsub("%20", " ", queryterm) # for URLencode

  # translation to ClinicalTrials.gov REST API 2.0.0-draft
  # https://clinicaltrials.gov/data-about-studies/learn-about-api

  # distance=50 seems to be added in webinterface
  # even though not requested by user, removing it
  queryterm <- sub("([&]?)distance=50(&|$)", "&", queryterm)

  # slice by "&"
  queryterm <- sort(strsplit(queryterm, split = "&")[[1]])
  queryterm <- queryterm[!grepl("^https://", queryterm)]
  queryterm <- queryterm[queryterm != ""]

  # url to api
  apiParams <- list(
    #
    "filter.geo" = list(
      "extract" = "distance=(.+?)(&|$)",
      "replace" = "filter.geo=distance(\\1)",
      "collapse" = "",
      "out" = character()
    ),
    #
    "filter.advanced" = list(
      "extract" = list(
        "primComp=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "studyComp=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "lastUpdPost=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "firstPost==([0-9-]+)_?([0-9-]*)(&.+|$)",
        "start==([0-9-]+)_?([0-9-]*)(&.+|$)",
        "ageRange=([0-9a-z]+)_?([0-9a-z]*)(&.+|$)"
      ),
      "replace" = list(
        "AREA[PrimaryCompletionDate]RANGE[\\1,\\2]",
        "AREA[CompletionDate]RANGE[\\1,\\2]",
        "AREA[LastUpdatePostDate]RANGE[\\1,\\2]",
        "AREA[StudyFirstPostDate]RANGE[\\1,\\2]",
        "AREA[StartDate]RANGE[\\1,\\2]",
        "AREA[MinimumAge]RANGE[\\1, MAX] AND AREA[MaximumAge]RANGE[MIN, \\2]"
      ),
      "collapse" = " AND ",
      "out" = character()
    ),
    #
    "query.locn" = list(
      "extract" = list(
        "locn=(.+)(&|$)",
        "locStr=(.+)(&.+|$)"
      ),
      "replace" = list(
        "LocationFacility:\\1",
        "LocationCity:\\1"
      ),
      "collapse" = ",",
      "out" = character()
    ),
    #
    list(
      "extract" = "(aggFilters=.+)(&|$)",
      "replace" = "&\\1",
      "collapse" = "",
      "out" = character()
    ),
    #
    # other "query." terms
    list(
      "extract" = "(cond|term|intr|title|outc|sponsor|lead|id)=(.+)(&|$)",
      "replace" = "&query.\\1=\\2",
      "collapse" = "",
      "out" = character()
    )
  )

  # iterate over API terms
  for (t in seq_along(queryterm)) {
    for (a in seq_along(apiParams)) {
      for (i in seq_along(apiParams[[a]][["extract"]])) {
        if (grepl(apiParams[[a]][["extract"]][[i]], queryterm[t])) {
          item <-
            sub(apiParams[[a]][["extract"]][[i]],
                apiParams[[a]][["replace"]][[i]],
                queryterm[t]
            )
          apiParams[[a]][["out"]] <-
            paste0(
              c(apiParams[[a]][["out"]], item),
              collapse = apiParams[[a]][["collapse"]]
            )
        } # if extract
      } # extract
    } # apiParams
  } # queryterm

  # concatenate
  queryterm <- sapply(apiParams, "[[", "out")
  queryterm <- queryterm[seq_along(queryterm)[sapply(queryterm, length) > 0L]]
  for (i in seq_along(queryterm)) { # i = 4
    nm <- names(queryterm)[i]
    if (nchar(nm)) queryterm[i] <- paste0(nm, "=", queryterm[i])
  }
  queryterm <- paste0(queryterm, collapse = "&")

  # adjust remaining quirks
  queryterm <- gsub("&&+", "&", queryterm)
  queryterm <- gsub("RANGE\\[,", "RANGE[MIN,", queryterm)
  queryterm <- stringi::stri_replace_all_regex(queryterm, "(RANGE\\[.+?),\\]", "$1,MAX]")

  ## process query -----------------------------------------------------

  # corresponds to count
  url <- sprintf(ctgovEndpoints[1], queryterm)
  if (verbose) message("API call: ", url)
  message("* Checking trials using CTGOV API 2.0.0.-test...", appendLF = FALSE)
  url <- utils::URLencode(url)
  counts <- httr::GET(url)

  # early exit
  if (httr::status_code(counts) != 200L) {
    warning("Could not be retrieved, check 'queryterm' and / or 'register'. ",
            "\nAPI returned: ", httr::content(counts), call. = FALSE)
    message("API call: ", url)
    return(emptyReturn)
  }

  # extract total number of trial records
  counts <- suppressMessages(httr::content(counts, as = "text"))
  resultsEuNumTrials <- as.numeric(jqr::jq(counts, '.totalCount'))
  message("\b\b\b, found ", resultsEuNumTrials, " trials")

  # early exit
  if (!resultsEuNumTrials) {
    warning("No trials found, check 'queryterm' and 'register'")
    return(emptyReturn)
  }

  # only count?
  if (only.count) {

    # return
    return(list(n = resultsEuNumTrials,
                success = NULL,
                failed = NULL))
  }

  ## download json -----------------------------------------------------

  # corresponds to trials

  url <- sprintf(ctgovEndpoints[2], queryterm)
  url <- utils::URLencode(url)

  pageNextToken <- ""
  pageNumber <- 1L

  message("(1/2) Downloading and converting in ",
          ceiling(resultsEuNumTrials / 1000L),
          " batch(es) (max. 1000 trials each; estimate: ",
          format(resultsEuNumTrials * 0.1, digits = 2), " MB total)")

  while (TRUE) {

    # for download
    fTrialJson <- file.path(tempDir, paste0("ctgov_trials_", pageNumber,".json"))

    # page url
    urlToDownload <- ifelse(pageNextToken != "",
                            paste0(url, "&pageToken=", pageNextToken), url)

    # do download
    tmp <- ctrMultiDownload(urlToDownload, fTrialJson, progress = TRUE)

    # inform user
    if (tmp[1, "status_code", drop = TRUE] != 200L) message(
      "Download not successful for ", urlToDownload)

    # convert to ndjson
    message("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\bconverting to NDJSON...")
    fTrialsNdjson <- file.path(tempDir, paste0("ctgov_trials_", pageNumber,".ndjson"))
    jqr::jq(
      file(fTrialJson),
      paste0(
        # extract trial records
        ' .studies | .[] ',
        # add elements
        '| .["_id"] = .protocolSection.identificationModule.nctId
         | .["ctrname"] = "CTGOV2"
         | .["record_last_import"] = "', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '"'
      ),
      flags = jqr::jq_flags(pretty = FALSE),
      out = fTrialsNdjson
    )

    # continue or exit
    pageNumber <- pageNumber + 1L
    # "nextPageToken":"NF0g5JGBlPMuwQY"} at end of json
    fTrialJsonCon <- file(fTrialJson, open = "rb")
    seek(fTrialJsonCon, where = file.size(fTrialJson) - 40L)
    pageNextTokenTest <- readChar(fTrialJsonCon, 1000L)
    close(fTrialJsonCon)
    pageNextToken <- sub('.*"nextPageToken":"(.+?)".*', "\\1", pageNextTokenTest)
    if (pageNextToken == pageNextTokenTest) break

  }

  ## database import -----------------------------------------------------

  message("(2/2) Importing JSON records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)
  message("")

  ## download files-----------------------------------------------------

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
      downloadsNdjson <- file.path(tempDir, "ctis_downloads.ndjson")
      suppressMessages(unlink(downloadsNdjson))
      downloadsNdjsonCon <- file(downloadsNdjson, open = "at")

      # extract trial ids and file name and save in temporary file
      for (ndjsonFile in dir(
        path = tempDir, pattern = "^.+_trials_.*.ndjson$", full.names = TRUE)) {
        jqr::jq(
          file(ndjsonFile),
          ' { _id: ._id,
          filename: .documentSection.largeDocumentModule.largeDocs[].filename }',
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
        ctgovEndpoints[3], sub(".*([0-9]{2})$", "\\1", dlFiles$`_id`),
        dlFiles$`_id`, dlFiles$filename)

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

        message("- Applying 'documents.regexp' to ",
                nrow(dlFiles), " documents")

        dlFiles <- dlFiles[
          grepl(documents.regexp, dlFiles$filename, ignore.case = TRUE), ,
          drop = FALSE]

        # do download
        message("- Downloading ",
                nrow(dlFiles[!dlFiles$fileexists, , drop = FALSE]),
                " missing documents")

        # do download
        tmp <- ctrMultiDownload(
          urls = dlFiles$url[!dlFiles$fileexists],
          destfiles = dlFiles$filepathname[!dlFiles$fileexists])

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

  ## inform user -----------------------------------------------------

  message("= Imported / updated ",
          paste0(c(imported$n), collapse = " / "),
          " records on ", resultsEuNumTrials, " trial(s)")

  # return
  return(imported)

}
# end ctrLoadQueryIntoDbCtogv2023
