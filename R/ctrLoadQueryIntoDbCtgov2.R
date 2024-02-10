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
  queryterm <- strsplit(queryterm, split = "&")[[1]]
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
        "resFirstPost=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "primComp=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "studyComp=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "lastUpdPost=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "firstPost=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "start=([0-9-]+)_?([0-9-]*)(&.+|$)",
        "ageRange=([0-9a-z]+)_?([0-9a-z]*)(&.+|$)"
      ),
      "replace" = list(
        "AREA[ResultsFirstPostDate]RANGE[\\1,\\2]",
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
        "country=(.+)(&|$)",
        "locStr=(.+)(&.+|$)",
        "locn=(.+)(&|$)"
      ),
      "replace" = list(
        "AREA[LocationCountry]\\1",
        "AREA[LocationCity]\\1",
        "AREA[LocationFacility]\\1"
      ),
      "collapse" = ",",
      "out" = character()
    ),
    #
    # hand through aggFilters
    list(
      "extract" = "(aggFilters=.+)(&|$)",
      "replace" = "&\\1",
      "collapse" = "",
      "out" = character()
    ),
    #
    # other "query." terms
    list(
      "extract" = "(cond|term|intr|titles|outc|spons|lead|id)=(.+)(&|$)",
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
  importDateTime <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")

  message("(1/3) Downloading in ",
          ceiling(resultsEuNumTrials / 1000L),
          " batch(es) (max. 1000 trials each; estimate: ",
          format(resultsEuNumTrials * 0.1, digits = 2), " MB total)")

  while (TRUE) {

    # page url
    urlToDownload <- ifelse(
      pageNextToken != "",
      paste0(url, "&pageToken=", pageNextToken),
      url)

    # for download
    fTrialJson <- file.path(
      tempDir, paste0(
        "ctgov_trials_",
        # include query in file name for potential re-download
        sapply(url, digest::digest, algo = "crc32"),
        "_", pageNumber, ".json"))

    # do download
    tmp <- ctrMultiDownload(
      urlToDownload,
      fTrialJson,
      progress = TRUE,
      verbose = verbose)

    # inform user
    if (tmp[1, "status_code", drop = TRUE] != 200L) message(
      "Download not successful for ", urlToDownload)

    # convert to ndjson
    message("(2/3) Converting to NDJSON...")
    fTrialsNdjson <- file.path(tempDir, paste0("ctgov_trials_", pageNumber,".ndjson"))
    jqr::jq(
      file(fTrialJson),
      paste0(
        # extract trial records. studies seems always to be an array,
        # even for a single trial, thus no handling needed if array or not
        ' .studies | .[] ',
        # add elements
        '| .["_id"] = .protocolSection.identificationModule.nctId
         | .["ctrname"] = "CTGOV2"
         | .["record_last_import"] = "', importDateTime, '"'
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

  message("(3/3) Importing records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)
  message("")

  ## download files-----------------------------------------------------

  if (!is.null(documents.path)) {

    # temporary file for trial ids and file names
    downloadsNdjson <- file.path(tempDir, "ctgov2_downloads.ndjson")
    suppressMessages(unlink(downloadsNdjson))
    downloadsNdjsonCon <- file(downloadsNdjson, open = "at")
    on.exit(try(close(downloadsNdjsonCon), silent = TRUE), add = TRUE)
    on.exit(try(unlink(downloadsNdjson), silent = TRUE), add = TRUE)

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

    # check if any documents
    if (!nrow(dlFiles)) {
      message("= No documents identified for downloading.")
    } else {

      # calculate urls
      dlFiles$url <- sprintf(
        ctgovEndpoints[3],
        sub(".*([0-9]{2})$", "\\1", dlFiles$`_id`),
        dlFiles$`_id`,
        dlFiles$filename)

      # do download
      resFiles <- ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path, documents.regexp, verbose)

    } # if (!nrow(dlFiles))

  } # !is.null(documents.path)

  ## delete for any re-downloads
  try(unlink(dir(
    path = tempDir, pattern = "ctgov_trials_[0-9]+.ndjson",
    full.names = TRUE)), silent = TRUE)

  ## inform user -----------------------------------------------------

  # find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)

}
# end ctrLoadQueryIntoDbCtogv2023
