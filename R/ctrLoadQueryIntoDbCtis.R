### ctrdata package

#' ctrLoadQueryIntoDbCtis
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jqr jq jqr jq_flags
#' @importFrom tools toTitleCase
#' @importFrom nodbi docdb_update
#' @importFrom jsonlite stream_in fromJSON
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @importFrom httr2 req_perform req_body_json request req_user_agent
#'
ctrLoadQueryIntoDbCtis <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    euctrprotocolsall,
    ctgov2history,
    ctishistory,
    documents.path,
    documents.regexp,
    annotation.text,
    annotation.mode,
    only.count,
    con, verbose,
    queryupdateterm) {

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  ## ctis api -----------------------------------------------------------

  # https://euclinicaltrials.eu/ctis-public/assets/i18n/en.json

  ctisEndpoints <- paste0(
    #
    # root
    "https://euclinicaltrials.eu/ctis-public-api",
    #
    # endpoints
    c(
      # - 1 trial overview - post method
      "/search",
      #
      # - 2 trial information, partI and partsII
      "/retrieve/%s", # %s is ctNumber
      #
      # - 3 download files
      "/documents/%s/%s/download" # %s are trial _id, document[].uuid
      #
    )
  )

  ## api_1: overviews ---------------------------------------------------------

  # for importing overview (recruitment, status etc.) into database
  message("* Checking trials in CTIS...", appendLF = FALSE)

  # queryterm comes from ctrGetQueryUrl()

  # 2024-06-17 defined by ctrdata Tampermonkey script:
  # https://euclinicaltrials.eu/ctis-public/search#searchCriteria=
  # {"containAll":"infection","containAny":"neonates"}

  initialData <- try(rawToChar(
    httr2::req_perform(
      httr2::req_body_json(
        httr2::req_user_agent(
          httr2::request(
            ctisEndpoints[1]),
          ctrdataUseragent),
        data = jsonlite::fromJSON(
          paste0(
            # add pagination parameters
            '{"pagination":{"page":1,"size":1},',
            # add search criteria
            sub("searchCriteria=", '"searchCriteria":',
                # handle empty search query terms
                ifelse(queryterm != "", queryterm, 'searchCriteria={}')),
            # remaining parameters needed for proper server response
            ',"sort":{"property":"decisionDate","direction":"DESC"}}'
          ), simplifyVector = FALSE)
      ))$body), silent = TRUE)

  # early exit
  if (inherits(initialData, "try-error")) {
    warning("Could not be retrieved, check 'queryterm' and / or 'register'. ",
            "\nAPI returned: ", initialData$error[1], call. = FALSE
    )
    message("API parameters from URL: ", queryterm)
    return(emptyReturn)
  }

  # get overview
  overview <- jsonlite::fromJSON(
    jqr::jq(initialData, " .pagination "))

  # early exit
  if (overview$totalRecords == 0L) {
    message("No trials found? Check 'queryterm' and / or 'register'.")
    return(emptyReturn)
  }

  # inform user
  message("\b\b\b, found ", overview$totalRecords, " trials ")

  # only count?
  if (only.count) {
    message(
      "= Not done (only.count = TRUE): Imported ",
      overview$totalRecords, " trial(s)")
    # return
    return(list(
      n = overview$totalRecords,
      success = NULL,
      failed = NULL
    ))
  }

  # prepare to retrieve overviews
  importDateTime <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  nRecords <- 100L
  pageNo <- seq_len(overview$totalPages %/% nRecords + 1L)

  fTrialsJsonApi1PageFiles <- file.path(
    tempDir, paste0("ctis_api1_page_", pageNo, ".json"))

  on.exit(unlink(dir(tempDir, "ctis_api1_page_.*.json", full.names = TRUE)), add = TRUE)

  jsonApi1Pages <- paste0(
    # add pagination parameters
    paste0(
      '{"pagination":{"page":', pageNo, ',"size":', nRecords, "},"),
    # add search criteria
    sub(
      "searchCriteria=", '"searchCriteria":',
      # handle empty search query terms
      ifelse(
        queryterm != "", queryterm,
        'searchCriteria={}'),
    ),
    # remaining parameters needed for proper server response
    ',"sort":{"property":"decisionDate","direction":"DESC"}}'
  )

  # download files
  ctrMultiDownload(
    urls = rep.int(ctisEndpoints[1], length(pageNo)),
    destfiles = fTrialsJsonApi1PageFiles,
    data = jsonApi1Pages,
    verbose = verbose
  )

  # prepare files
  fTrialsNdjsonApi1 <- file.path(tempDir, "ctis_add_api1.ndjson")
  unlink(fTrialsNdjsonApi1)
  fTrialsNdjsonApi1Con <- file(fTrialsNdjsonApi1, open = "at")
  on.exit(try(close(fTrialsNdjsonApi1Con), silent = TRUE), add = TRUE)
  on.exit(try(unlink(fTrialsNdjsonApi1), silent = TRUE), add = TRUE)

  # iterate over json files and create ndjson
  sapply(
    fTrialsJsonApi1PageFiles,
    # {...,"data":[{"ctNumber":"2023-510173-34-00","ctStatus"
    function(i) writeLines(
      jqr::jq(
        file(i), paste0(
          # extract trial records
          " .data | .[] ",
          # add canonical elements
          '| .["_id"] = .ctNumber ',
          '| .["ctrname"] = "CTIS" ',
          '| .["record_last_import"] = "', importDateTime, '" ',
          # keep only standardised fields
          "| del(.id, .ctNumber, .product, .endPoint, .eudraCtInfo,
                 .ctTitle, .primaryEndPoint, .sponsor, .conditions) "
        )),
      con =  fTrialsNdjsonApi1Con),
    USE.NAMES = FALSE)

  # cleanup and close
  unlink(dir(tempDir, "ctis_api1_page_.*.json", full.names = TRUE))
  close(fTrialsNdjsonApi1Con)

  # user info
  message("\r", appendLF = FALSE)

  # get ids
  idsTrials <- gsub(
    '"', "", as.character(
      jqr::jq(file(fTrialsNdjsonApi1), ' ."_id" ')))

  ## api_2: per trial partI, partsII ------------------------------------------

  # this is imported as the main data into the database

  message("- Downloading and processing trial data... (",
          "estimate: ", signif(length(idsTrials) * 12 / 96, 1L), " Mb)")

  urls <- sprintf(ctisEndpoints[2], idsTrials)

  fPartIPartsIIJson <- function(i) {
    file.path(tempDir, paste0("ctis_trial_partIpartsII_", i, ".json"))
  }

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  resDf <- ctrMultiDownload(
    urls = urls,
    destfiles = fPartIPartsIIJson(idsTrials),
    verbose = verbose
  )

  # convert partI and partsII details into ndjson file(s),
  # each approximately 10MB for nRecords = 100L
  nRecords <- 100L
  groupNo <- (nrow(resDf) %/% nRecords) + 1L
  groupNo <- rep(seq_len(groupNo), nRecords)
  on.exit(unlink(dir(tempDir, "ctis_trials_api2_.*.ndjson", full.names = TRUE)), add = TRUE)

  # mangle and save as ndjson
  for (g in unique(groupNo)) {

    fTrialsNdjsonApi2 <- file.path(tempDir, sprintf("ctis_trials_api2_%i.ndjson", g))
    fTrialsNdjsonApi2Con <- file(fTrialsNdjsonApi2, open = "at")
    on.exit(try(close(fTrialsNdjsonApi2Con), silent = TRUE), add = TRUE)

    sapply(
      X = na.omit(resDf$destfile[groupNo == g]),
      FUN = function(f) {

        if (!file.exists(f)) return()

        writeLines(
          stringi::stri_replace_all_regex(
            readLines(f, warn = FALSE),
            '^\\{"ctNumber": *"([-0-9]+)",',
            '{"_id": "$1", "ctNumber": "$1",'),
          con = fTrialsNdjsonApi2Con)

      })

    close(fTrialsNdjsonApi2Con)
    message(g * nRecords, "...\r", appendLF = FALSE)

  }

  ## database import -----------------------------------------------------

  message("- Importing records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)

  # additional ndjson file
  message("- Updating with additional data: ", appendLF = FALSE)

  message(". ", appendLF = FALSE)
  updated <- nodbi::docdb_update(
    src = con, key = con$collection, query = "{}",
    value = fTrialsNdjsonApi1)

  message(" ")

  ## api_3: documents -------------------------------------------------------

  if (!is.null(documents.path)) {

    # 1 - get ids of lists (which include urls to download)
    message("* Checking for documents ", appendLF = FALSE)

    # 2 - create data frame with info on documents (url, name, extension etc.)

    # get temporary file
    downloadsNdjson <- file.path(tempDir, "ctis_downloads.ndjson")
    unlink(downloadsNdjson)
    downloadsNdjsonCon <- file(downloadsNdjson, open = "at")
    on.exit(try(close(downloadsNdjsonCon), silent = TRUE), add = TRUE)
    on.exit(try(unlink(downloadsNdjson), silent = TRUE), add = TRUE)

    # iterate to get docs information
    for (f in dir(tempDir, "ctis_trials_api2_[0-9]+.ndjson", full.names = TRUE)) {

      message(". ", appendLF = FALSE)

      writeLines(
        jqr::jqr(
          file(f),
          " ._id as $_id | .documents[] | { $_id,
            title, uuid, documentType, documentTypeLabel,
            fileType, associatedEntityId } ",
          flags = jqr::jq_flags(pretty = FALSE)
        ),
        con = downloadsNdjsonCon)

      unlink(f)
    }
    message(" ")
    close(downloadsNdjsonCon)

    # 3 - documents download
    dlFiles <- jsonlite::stream_in(
      file(downloadsNdjson), pagesize = 5000L, verbose = FALSE)
    unlink(downloadsNdjson)

    # check if any documents
    if (!nrow(dlFiles)) {

      message("= No documents identified for downloading.")

    } else {

      # remove duplicate files based on their title

      # - robustly sanitise file name
      dlFiles$title <- gsub("[^[:alnum:] ._-]", "",  dlFiles$title)

      # - normalise punctuation found with various file titles
      dlFiles$title <- gsub("_|, |: ", " ", dlFiles$title)
      dlFiles$title <- gsub("  +", " ", dlFiles$title)
      dlFiles$title <- trimws(dlFiles$title)

      # calculate prefix for document type
      dlFiles$prefix <- paste0(
        # - type of document
        abbreviate(
          tools::toTitleCase(
            stringi::stri_replace_all_fixed(
              dlFiles$documentTypeLabel,
              c(" - Final", ": ", " (SmPC)", "(EU) ",
                " (for publication)", ":", "\"", "/", "."),
              "", vectorize_all = FALSE)),
          minlength = 12L, named = FALSE))

      # add destination file name
      dlFiles$filename <- paste0(
        dlFiles$prefix, " - ",
        dlFiles$title, " - ",
        dlFiles$associatedEntityId,
        ".", dlFiles$fileType)

      # calculate url
      dlFiles$url <- sprintf(
        ctisEndpoints[3], dlFiles$`_id`, dlFiles$uuid)

      # do download
      ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path,
        documents.regexp,
        multiplex = FALSE,
        verbose = verbose)

    } # if (!nrow(dlFiles))

  } # !is.null(documents.path)

  ## inform user -----------------------------------------------------

  #  find out number of trials imported into database
  message("= Imported ", imported$n,
          ", updated ", updated,
          " record(s) on ", length(idsTrials), " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtis
