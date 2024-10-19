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
#' @importFrom httr GET status_code content
#' @importFrom digest digest
#'
ctrLoadQueryIntoDbCtis <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    ctgov2history,
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
  message("* Checking trials in CTIS...")

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  message("(1/4) Downloading trial list(s)...", appendLF = FALSE)

  # queryterm comes from ctrGetQueryUrl()

  # 2024-06-17 defined by ctrdata Tampermonkey script:
  # https://euclinicaltrials.eu/ctis-public/search#searchCriteria=
  # {"containAll":"infection","containAny":"neonates","containNot":""}

  # get page data
  initialData <- try(rawToChar(
    curl::curl_fetch_memory(
      url = ctisEndpoints[1],
      handle = curl::new_handle(
        postfields = paste0(
          # add pagination parameters
          '{"pagination":{"page":1,"size":1},',
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
        ) # paste
      ) # curl
    )$content), silent = TRUE)

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
  message("\b\b\b, found ", overview$totalRecords, " trials ", appendLF = FALSE)

  # only count?
  if (only.count) {
    message()
    # return
    return(list(
      n = overview$totalRecords,
      success = NULL,
      failed = NULL
    ))
  }

  # prepare to retrieve overviews
  importDateTime <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  fTrialsNdjson <- file.path(tempDir, "ctis_add_api1.ndjson")
  unlink(fTrialsNdjson)
  on.exit(unlink(fTrialsNdjson), add = TRUE)

  # parallel running helper functions
  failure <- function(str) message(paste("Failed request:", str))
  pool <- curl::new_pool()
  nRecords <- 100L

  # main function for handling results
  success <- function(x) {

    if (is.list(x)) x <- rawToChar(x$content)

    # {...,"data":[{"ctNumber":"2023-510173-34-00","ctStatus"
    cat(
      jqr::jq(
        x, paste0(
          # extract trial records
          " .data | .[] ",
          # add canonical elements
          '| .["_id"] = .ctNumber ',
          '| .["ctrname"] = "CTIS" ',
          '| .["record_last_import"] = "', importDateTime, '" ',
          # keep only standardised fields
          "| del(.id, .ctNumber, .product, .endPoint, .eudraCtInfo, .ctTitle,
               .primaryEndPoint, .sponsor, .conditions) "
        )
      ),
      file =  fTrialsNdjson,
      append = TRUE,
      sep = "\n")

    message(". ", appendLF = FALSE)

  }

  # create POST requests
  sapply(seq_len(overview$totalPages %/% nRecords + 1L), function(i) {
    curl::multi_add(
      curl::new_handle(
        url = ctisEndpoints[1],
        postfields = paste0(
          # add pagination parameters
          paste0(
            '{"pagination":{"page":', i, ',"size":', nRecords, "},"),
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
        ) # paste
      ), # handle
      done = success,
      fail = failure,
      data = NULL,
      pool = pool
    )
  })

  # important on 2024-06-29 disable HTTP/2 multiplexing
  # as it leads to data loss with ctis servers
  curl::multi_set(multiplex = FALSE, pool = pool)

  # run in parallel
  curl::multi_run(pool = pool)

  # user info
  message("\r", appendLF = FALSE)

  # get ids
  idsTrials <- gsub(
    '"', "", as.character(
      jqr::jq(file(fTrialsNdjson), ' ."_id" ')))

  ## api_2: per trial partI, partsII ------------------------------------------

  # this is imported as the main data into the database

  message("(2/4) Downloading and processing trial data... (",
          "estimate: ", signif(length(idsTrials) * 405 / 5505, 1L), " Mb)")

  urls <- sprintf(ctisEndpoints[2], idsTrials)

  fPartIPartsIIJson <- function(i) {
    file.path(tempDir, paste0("ctis_trial_partIpartsII_", i, ".json"))
  }

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  tmp <- ctrMultiDownload(
    urls, fPartIPartsIIJson(idsTrials),
    multiplex = FALSE, verbose = verbose)

  # convert partI and partsII details into ndjson file(s),
  # each approximately 10MB for nRecords = 100L
  nRecords <- 100L
  groupNo <- (nrow(tmp) %/% nRecords) + 1L
  groupNo <- rep(seq_len(groupNo), nRecords)
  on.exit(unlink(dir(tempDir, "ctis_trials_api2_.*.ndjson", full.names = TRUE)), add = TRUE)

  # mangle and save as ndjson
  for (g in unique(groupNo)) {

    sapply(
      na.omit(tmp[["destfile"]][groupNo == g]), function(f) {

        if (!file.exists(f)) return()

        cat(
          stringi::stri_replace_all_regex(
            readLines(f, warn = FALSE),
            '^\\{"ctNumber": *"([-0-9]+)",',
            '{"_id": "$1", "ctNumber": "$1",'),
          file = file.path(
            tempDir,
            sprintf("ctis_trials_api2_%i.ndjson", g)),
          sep = "\n",
          append = TRUE)

      })

    message(g * nRecords, "...\r", appendLF = FALSE)

  }

  message("\r", appendLF = FALSE)

  ## database import -----------------------------------------------------

  message("(3/4) Importing records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)

  # additional ndjson file
  message("(4/4) Updating with additional data: ", appendLF = FALSE)

  message(". ", appendLF = FALSE)
  updated <- nodbi::docdb_update(
    src = con, key = con$collection, query = "{}",
    value = fTrialsNdjson)

  message("")

  ## api_3: documents -------------------------------------------------------

  if (!is.null(documents.path)) {

    # 1 - get ids of lists (which include urls to download)
    message("* Checking for documents: ", appendLF = FALSE)

    # 2 - create data frame with info on documents (url, name, extension etc.)

    # get temporary file
    downloadsNdjson <- file.path(tempDir, "ctis_downloads.ndjson")
    unlink(downloadsNdjson)
    on.exit(unlink(downloadsNdjson), add = TRUE)

    # iterate to get docs information
    for (f in dir(tempDir, "ctis_trials_api2_[0-9]+.ndjson", full.names = TRUE)) {

      message(". ", appendLF = FALSE)

      cat(
        jqr::jqr(
          file(f),
          " ._id as $_id | .documents[] | { $_id,
            title, uuid, documentType, documentTypeLabel,
            fileType, associatedEntityId } ",
          flags = jqr::jq_flags(pretty = FALSE)
        ),
        file = downloadsNdjson,
        sep = "\n",
        append = TRUE)

      unlink(f)
    }

    # 3 - documents download
    dlFiles <- jsonlite::stream_in(
      file(downloadsNdjson), pagesize = 5000L, verbose = FALSE)
    unlink(downloadsNdjson)

    # check if any documents
    if (!nrow(dlFiles)) {

      message("\n= No documents identified for downloading.")

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
        dlFiles$title, ".",
        dlFiles$fileType)

      # calculate url
      dlFiles$url <- sprintf(
        ctisEndpoints[3], dlFiles$`_id`, dlFiles$uuid)

      # do download
      ctrDocsDownload(
        dlFiles[
          !duplicated(dlFiles$filename),
          c("_id", "filename", "url"), drop = FALSE],
        documents.path,
        documents.regexp,
        multiplex = FALSE,
        verbose = verbose)

    } # if (!nrow(dlFiles))

  } # !is.null(documents.path)

  ## inform user -----------------------------------------------------

  #  find out number of trials imported into database
  message("= Imported ", imported$n, ", updated ", updated,
          " record(s) on ", length(idsTrials), " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtis
