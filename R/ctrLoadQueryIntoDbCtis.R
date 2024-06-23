### ctrdata package

#' ctrLoadQueryIntoDbCtis
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jqr jq jq_flags
#' @importFrom tools toTitleCase
#' @importFrom nodbi docdb_update
#' @importFrom jsonlite stream_in fromJSON
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed
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

  ## output mangle helper -----------------------------------------------

  mangleText <- function(t) {

    stringi::stri_replace_all_fixed(str = t, pattern = "'", replacement = "&apos;")

  }

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
      # - 2 trial information - %s is ctNumber
      "/retrieve/%s", # partI and partsII
      #
      # - 3 download files
      "/documents/%s/%s/download" # trial _id, document[].uuid
      #
    )
  )

  ## api_1: overviews ---------------------------------------------------------

  # this is for importing overview (recruitment, status etc.) into database
  message("* Checking trials in CTIS...")

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  message("(1/4) Downloading trials list...", appendLF = FALSE)

  # queryterm comes from ctrGetQueryUrl()

  # 2024-06-17 defined by ctrdata Tampermonkey script:
  # https://euclinicaltrials.eu/ctis-public/search#searchCriteria=
  # {"containAll":"infection","containAny":"neonates","containNot":""}

  # get overview
  initialData <- try(rawToChar(
    curl::curl_fetch_memory(
      url = ctisEndpoints[1],
      handle = curl::new_handle(
        postfields = paste0(
          # add pagination parameters
          paste0(
            '{"pagination":{"page":1,"size":',
            ifelse(only.count, 1L, 100L), '},'),
          # add search criteria
          sub(
            "searchCriteria=", '"searchCriteria":',
            # handle empty search query terms
            ifelse(
              queryterm != "", queryterm,
              'searchCriteria={"containAll":"","containAny":"","containNot":""}'),
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
    jqr::jq(initialData, ' .pagination '))

  # early exit
  if (overview$totalRecords == 0L) {
    message("No trials found? Check 'queryterm' and / or 'register'.")
    return(emptyReturn)
  }

  # inform user
  message("\b\b\b, found ", overview$totalRecords, " trials")

  # only count?
  if (only.count) {
    # return
    return(list(
      n = overview$totalRecords,
      success = NULL,
      failed = NULL
    ))
  }

  # prepare to retrieve overviews
  importDateTime <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # temp file for mangled download
  fTrialsNdjson <- file.path(tempDir, "ctis_add_api1.ndjson")
  unlink(fTrialsNdjson)
  on.exit(unlink(fTrialsNdjson), add = TRUE)
  fTrialsNdjsonCon <- file(fTrialsNdjson, open = "at")
  on.exit(try(close(fTrialsNdjsonCon), silent = TRUE), add = TRUE)

  # construct post calls

  # select from json and write as ndjson
  success <- function(x){

    if (is.list(x)) x <- rawToChar(x$content)

    # {...,"data":[{"ctNumber":"2023-510173-34-00","ctStatus"
    jqr::jq(
      textConnection(x),
      paste0(
        # extract trial records
        " .data | .[] ",
        # add canonical elements
        '| .["_id"] = .ctNumber ',
        '| .["ctrname"] = "CTIS" ',
        '| .["record_last_import"] = "', importDateTime, '" ',
        # keep only standardised fields
        "| del(.id, .ctNumber, .product, .endPoint, .eudraCtInfo, .ctTitle,
               .primaryEndPoint, .sponsor, .conditions) "
      ),
      flags = jqr::jq_flags(pretty = FALSE),
      out = fTrialsNdjsonCon
    )

    message(". ", appendLF = FALSE)

  }

  # process initialData
  success(x = initialData)

  # get further pages data
  if (overview$totalPages > 1L) {

    # user info
    failure <- function(str) message(paste("Failed request:", str))

    # create pool of post requests
    pool <- curl::new_pool()

    # requests
    sapply(seq_len(overview$totalPages)[-1], function(i)
      curl::multi_add(
        curl::new_handle(
          url = ctisEndpoints[1],
          postfields = paste0(
            # pagination
            '{"pagination":{"page":', i, ',"size":100},',
            # queryterms
            '"searchCriteria":', queryterm, ',',
            '"sort":{"property":"decisionDate","direction":"DESC"}}'
          )
        ),
        done = success,
        fail = failure,
        data = NULL,
        pool = pool
      )
    )

    # run in parallel
    curl::multi_run(pool = pool)

  }

  # close connection
  close(fTrialsNdjsonCon)
  message("\r", appendLF = FALSE)

  # get ids
  idsTrials <- gsub(
    '"', "", as.character(
      jqr::jq(file(fTrialsNdjson), ' ."_id" ')))

  ## api_2: per trial partI, partsII ------------------------------------------

  # this is imported as the main data into the database

  message("(2/4) Downloading and processing trial data... (",
          "estimate: ", signif(length(idsTrials) * 27 / 312, 1L), " Mb)")

  urls <- sprintf(ctisEndpoints[2], idsTrials)

  fPartIPartsIIJson <- function(i) {
    file.path(tempDir, paste0("ctis_trial_partIpartsII_", i, ".json"))
  }

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  tmp <- ctrMultiDownload(urls, fPartIPartsIIJson(idsTrials), verbose = verbose)

  # convert partI and partsII details into ndjson file
  fPartIPartsIINdjson <- file.path(tempDir, "ctis_trials_api2.ndjson")
  on.exit(unlink(fPartIPartsIINdjson), add = TRUE)
  unlink(fPartIPartsIINdjson)
  fPartIPartsIINdjsonCon <- file(fPartIPartsIINdjson, open = "at")
  on.exit(try(close(fPartIPartsIINdjsonCon), silent = TRUE), add = TRUE)

  fi <- 0L
  for (fn in tmp[["destfile"]]) {
    if (!file.exists(fn)) next
    jqr::jq(
      textConnection(mangleText(readLines(fn, warn = FALSE))),
      # add _id to enable docdb_update()
      ' .["_id"] = .ctNumber ',
      flags = jqr::jq_flags(pretty = FALSE),
      out = fPartIPartsIINdjsonCon
    )
    fi <- fi + 1L
    message(fi, "\r", appendLF = FALSE)
  }
  close(fPartIPartsIINdjsonCon)
  message("\r", appendLF = FALSE)

  ## database import -----------------------------------------------------

  message("(3/4) Importing records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)

  # iterating over any additional ndjson files
  resAll <- NULL
  message("(4/4) Updating with additional data: ", appendLF = FALSE)

  for (f in dir(path = tempDir, pattern = "^ctis_add_api[0-9]*.ndjson$", full.names = TRUE)) {

    message(". ", appendLF = FALSE)
    res <- nodbi::docdb_update(src = con, key = con$collection, query = "{}", value = f)
    resAll <- c(resAll, res)

  }
  message("")

  ## api_3: documents -------------------------------------------------------

  if (!is.null(documents.path)) {

    # 1 - get ids of lists (which include urls to download)
    message("* Checking for documents...")

    # 2 - create data frame with info on documents (url, name, extension etc.)

    # get temporary file
    downloadsNdjson <- file.path(tempDir, "ctis_downloads.ndjson")
    on.exit(unlink(downloadsNdjson), add = TRUE)

    jqr::jq(
      file(fPartIPartsIINdjson),
      ' ._id as $_id | .documents[] | { $_id,
      title, uuid, documentType, documentTypeLabel,
      fileType, associatedEntityId } ',
      flags = jqr::jq_flags(pretty = FALSE),
      out = downloadsNdjson
    )
    unlink(fPartIPartsIINdjson)

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
        dlFiles$title, ".",
        dlFiles$fileType)

      # calculate url
      dlFiles$ctisurl <- sprintf(ctisEndpoints[3], dlFiles$`_id`, dlFiles$uuid)

      # get cdn download urls for cits urls
      resList <- data.frame(ctisurl = NULL, url = NULL)
      failure <- function(str) message(paste("Failed request:", str))
      success <- function(x) {
        resList <<- rbind(
          resList, cbind(
            ctisurl = x$url,
            url = jsonlite::fromJSON(rawToChar(x$content))$url
          ))
      }

      pool <- curl::new_pool()
      sapply(dlFiles$ctisurl, function(i)
        curl::multi_add(
          curl::new_handle(url = i),
          done = success,
          fail = failure,
          data = NULL,
          pool = pool
        ))
      curl::multi_run(pool = pool)
      dlFiles <- merge(dlFiles, resList)

      # do download
      resFiles <- ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path, documents.regexp, verbose)

    } # if (!nrow(dlFiles))

  } # !is.null(documents.path)

  ## inform user -----------------------------------------------------

  #  find out number of trials imported into database
  message("= Imported ", imported$n, ", updated ",
          paste0(resAll, collapse = " / "),
          " record(s) on ", length(idsTrials), " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtis
