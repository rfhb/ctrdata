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
#' @importFrom stringi stri_replace_all_regex
#' @importFrom httr2 req_perform req_user_agent request
#' @importFrom rlang hash
#'
ctrLoadQueryIntoDbCtgov2 <- function(
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
    con, verbose,
    queryupdateterm) {
  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  ## ctgov api ---------------------------------------------------------

  ctgovEndpoints <- c(
    # pageSize 0 delivers default 10
    "https://www.clinicaltrials.gov/api/v2/studies?format=json&countTotal=true&pageSize=1&%s",
    "https://www.clinicaltrials.gov/api/v2/studies?format=json&countTotal=true&pageSize=1000&%s",
    "https://cdn.clinicaltrials.gov/large-docs/%s/%s/%s",
    "https://www.clinicaltrials.gov/api/int/studies/%s/history/%s",
    "https://www.clinicaltrials.gov/api/int/studies/%s?history=true"
  )

  ## process parameters ------------------------------------------------

  ## check
  if (!is.character(ctgov2history)) {
    ctgov2history <- deparse(ctgov2history)
  }
  if (!length(ctgov2history) ||
    !grepl("^(FALSE|TRUE|-1L?|1L?|[0-9]+L?:[0-9]+L?|[1-9]+[0-9]+L?|[1-9]+L?)$", ctgov2history)) {
    message("Parameter 'ctgov2history' invalid, ignored: ", ctgov2history)
    ctgov2history <- "FALSE"
  }

  # append if to update
  queryterm <- paste0(queryterm, "&", queryupdateterm)

  # mangle
  queryterm <- gsub("&$", "", queryterm)
  queryterm <- gsub("%20", " ", queryterm) # for URLencode

  # - translation to ClinicalTrials.gov REST API 2
  #   https://clinicaltrials.gov/data-about-studies/learn-about-api

  # - input CTGOV expert search is implicitly handled as it starts with "term="
  # - other input search terms are mapped to expert search terms for API call

  # distance=50 seems to be added in webinterface
  # even though not requested by user, removing it
  queryterm <- sub("([&]?)distance=50(&|$)", "&", queryterm)

  # slice by "&"
  queryterm <- strsplit(queryterm, split = "&")[[1]]
  queryterm <- queryterm[!grepl("^https://", queryterm)]
  queryterm <- queryterm[queryterm != ""]

  # location - special cases:
  # - if state and city identical, remove state
  queryValues <- sub("(.+)=(.*)", "\\2", queryterm)
  names(queryValues) <- sub("(.+)=(.*)", "\\1", queryterm)
  if (!is.na(queryValues["state"]) &&
    !is.na(queryValues["city"]) &&
    (queryValues["state"] == queryValues["city"])) {
    queryterm <-
      queryterm[!grepl("state=", queryterm)]
  }
  # - if only locStr, warn user
  if (!is.na(queryValues["locStr"]) &&
    (is.na(queryValues["country"]) &&
      is.na(queryValues["state"]) &&
      is.na(queryValues["city"]))) {
    stop(
      "Parameter 'locStr' provided, but no 'country', 'state' or 'city'; ",
      "please check in CTGOV; e.g., the name of a trial site should go ",
      "into Facility Name in the webinterface or parameter 'locn' in the ",
      "search URL.",
      call. = FALSE
    )
  }

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
        "resFirstPost=([0-9-]*)_?([0-9-]*)(&.+|$)",
        "primComp=([0-9-]*)_?([0-9-]*)(&.+|$)",
        "studyComp=([0-9-]*)_?([0-9-]*)(&.+|$)",
        "lastUpdPost=([0-9-]*)_?([0-9-]*)(&.+|$)",
        "firstPost=([0-9-]*)_?([0-9-]*)(&.+|$)",
        "start=([0-9-]*)_?([0-9-]*)(&.+|$)",
        "ageRange=([0-9a-z]*)_?([0-9a-z]*)(&.+|$)"
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
        "city=(.+)(&.+|$)",
        "state=(.+)(&.+|$)",
        "locn=(.+)(&|$)"
      ),
      "replace" = list(
        "AREA[LocationCountry]\\1",
        "AREA[LocationCity]\\1",
        "AREA[LocationState]\\1",
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
    # the values apparently can include AREA expressions
    #
    # https://clinicaltrials.gov/find-studies/constructing-complex-search-queries#areaOp
    # Declares which search area should be searched. Search areas are defined on the
    # ClinicalTrials.gov Search Area page. In addition to specifying search areas,
    # it is possible to specify a field from the study structure. Any field from the study
    # structure is searchable.
    #
    # https://clinicaltrials.gov/data-api/api#extapi
    # see /studies/ endpoint for relation of search areas to query parameters
    #
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
            sub(
              apiParams[[a]][["extract"]][[i]],
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
  for (i in seq_along(queryterm)) {
    nm <- names(queryterm)[i]
    if (nchar(nm)) queryterm[i] <- paste0(nm, "=", queryterm[i])
  }
  queryterm <- paste0(queryterm, collapse = "&")

  # adjust remaining quirks
  queryterm <- sub("^&", "", queryterm)
  queryterm <- gsub("&&+", "&", queryterm)
  queryterm <- gsub("RANGE\\[,", "RANGE[MIN,", queryterm)
  queryterm <- stringi::stri_replace_all_regex(queryterm, "(RANGE\\[.+?),\\]", "$1,MAX]")

  ## process query -----------------------------------------------------

  # corresponds to count
  url <- sprintf(ctgovEndpoints[1], queryterm)
  if (verbose) message("API call: ", url)
  message("* Checking trials in CTGOV...", appendLF = FALSE)
  #
  url <- utils::URLencode(url)
  counts <- try(httr2::req_perform(
    httr2::req_user_agent(
      httr2::request(url),
      ctrdataUseragent
    )), silent = TRUE)

  # early exit
  if (inherits(counts, "try-error") ||
      counts$status_code != 200L) {
    warning("Could not be retrieved, check 'queryterm' and / or 'register'. ",
      "\nAPI returned: ", rawToChar(counts$body),
      call. = FALSE
    )
    message("API call: ", url)
    return(emptyReturn)
  }

  # extract total number of trial records
  counts <- suppressMessages(rawToChar(counts$body))
  resultsEuNumTrials <- as.numeric(jqr::jq(counts, " .totalCount "))
  message("\b\b\b, found ", resultsEuNumTrials, " trials")

  # early exit
  if (!resultsEuNumTrials) {
    warning("No trials found, check 'queryterm' and 'register'")
    return(emptyReturn)
  }

  # only count?
  if (only.count) {
    # return
    return(list(
      n = resultsEuNumTrials,
      success = NULL,
      failed = NULL
    ))
  }

  # exit if too many records
  if (resultsEuNumTrials > 10000L) {
    stop(
      "These are ", resultsEuNumTrials, " (more than 10,000) trials, this may be ",
      "unintended. Downloading more than 10,000 trials may not be supported ",
      "by the register; consider correcting or splitting queries"
    )
  }

  ## download json -----------------------------------------------------

  # corresponds to trials

  url <- sprintf(ctgovEndpoints[2], queryterm)

  pageNextToken <- ""
  pageNumber <- 1L
  importDateTime <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")

  message(
    "- Downloading in ",
    ceiling(resultsEuNumTrials / 1000L),
    " batch(es) (max. 1000 trials each; estimate: ",
    format(resultsEuNumTrials * 0.1, digits = 2), " Mb total)"
  )

  # register
  on.exit(unlink(dir(tempDir, "ctgov_trials_[0-9]+.ndjson", full.names = TRUE)), add = TRUE)

  # download and compile into ndjson
  while (TRUE) {

    # page url
    urlToDownload <- ifelse(
      pageNextToken != "",
      paste0(url, "&pageToken=", pageNextToken),
      url
    )

    # for download
    fTrialJson <- file.path(
      tempDir, paste0(
        "ctgov_trials_",
        # include query in file name for potential re-download
        sapply(url, rlang::hash),
        "_", pageNumber, ".json"
      )
    )

    # do download
    resDf <- ctrMultiDownload(
      urls = urlToDownload,
      destfiles = fTrialJson,
      verbose = verbose
    )

    # inform user
    if (!nrow(resDf) == 1L || !resDf$success) message(
        "Download not successful for ", urlToDownload)

    # convert to ndjson
    message("- Converting to NDJSON...\r", appendLF = FALSE)
    fTrialsNdjson <- file.path(tempDir, paste0("ctgov_trials_", pageNumber, ".ndjson"))
    jqr::jq(
      file(fTrialJson),
      paste0(
        # extract trial records. studies seems always to be an array,
        # even for a single trial, thus no handling needed if array or not
        " .studies | .[] ",
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

  message("\n- Importing records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)
  message("")

  ## download history---------------------------------------------------

  if (ctgov2history != "FALSE") {
    message("* Checking and processing historic versions... ")

    ## 1 - get history overview for every trial
    urls <- as.vector(vapply(
      X = imported$success,
      FUN = function(i) sprintf(ctgovEndpoints[5], i),
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    ))

    files <- as.vector(vapply(
      X = urls,
      FUN = function(i) {
        file.path(
          tempDir, paste0(
            "h_ov_",
            sub(".+/(NCT[0-9]+)[?].+", "\\1", i),
            ".json"
          )
        )
      },
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    ))

    ctrMultiDownload(
      urls = urls,
      destfiles = files,
      verbose = verbose
    )

    # process
    historyDf <- lapply(
      X = files,
      FUN = function(i) {
        jsonlite::stream_in(
          textConnection(
            jqr::jq(file(i), paste0(
              " .history.changes[] | { ",
              '"_id": "', sub(".+_(NCT[0-9]+)[.]json", "\\1", i), '", ',
              "version_number: .version, version_date: .date }"
            ))
          ),
          pagesize = 5000L, verbose = FALSE
        )
      }
    )
    #
    historyDf <- do.call(rbind, historyDf)
    if (verbose) {
      message("Trial _id - number of versions")
      msgTmp <- as.data.frame(table(historyDf[["_id"]]))
      for (i in seq_len(nrow(msgTmp))) {
        message(
          msgTmp[i, 1, drop = TRUE], " - ",
          msgTmp[i, 2, drop = TRUE]
        )
      }
    }
    #
    # shift version number from API 0... to ctrdata 1...
    historyDf[["version_number"]] <- historyDf[["version_number"]] + 1L

    ## 2 - for specific ctgov2history
    ##     values, adjust historyDf
    #
    # n versions per trial
    if (grepl("^([1-9]+[0-9]+L?|[1-9]+L?)$", ctgov2history)) {
      ctgov2history <- sub("L$", "", ctgov2history)
      countVersions <- as.integer(ctgov2history)
      historyDf <- array2DF(tapply(
        historyDf, historyDf[["_id"]],
        FUN = function(i) {
          ntr <- sort(i[["version_number"]])
          ntl <- seq(1L, length(ntr), length.out = countVersions)
          i[i[["version_number"]] %in% ntr[ntl], ]
        }
      ))[, -1]
    }
    # last-but-one version
    if (grepl("^-1L?$", ctgov2history)) {
      ctgov2history <- sub("L$", "", ctgov2history)
      historyDf <- array2DF(tapply(
        historyDf, historyDf[["_id"]],
        FUN = function(i) {
          lbo <- max(i[["version_number"]])
          i[i[["version_number"]] == max(lbo - 1L, 1L), ]
        }
      ))[, -1]
    }
    # only initial version
    if (grepl("^1L?$", ctgov2history)) {
      historyDf <- historyDf[historyDf[["version_number"]] == 1L, ]
    }
    # selected versions
    if (grepl(":", ctgov2history)) {
      minVersion <- as.numeric(sub("([0-9]+)L?:([0-9]+)L?", "\\1", ctgov2history))
      maxVersion <- as.numeric(sub("([0-9]+)L?:([0-9]+)L?", "\\2", ctgov2history))
      soughtVersion <- historyDf[["version_number"]] >= minVersion &
        historyDf[["version_number"]] <= maxVersion
      historyDf <- historyDf[soughtVersion, ]
    }
    # construct urls
    urls <- sprintf(
      ctgovEndpoints[4], historyDf[["_id"]], historyDf[["version_number"]] - 1L
    )

    ## 3 - handle historic versions

    # calculate file paths
    files <- as.vector(vapply(
      X = urls,
      FUN = function(i) {
        file.path(
          tempDir, paste0(
            "h_v_",
            sub(".+/(NCT[0-9]+)/.+", "\\1", i), "_",
            sub(".+/([0-9]+)$", "\\1", i),
            ".json"
          )
        )
      },
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    ))

    # download
    message(
      "- Downloading ", length(files), " historic versions (estimate: ",
      format(length(files) * 2.7 / 71, digits = 2), " MB total)..."
    )

    resDf <- ctrMultiDownload(
      urls = urls,
      destfiles = files,
      verbose = verbose
    )

    ## 4 - merge versions by trial
    message("- Merging trial versions ", appendLF = FALSE)

    # register deletion
    on.exit(unlink(file.path(tempDir, paste0(
      "h_m_", unique(historyDf[["_id"]]), ".json"
    ))), add = TRUE)

    # do version merge
    sapply(
      X = unique(historyDf[["_id"]]),
      FUN = function(i) {

        out <- file.path(tempDir, paste0("h_m_", i, ".json"))
        unlink(out)
        outCon <- file(out, open = "at")
        on.exit(try(close(outCon), silent = TRUE), add = TRUE)

        # put historic versions into top level array
        writeLines(paste0('{"_id": "', i, '", "history": ['),
                   con = outCon, sep = "")

        fToMerge <- resDf[["destfile"]][grepl(i, resDf[["destfile"]])]

        # write history study versions into array
        for (ii in seq_along(fToMerge)) {

          if (!file.exists(fToMerge[ii]) || !file.size(fToMerge[ii]) > 10L) next

          if (ii > 1L) writeLines(",", con = outCon, sep = "")

          vn <- as.numeric(jqr::jq(file(fToMerge[ii]), " .studyVersion")) + 1L

          # add information about version
          writeLines(
            jqr::jq(file(fToMerge[ii]), paste0(
              ' .study | .history_version = { "version_number": ', vn, ",",
              ' "version_date": "', historyDf[["version_date"]][
                historyDf[["_id"]] == i & historyDf[["version_number"]] == vn
              ], '"} '
            ),
            flags = jqr::jq_flags(pretty = FALSE)
            ),
            con = outCon,
            sep = ""
          )

        }

        writeLines("]}", con = outCon)
        close(outCon)
        message(". ", appendLF = FALSE)
      },
      USE.NAMES = FALSE
    )

    ## 5 - import
    message("\n- Updating trial records ", appendLF = FALSE)
    resAll <- NULL
    for (f in dir(path = tempDir, pattern = "^h_m_.*.json$", full.names = TRUE)) {
      message(". ", appendLF = FALSE)
      res <- nodbi::docdb_update(src = con, key = con$collection, query = "{}", value = f)
      resAll <- c(resAll, res)
    }
    message("\nUpdated ", sum(resAll), " trial(s) with historic versions")
  }

  ## download files-----------------------------------------------------

  if (!is.null(documents.path)) {
    # user info
    message(
      "* Checking for documents...\n",
      "- Getting links to documents"
    )

    # temporary file for trial ids and file names
    downloadsNdjson <- file.path(tempDir, "ctgov2_downloads.ndjson")
    unlink(downloadsNdjson)
    downloadsNdjsonCon <- file(downloadsNdjson, open = "at")
    on.exit(try(close(downloadsNdjsonCon), silent = TRUE), add = TRUE)
    on.exit(unlink(downloadsNdjson), add = TRUE)

    # extract trial ids and file name and save in temporary file
    for (ndjsonFile in dir(
      path = tempDir, pattern = "^.+_trials_.*.ndjson$", full.names = TRUE
    )) {
      jqr::jq(
        file(ndjsonFile),
        " { _id: ._id,
            filename: .documentSection.largeDocumentModule.largeDocs[].filename }",
        flags = jqr::jq_flags(pretty = FALSE),
        out = downloadsNdjsonCon
      )
      message(". ", appendLF = FALSE)
    }
    close(downloadsNdjsonCon)
    message("\r", appendLF = FALSE)

    # get document trial id and file name
    dlFiles <- jsonlite::stream_in(
      file(downloadsNdjson),
      pagesize = 5000L, verbose = FALSE
    )

    # check if any documents
    if (!nrow(dlFiles)) {
      message("= No documents identified for downloading.")
    } else {
      # calculate urls
      dlFiles$url <- sprintf(
        ctgovEndpoints[3],
        sub(".*([0-9]{2})$", "\\1", dlFiles$`_id`),
        dlFiles$`_id`,
        dlFiles$filename
      )

      # do download
      ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path,
        documents.regexp,
        verbose = verbose
      )
    } # if (!nrow(dlFiles))
  } # !is.null(documents.path)

  ## delete for any re-downloads
  unlink(dir(
    path = tempDir, pattern = "ctgov_trials_[0-9]+.ndjson",
    full.names = TRUE
  ))

  ## inform user -----------------------------------------------------

  # find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtogv2023
