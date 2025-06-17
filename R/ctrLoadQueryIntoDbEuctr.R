### ctrdata package

#' ctrLoadQueryIntoDbEuctr
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom nodbi docdb_query docdb_update
#' @importFrom zip unzip
#' @importFrom stringi stri_replace_all_fixed stri_detect_fixed
#' @importFrom readr write_file read_file
#' @importFrom rlang hash
#' @importFrom httr2 req_perform_parallel request req_options req_throttle req_user_agent
#'
ctrLoadQueryIntoDbEuctr <- function(
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

  ## sanity correction for naked terms
  # otherwise all trials would be retrieved
  # see also ctrGetQueryUrl
  queryterm <- sub(
    "(^|&|[&]?\\w+=\\w+&)([ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
    "\\1query=\\2\\3",
    queryterm)

  # inform user
  message("* Checking trials in EUCTR...")

  ## euctr api ----------------------------------------------------------------

  queryEuRoot  <- "https://www.clinicaltrialsregister.eu/"
  queryEuType1 <- "ctr-search/search?"
  queryEuType3 <- "ctr-search/rest/download/full?"
  queryEuType4 <- "ctr-search/rest/download/result/zip/xml/"
  queryEuPost  <- paste0(
    "&mode=current_page&format=text&dContent=full",
    "&number=current_page&submit-download=Download")

  # get first result page
  q <- utils::URLencode(paste0(queryEuRoot, queryEuType1, queryterm))
  if (verbose) message("DEBUG: queryterm is ", q)
  #
  # TODO
  resultsEuPages <- try(httr2::req_perform(
    httr2::req_user_agent(
      httr2::request(q),
      ctrdataUseragent
    )), silent = TRUE)
  #
  if (inherits(resultsEuPages, "try-error") ||
      resultsEuPages$status_code != 200L) {
    if (grepl("SSL certificate.*local issuer certificate",
              rawToChar(resultsEuPages$body))) {
      stop("Host ", queryEuRoot, " cannot be queried as expected, error:\n",
           trimws(resultsEuPages), "\nFor a potential workaround, check ",
           "https://github.com/rfhb/ctrdata/issues/19#issuecomment-820127139",
           call. = FALSE)
    } else {
      stop("Host ", queryEuRoot, " not working as expected, ",
           "cannot continue: ", resultsEuPages[[1]], call. = FALSE)
    }
  }
  # - get content of response
  resultsEuPages <- rawToChar(resultsEuPages$body)

  # get number of trials identified by query
  resultsEuNumTrials <- sub(
    ".*Trials with a EudraCT protocol \\(([0-9,.]*)\\).*",
    "\\1",
    resultsEuPages)
  #
  resultsEuNumTrials <- suppressWarnings(
    as.integer(gsub("[,.]", "", resultsEuNumTrials)))
  #
  # no trials found even though host may have been online
  if (!is.integer(resultsEuNumTrials)) {
    message("ctrLoadQueryIntoDb(): register does not deliver ",
            "search results as expected, check if working with ",
            "'browseURL(\"", q, "\")'")
    return(invisible(emptyReturn))
  }

  # calculate number of results pages
  resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20)

  # check for plausibility and stop function without erro
  if (is.na(resultsEuNumPages) ||
      is.na(resultsEuNumTrials) ||
      (resultsEuNumTrials == 0)) {
    message("First result page empty - no (new) trials found?")
    return(invisible(emptyReturn))
  }

  # inform user
  message("Retrieved overview, multiple records of ",
          resultsEuNumTrials, " trial(s) from ",
          resultsEuNumPages, " page(s) to be downloaded ",
          "(estimate: ", signif(resultsEuNumTrials * 0.13, 1L), " MB)")

  # only count?
  if (only.count) {

    # return
    return(list(n = resultsEuNumTrials,
                success = NULL,
                failed = NULL))
  }

  # suggest chunking if large number of trials found
  if (as.integer(resultsEuNumTrials) > 10000L) {
    stop("These are ", resultsEuNumTrials, " (more than 10,000) trials; ",
         "consider correcting or splitting into separate queries")
  }

  ## protocol-related information ---------------------------------------------

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  # check results parameters
  if (is.null(documents.path)) {
    documents.path <- tempDir
  } else {
    euctrresults <- TRUE
    message("Note: Running with euctrresults = TRUE to download documents")
  }
  if (euctrresults &&
      (is.na(file.info(documents.path)[["isdir"]]) ||
       !file.info(documents.path)[["isdir"]])) {
    createdDir <- try(
      dir.create(documents.path, recursive = TRUE, showWarnings = FALSE),
      silent = TRUE)
    if (!inherits(createdDir, "try-errror") && createdDir) {
      message("Created directory ", documents.path)
    } else {
      warning("Directory could not be created for 'documents.path' ",
              documents.path, ", ignored", call. = FALSE)
      documents.path <- tempDir
    }
    # canonical directory path
    documents.path <- normalizePath(documents.path, mustWork = TRUE)
  }

  ## download all text files from pages

  # # new handle
  # h <- do.call(
  #   curl::new_handle,
  #   c(accept_encoding = "gzip,deflate,zstd,br",
  #     getOption("httr_config")[["options"]])
  # )
  #
  # # test fetch
  # tmp <- curl::curl_fetch_memory(
  #   url = paste0(
  #     queryEuRoot, queryEuType3,
  #     "query=2008-003606-33", "&page=1", queryEuPost),
  #   handle = h
  # )
  # # inform user about capabilities
  # sgzip <- curl::parse_headers(tmp$headers)
  # sgzip <- sgzip[grepl("Transfer-Encoding", sgzip)]
  # sgzip <- grepl("gzip|deflate", sgzip)
  # if (length(sgzip) && !sgzip) {
  #   message("Note: register server cannot compress data, ",
  #           "transfer takes longer (estimate: ",
  #           signif(resultsEuNumTrials * 1.2, 1L), " s)")
  # }
  #
  # # TODO

  # check host, takes around 1s
  initialData <- httr2::req_perform(
    httr2::req_user_agent(
      httr2::request(
        paste0(queryEuRoot, queryEuType3,
               "query=2008-003606-33", "&page=1", queryEuPost)),
      ctrdataUseragent))$headers
  names(initialData) <- tolower(names(initialData))

  # inform user
  if (is.null(initialData$`content-encoding`) ||
      !grepl("gzip|deflate", initialData$`content-encoding`)) message(
        "Note: register server cannot compress data, ",
        "transfer takes longer (estimate: ",
        signif(resultsEuNumTrials * 1.2, 1L), " s)")

  # inform user
  message("(1/3) Downloading trials...")

  # generate vector with URLs of all pages
  urls <- paste0(queryEuRoot, queryEuType3,
           queryterm, "&page=", 1:resultsEuNumPages, queryEuPost)

  # generate vector with file names for saving pages
  fp <- file.path(
    tempDir, paste0(
      "euctr_trials_",
      # appending hash of query for re-download
      sapply(urls, rlang::hash),
      ".txt"
    ))

  # do download and saving
  tmp <- ctrMultiDownload(
    urls = urls,
    destfiles = fp,
    verbose = verbose
  )

  if (nrow(tmp) != resultsEuNumPages) {
    message("Download from EUCTR failed; incorrect number of records")
    return(invisible(emptyReturn))
  }

  ## convert euctr to ndjson -----------------------------------------------

  if (length(.ctrdataenv$ct) == 0L) initTranformers()

  # run conversion (~12s for 563 trials)
  message("(2/3) Converting to NDJSON (estimate: ",
          signif(resultsEuNumTrials * 0.02, 1L), " s)...")

  tmp$ndjsonfile <- sub("[.]txt$", ".ndjson", tmp$destfile)

  sapply(seq_len(nrow(tmp)), function(r) {
    readr::write_file(
      .ctrdataenv$ct$call(
        "euctr2ndjson", readr::read_file(tmp$destfile[r]),
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      file = tmp$ndjsonfile[r])
  })

  ## import into database -----------------------------------------------

  # run import into database from json files
  message("(3/3) Importing records into database...")
  if (verbose) message("DEBUG: ", tempDir)

  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  # inform user on final import outcome
  message("= Imported or updated ",
          imported$n, " records on ",
          resultsEuNumTrials, " trial(s)")

  # read in the eudract numbers of the
  # trials just retrieved and imported
  eudractnumbersimported <- imported$success

  # remove calculated ndjson files in case of re-download import
  # because dbCTRLoadJSONFiles() imports all ndjson in the folder
  unlink(tmp$ndjsonfile)

  ## result-related information -----------------------------------------------

  if (euctrresults) {

    # results are available only one-by-one for
    # each trial as just retrieved and imported

    # transform eudract numbers
    # with country info ("2010-024264-18-3RD")
    # into eudract numbers ("2010-024264-18")
    eudractnumbersimported <- unique(
      substring(text = eudractnumbersimported,
                first = 1,
                last = 14))

    # inform user
    message("* Checking results if available from EUCTR for ",
            length(eudractnumbersimported), " trials: ")

    ## parallel download and unzipping into temporary directory

    # "https://www.clinicaltrialsregister.eu/ctr-search/rest/
    # download/result/zip/xml/..."
    # first version:  "2007-000371-42/1"
    # second version: "2007-000371-42/2"
    # latest version: "2007-000371-42"

    # inform user
    message("(1/4) Downloading results...")

    # prepare download and save

    # urls
    urls <- paste0(queryEuRoot, queryEuType4,
                   eudractnumbersimported)

    # destfiles
    fp <- file.path(
      tempDir, paste0(
        "euctr_results_",
        eudractnumbersimported,
        ".zip"
      ))

    # do download and save
    tmp <- ctrMultiDownload(
      urls = urls,
      destfiles = fp,
      verbose = verbose
    )

    # work only on successful downloads
    tmp <- tmp[tmp$success, , drop = FALSE]

    # inform user
    message(
      "- extracting results (. = data, F = file[s] and data, x = none):")

    # unzip downloaded files and move non-XML extracted files
    tmp <- lapply(
      tmp[["destfile"]], function(f) {

        if (file.exists(f) &&
            file.size(f) != 0L) {

          # this unzip does not handle special
          # characters under windows, thus only
          # obtain file names and extract with
          # extra package
          tmp <- utils::unzip(
            zipfile = f,
            list = TRUE)$Name
          if (is.null(tmp)) return(NULL)
          try(zip::unzip(
            zipfile = f,
            exdir = tempDir),
            silent = TRUE)

          # results in files such as
          # EU-CTR 2008-003606-33 v1 - Results.xml
          nonXmlFiles <- tmp[!grepl("Results[.]xml$", tmp)]
          euctrnr <- gsub(paste0(".*(", regEuctr, ").*"),
                          "\\1", tmp[grepl("Results[.]xml$", tmp)])[1]

          # any non-XML file
          if (length(nonXmlFiles)) {
            message("F ", appendLF = FALSE)
            if (documents.path != tempDir) {
              # move results file(s) to user specified directory
              saved <- try(suppressWarnings(
                file.rename(
                  from = file.path(tempDir, nonXmlFiles),
                  to = file.path(documents.path,
                                 paste0(euctrnr, "--", basename(nonXmlFiles))
                  ))), silent = TRUE)
              # inform user
              if (inherits(saved, "try-error")) {
                warning("Could not save ", nonXmlFiles, "; ", trimws(saved),
                        call. = FALSE, immediate. = TRUE)
                if (grepl("expanded 'from' name too long", saved)) {
                  message("Set options(ctrdata.tempdir = <dir>) to a ",
                          "short absolute path name for a directory.")
                }
              }
              if (!inherits(saved, "try-error") && any(!saved)) {
                warning("Could not save ", nonXmlFiles[!saved],
                        call. = FALSE, immediate. = TRUE)
              }
            } # if paths
          } else {
            # only XML data file
            if (any(grepl("Results[.]xml$", tmp)))
              message(". ", appendLF = FALSE)
          }

        } else {
          # unsuccessful
          message("x ", appendLF = FALSE)
        }

      }) # lapply fp

    # line break
    message("", appendLF = TRUE)

    ## convert xml to ndjson -----------------------------------------------

    if (length(.ctrdataenv$ct) == 0L) initTranformers()

    # for each file of an imported trial create new ndjson file
    xmlFileList <- dir(path = tempDir, pattern = "EU-CTR.+Results.xml", full.names = TRUE)
    xmlFileList <- xmlFileList[vapply(xmlFileList, function(i) {any(
      stringi::stri_detect_fixed(i, eudractnumbersimported))}, logical(1L))]
    on.exit(unlink(xmlFileList), add = TRUE)
    jsonFileList <- file.path(tempDir, paste0(
      "EU_Results_", sub(".+ ([0-9]{4}-[0-9]{6}-[0-9]{2}) .+", "\\1.ndjson", xmlFileList)))
    on.exit(unlink(jsonFileList), add = TRUE)

    # run conversion (~2 s for 19 records)
    message("(2/4) Converting to NDJSON (estimate: ",
            signif(length(xmlFileList) * 2 / 19, 1L), " s)...")

    sapply(seq_along(xmlFileList), function(f) {

      writeLines(stringi::stri_replace_all_fixed(
        .ctrdataenv$ct$call(
          "parsexml",
          # read source xml file
          paste0(readLines(xmlFileList[f], warn = FALSE), collapse = ""),
          # important parameters
          V8::JS("{trim: true, ignoreAttrs: false, mergeAttrs: true,
                     explicitRoot: false, explicitArray: false, xmlns: false}")),
        # remove conversion remnants and conformity breaking characters
        c('"xmlns:ns0":"http://eudract.ema.europa.eu/schema/clinical_trial_result",',
          '"xmlns:xsi":"http://www.w3.org/2001/XMLSchema-instance","xsi:nil":"true"',
          "&", "'", "\n", "\r", "\t"),
        c("", "", "&amp;", "&apos;", " ", " ", " "),
        vectorize_all = FALSE),
        con = file(jsonFileList[f])
      )

    }) # sapply xmlFileList

    # iterate over results files
    message("(3/4) Importing results into database (may take some time)...")

    # initiate counter
    importedresults <- 0L
    eudractnumbersimportedresults <- NULL

    # import results data from ndjson files
    for (f in jsonFileList) {

      if (!file.exists(f) || file.size(f) == 0L) next

      eudractNumber <- gsub(
        "\"", "", as.character(jqr::jq(file(f), " .eudractNumber ")))

      # update database with results
      tmp <- try({
        tmpnodbi <-
          nodbi::docdb_update(
            src = con,
            key = con$collection,
            value = f,
            query = paste0('{"a2_eudract_number": "', eudractNumber, '"}')
          )
        max(tmpnodbi, na.rm = TRUE)
      },
      silent = TRUE)

      # inform user on failed trial
      if (inherits(tmp, "try-error")) {
        warning(paste0(
          "Import of results failed for trial ", eudractNumber), immediate. = TRUE)
        tmp <- 0L
      } else {

        # output is number of trials updated
        eudractnumbersimportedresults <- c(eudractnumbersimportedresults, eudractNumber)
        importedresults <- importedresults + 1L

      }

      # inform user on records
      message(
        importedresults,
        " trials' records updated with results\r",
        appendLF = FALSE)

    } # for f

    ## result history information ---------------------------------------------

    # get result history from result webpage, section Results information
    importedresultshistory <- NULL
    if (euctrresultshistory) {

      # for date time conversion
      lct <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")

      # helper function
      extractResultsInformation <- function(t) {

        # get content of partial webpage
        wpText <- rawToChar(t[["body"]])
        eudractNumber <- t[["url"]]
        eudractNumber <- sub(
          paste0(".*(", regEuctr, ").*"),
          "\\1", eudractNumber)

        # extract information about results
        tmpFirstDate <- as.Date(trimws(
          sub(".+First version publication date</div>.*?<div>(.+?)</div>.*",
              "\\1", ifelse(grepl("First version publication date", wpText),
                            wpText, ""))),
          format = "%d %b %Y")

        tmpThisDate <- as.Date(trimws(
          sub(".+This version publication date</div>.*?<div>(.+?)</div>.*",
              "\\1", ifelse(grepl("This version publication date", wpText),
                            wpText, ""))),
          format = "%d %b %Y")

        tmpChanges <- trimws(
          gsub("[ ]+", " ", gsub("[\n\r]", "", gsub("<[a-z/]+>", "", sub(
            ".+Version creation reas.*?<td class=\"valueColumn\">(.+?)</td>.+",
            "\\1", ifelse(grepl("Version creation reas", wpText), wpText, ""))
          ))))

        # return
        return(list("eudractNumber" = eudractNumber,
                    "tmpFirstDate" = tmpFirstDate,
                    "tmpThisDate" = tmpThisDate,
                    "tmpChanges" = tmpChanges))
      }

      # this does not include the retrieval of information
      # about amendment to the study, as presented at the bottom
      # of the webpage for the respective trial results
      message("(4/4) Retrieving results history:                           ")

      # TODO start

      # # prepare download and save
      # pool <- curl::new_pool(
      #   multiplex = TRUE)
      # #
      # pc <- 0L
      # curlSuccess <- function(res) {
      #   pc <<- pc + 1L
      #   # incomplete data is 206L but some results pages are complete
      #   if (any(res$status_code == c(200L, 206L))) {
      #     retdat <<- c(retdat, list(extractResultsInformation(res)))
      #     message("\r", pc, " downloaded", appendLF = FALSE)
      #   }
      # }
      #
      # # compose urls to access results page
      # urls <- vapply(paste0(
      #   "https://www.clinicaltrialsregister.eu/ctr-search/trial/",
      #   eudractnumbersimportedresults, "/results"),
      #   utils::URLencode, character(1L))
      # # add urls to pool
      # tmp <- lapply(
      #   seq_along(urls),
      #   function(i) {
      #     h <- curl::new_handle(
      #       url = urls[i],
      #       range = "0-30000", # only top of page needed
      #       accept_encoding = "identity")
      #     curl::handle_setopt(h, .list = getOption("httr_config")[["options"]])
      #     curl::multi_add(
      #       handle = h,
      #       done = curlSuccess,
      #       pool = pool)
      #   })
      # # do download and save into batchresults
      # retdat <- list()
      # tmp <- try(curl::multi_run(pool = pool))
      #
      # # check plausibility
      # if (inherits(tmp, "try-error") || tmp[["error"]] || !length(retdat)) {
      #   stop("Download from EUCTR failed; last error with one or more of:\n",
      #        paste0(urls, collapse = "\n"), call. = FALSE
      #   )
      # }

      # TODO end

      # system.time(
      res <- httr2::req_perform_parallel(
        reqs = lapply(
          paste0(
            "https://www.clinicaltrialsregister.eu/ctr-search/trial/",
            eudractnumbersimportedresults, "/results"),
          FUN = function(u) {
            # start with basic request
            r <- httr2::request(u)
            r <- httr2::req_user_agent(r, ctrdataUseragent)

            # curl::curl_options("vers")
            r <- httr2::req_options(r, range = "0-30000")

            r <- httr2::req_throttle(
              req = r,
              # ensures that you never make more
              # than capacity requests in fill_time_s
              capacity = 20L * 60L,
              fill_time_s = 60L
            )
            return(r)
          }
        ),
        on_error = "continue",
        max_active = 10L
      )
      # )

      # mangle results info
      message("- processing...", appendLF = FALSE)
      retdat <- lapply(res, extractResultsInformation)

      #         res,
      #   function(r) {
      #     if (inherits(r, "httr2_failure")) return(
      #       data.frame(
      #         "success" = FALSE,
      #         "status_code" = NA_integer_,
      #         "url" = r$request$url,
      #         "destfile" = NA_character_,
      #         "content_type" = NA_character_,
      #         "urlResolved" = NA_character_,
      #         "data" = body2json(r$request$body$data)
      #       )
      #     )
      #     if (inherits(r, "httr2_error")) return(
      #       data.frame(
      #         "success" = FALSE,
      #         "status_code" = r$status,
      #         "url" = r$request$url,
      #         "destfile" = as.character(r$resp$body),
      #         "content_type" = NA_character_,
      #         "urlResolved" = NA_character_,
      #         "data" = body2json(r$request$body$data)
      #       )
      #     ) # else
      #     return(
      #       data.frame(
      #         "success" = TRUE,
      #         "status_code" = r[["status_code"]],
      #         "url" = r[["url"]],
      #         "destfile" = as.character(r[["body"]]),
      #         "content_type" = r[["headers"]]$`content-type`,
      #         "urlResolved" = NA_character_,
      #         "data" = body2json(r$request$body$data)
      #       )
      #     )})
      # res <- as.data.frame(do.call(rbind, res))

      # combine results
      resultHistory <- do.call(
        rbind,
        c(lapply(retdat, as.data.frame),
          stringsAsFactors = FALSE,
          make.row.names = FALSE))

      # apply to store in database
      message("\b\b\b, updating records ", appendLF = FALSE)
      importedresultshistory <- apply(
        resultHistory, 1,
        function(r) {
          r <- as.list(r)

          # check, add default, inform user
          if (is.na(r$tmpFirstDate) &
              is.na(r$tmpThisDate) &
              r$tmpChanges == "") {
            message("x ", appendLF = FALSE)
          } else {

            # update record
            message(". ", appendLF = FALSE)
            tmp <- nodbi::docdb_update(
              src = con,
              key = con$collection,
              value = list(
                "firstreceived_results_date" = as.character(r[["tmpFirstDate"]]),
                "this_results_date" = as.character(r[["tmpThisDate"]]),
                "version_results_history" = r[["tmpChanges"]]),
              query = paste0('{"a2_eudract_number":"', r[["eudractNumber"]], '"}')
            )

            # return if successful
            ifelse(inherits(tmp, "try-error"), 0L, 1L)
          }
        }) # apply resultsHistory

      # reset date time
      Sys.setlocale("LC_TIME", lct)

      # sum up successful downloads
      importedresultshistory <- sum(unlist(
        importedresultshistory, use.names = FALSE), na.rm = TRUE)

    } else {

      message("(4/4) Results history: not retrieved ",
              "(euctrresultshistory = FALSE)",
              appendLF = FALSE)

    } # if euctrresultshistory

    ## inform user on final import outcomes
    message("\n= Imported or updated results for ",
            importedresults, " trials")

    if (!is.null(importedresultshistory) &&
        importedresultshistory > 0L) {
      message("= Imported or updated results history for ",
              importedresultshistory, " trials")
    }
    if (documents.path != tempDir) {
      message("= documents saved in '", documents.path, "'")
    }

  } # if euctrresults

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr
