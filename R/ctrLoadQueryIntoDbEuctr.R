### ctrdata package

#' ctrLoadQueryIntoDbEuctr
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom nodbi docdb_update
#' @importFrom zip unzip
#' @importFrom stringi stri_replace_all_fixed stri_detect_fixed stri_extract_all_regex
#' @importFrom readr write_file read_file
#' @importFrom rlang hash
#' @importFrom httr2 req_perform_parallel request req_options req_throttle req_user_agent
#' @importFrom V8 JS
#'
ctrLoadQueryIntoDbEuctr <- function(
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

  ## sanity correction for naked terms
  # otherwise all trials would be retrieved
  # see also ctrGetQueryUrl
  queryterm <- sub(
    "(^|&|[&]?\\w+=\\w+&)([ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
    "\\1query=\\2\\3",
    queryterm)

  ## euctr api ----------------------------------------------------------------

  euctrEndpoints <- paste0(
    #
    # root
    "https://www.clinicaltrialsregister.eu/",
    #
    # endpoints
    c(
      # search
      "ctr-search/search?%s",
      #
      # download summary file per webpage
      "ctr-search/rest/download/summary?%s&page=%i&mode=current_page",
      #
      # protocol data for single country trial as text, no gzip version accessible:
      # /ctr-search/rest/download/trial/2008-006649-18/GB
      "ctr-search/rest/download/trial/%s/%s",
      #
      # from download results as pdf, xml download was found:
      "ctr-search/rest/download/result/zip/xml/%s"
    )
  )

  # inform user
  message("* Checking trials in EUCTR...", appendLF = FALSE)

  # get first trial search result page
  if (verbose) message("\nDEBUG: ", sprintf(euctrEndpoints[1], queryterm))
  initialData <- try(
    httr2::req_perform(
      httr2::req_user_agent(
        httr2::request(
          utils::URLencode(
            sprintf(euctrEndpoints[1], queryterm))),
        ctrdataUseragent
      )),
    silent = TRUE)

  # check host
  if (inherits(initialData, "try-error") ||
      initialData$status_code != 200L) {
    if (grepl("SSL certificate.*local issuer certificate",
              rawToChar(initialData$body))) {
      stop(
        "Host https://www.clinicaltrialsregister.eu/ cannot be queried as expected,
        error:\n", trimws(initialData), "\nFor a potential workaround, check ",
        "https://github.com/rfhb/ctrdata/issues/19#issuecomment-820127139",
        call. = FALSE)
    } else {
      stop(
        "Host https://www.clinicaltrialsregister.eu/ not working as ",
        "expected, cannot continue: ", initialData[[1]], call. = FALSE)
    }
  }

  # get number of trials identified by query
  resultsEuPages <- rawToChar(initialData$body)
  resultsEuNumTrials <- sub(
    ".*Trials with a EudraCT protocol \\(([0-9,.]*)\\).*",
    "\\1", resultsEuPages)
  resultsEuNumTrials <- suppressWarnings(
    as.integer(gsub("[,.]", "", resultsEuNumTrials)))

  # no trials found
  if (!is.integer(resultsEuNumTrials)) {
    message("ctrLoadQueryIntoDb(): register does not deliver ",
            "search results as expected, check if working with ",
            "'browseURL(\"", q, "\")'")
    return(invisible(emptyReturn))
  }

  # calculate number of trial pages, euctr paging is fixed
  resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20L)

  # check for plausibility and stop function without erro
  if (is.na(resultsEuNumPages) ||
      is.na(resultsEuNumTrials) ||
      (resultsEuNumTrials == 0L)) {
    message("First result page empty - no (new) trials found?")
    return(invisible(emptyReturn))
  }

  # inform user
  message("\b\b\b, found ", resultsEuNumTrials, " trials ")

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

  #### . summary text files ####

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  # check parameters relevant for results
  if (is.null(documents.path)) {
    documents.path <- tempDir
  } else {
    euctrresults <- TRUE
    message("- Running with euctrresults = TRUE to download documents")
  }
  if (euctrresults &&
      (is.na(file.info(documents.path)[["isdir"]]) ||
       !file.info(documents.path)[["isdir"]])) {
    createdDir <- try(
      dir.create(documents.path, recursive = TRUE, showWarnings = FALSE),
      silent = TRUE)
    if (!inherits(createdDir, "try-errror") && createdDir) {
      message("- Created directory ", documents.path)
    } else {
      warning("Directory could not be created for 'documents.path' ",
              documents.path, ", ignored", call. = FALSE)
      documents.path <- tempDir
    }
    # canonical directory path
    documents.path <- normalizePath(documents.path, mustWork = TRUE)
  }

  ## download all text files from pages

  # inform user
  message(
    "- Downloading in ", resultsEuNumPages, " batch(es) (20 trials each; ",
    "estimate: ", signif(resultsEuNumPages * 173 / 141, 1L), " s)...")

  # vector with URLs of all summaries
  urls <- sprintf(
    euctrEndpoints[2],
    queryterm,
    1:resultsEuNumPages)

  #  vector with file names
  fp <- file.path(
    tempDir, paste0(
      "euctr_summary_",
      # appending hash of query for
      # caching if query is re-run
      sapply(urls, rlang::hash),
      ".txt"
    ))

  # do download and saving
  # print(system.time(
  resDf <- ctrMultiDownload(
    urls = urls,
    destfiles = fp,
    verbose = verbose)
  # )) # print system time

  if (nrow(resDf) != resultsEuNumPages) {
    message("Download from EUCTR failed; incorrect number of records")
    return(invisible(emptyReturn))
  }

  #### . trial by country text files ####

  ptrn <- paste0(countriesEUCTR, collapse = "|")
  ptrn <- paste0("[0-9]{4}-[0-9]{6}-[0-9]{2}|", ptrn)
  ptrn <- paste0("3rd|", ptrn) # 3RD trial urls need lowercase

  # get trial number and protocol countries
  trialsList <- unlist(lapply(
    resDf$destfile, function(f) {

      t <- stringi::stri_extract_all_regex(
        readLines(con = file(f), warn = FALSE),
        "EudraCT Number: .+|Trial protocol: .+",
        omit_no_match = TRUE, simplify = TRUE
      )[,1]
      t <- t[t != ""]

      # fix input for mapping to countriesEUCTR
      t <- stringi::stri_replace_all_fixed(t, "Outside EU/EEA", "3rd")
      t <- stringi::stri_replace_all_fixed(t, "(GB - no longer in EU/EEA)", "")

      # all in one pattern extraction
      p <- stringi::stri_extract_all_regex(
        t, ptrn, omit_no_match = TRUE, simplify = F)

      # conditionally keep only one protocol record
      if (!euctrprotocolsall) {
        p <- lapply(p, function(i) {
          if (length(i) == 1L) return(i)
          # intentionally do not match 3RD
          # (which would need to upper) as
          # per euctrprotocolsall help text
          i <- i[i %in% countriesActive]
          w <- countriesPreferred %in% i
          w <- countriesPreferred[w][1]
          if (!is.na(w)) return(w)
          if (!is.na(i[1])) return(i[1])
          return(NULL)
        })
      }

      # mangling list which has even number of elements
      lapply(
        seq_len(length(p) / 2L),
        function(i) unlist(c(p[i * 2L - 1L], p[i * 2L])))
    }
  ), recursive = FALSE)

  # create download urls
  urls <- unlist(sapply(
    trialsList, function(i)
      sprintf(euctrEndpoints[3], i[1L], i[-1L])))

  #  vector with file names
  fp <- file.path(
    tempDir, paste0(
      "euctr_trial_",
      # appending hash of query for
      # caching if query is re-run
      sapply(urls, rlang::hash),
      ".txt"
    ))

  # inform user
  message(
    "- Downloading ", length(urls), " records of ", resultsEuNumTrials,
    " trials (estimate: ", signif(length(urls) * 686 / 13608, 1L), " s)...")

  # no trials for downloading
  # (e.g., only GB and 3RD)
  if (!length(urls)) {
    # return
    return(list(n = length(urls),
                success = NULL,
                failed = NULL))
  }

  # do download and save
  # print(system.time(
  resDf <- ctrMultiDownload(
    urls = urls,
    destfiles = fp,
    verbose = verbose)
  # )) # print system time

  ## convert euctr to ndjson -----------------------------------------------

  if (length(.ctrdataenv$ct) == 0L) initTranformers()

  # run conversion
  message(
    "- Converting to NDJSON (estimate: ",
    signif(nrow(resDf) * 32 / 13608, 1L),
    " s)... ", appendLF = FALSE)

  ti <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  rl <- nrow(resDf)

  # register
  on.exit(unlink(dir(tempDir, "euctr_trials_[0-9]+.ndjson", full.names = TRUE)), add = TRUE)

  # convert trial records and add to chunks
  # corresponding to 50 trial records each
  # print(system.time(
  invisible(sapply(seq_len(rl %/% 50L + 1L), function(i) {

    fo <- file.path(tempDir, paste0("euctr_trials_", i, ".ndjson"))
    unlink(fo)
    bs <- 50L

    # write out batches of trial records
    # for rapid importing into database
    invisible(sapply(1:bs, function(r) {

      if (((i - 1L) * bs + r) > rl) return()
      fi <- resDf$destfile[(i - 1L) * bs + r]

      invisible(
        readr::write_file(
          .ctrdataenv$ct$call(
            # parameters to js function call
            "euctr2ndjson", readr::read_file(fi), ti),
          file = fo, append = TRUE)
      )

    })) # single batch
  })) # across batches
  # )) # print system time
  message()

  ## import into database -----------------------------------------------

  # run import into database from json files
  message("- Importing records into database...")
  if (verbose) message("DEBUG: ", tempDir)

  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  # remove calculated ndjson files in case of re-download import
  # because dbCTRLoadJSONFiles() imports all ndjson in the folder
  unlink(dir(tempDir, "euctr_trials_[0-9]+.ndjson", full.names = TRUE))

  # inform user on final import outcome
  message("= Imported or updated ",
          imported$n, " records on ",
          resultsEuNumTrials, " trial(s)")

  # read in the eudract numbers of the
  # trials just retrieved and imported
  eudractnumbersimported <- imported$success

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
    message("- Downloading results...")

    # prepare download and save

    # urls
    urls <- sprintf(euctrEndpoints[4], eudractnumbersimported)

    # destfiles
    fp <- file.path(
      tempDir,
      eudractnumbersimported,
      paste0(
        "euctr_results_",
        eudractnumbersimported,
        ".zip"
      ))

    # create subfolders
    if (verbose) message("DEBUG: creating folders per trial")
    sapply(fp, function(i) dir.create(dirname(i), showWarnings = FALSE))

    # do download and save
    resDf <- ctrMultiDownload(
      urls = urls,
      destfiles = fp,
      verbose = verbose
    )

    # work only on successful downloads
    unlink(dirname(resDf$destfile[!resDf$success]), recursive = TRUE)
    resDf <- resDf[resDf$success, , drop = FALSE]

    # inform user
    if (documents.path != tempDir) message(
      "- Downloading documents into 'documents.path' = ", documents.path)
    message(
      "- Extracting results (. = data, F = file[s] and data, x = none): ",
      appendLF = FALSE)

    # unzip downloaded files and move non-XML extracted files
    resF <- lapply(
      resDf[["destfile"]], function(f) {

        if (file.exists(f) &&
            file.size(f) != 0L) {

          # this unzip does not handle special
          # characters under windows, thus only
          # obtain file names and extract with
          # extra package
          unzFiles <- try(utils::unzip(
            zipfile = f,
            list = TRUE)$Name,
            silent = TRUE)
          if(inherits(unzFiles, "try-error") ||
             is.null(unzFiles)) {
            warning("No zipped content, deleting ", f,
                    call. = FALSE, immediate. = TRUE)
            unlink(dirname(f), recursive = TRUE)
            return(0L)
          }
          try(zip::unzip(
            zipfile = f,
            exdir = dirname(f)),
            silent = TRUE)

          # results in files such as
          # EU-CTR 2008-003606-33 v1 - Results.xml
          nonXmlFiles <- unzFiles[!grepl("Results[.]xml$", unzFiles)]
          euctrnr <- gsub(paste0(".*(", regEuctr, ").*"),
                          "\\1", unzFiles[grepl("Results[.]xml$", unzFiles)])[1]

          # any non-XML file
          if (length(nonXmlFiles)) {
            message("F ", appendLF = FALSE)
            if (documents.path != tempDir) {
              # create user specified directory
              dir.create(file.path(
                documents.path, euctrnr),
                showWarnings = FALSE)
              # move results file(s) to user specified directory
              saved <- try(suppressWarnings(
                file.rename(
                  from = file.path(dirname(f), nonXmlFiles),
                  to = file.path(documents.path, euctrnr, basename(nonXmlFiles))
                )), silent = TRUE)
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
            if (any(grepl("Results[.]xml$", unzFiles))) {
              message(". ", appendLF = FALSE)
            }
          } # length nonXmlFiles
          return(1L)
        } else {
          # unsuccessful
          message("x ", appendLF = FALSE)
          return(0L)
        }
      } #  function f
    ) # lapply fp

    # user feedback and line break
    message("\n- Data found for ", sum(unlist(resF)), " trials", appendLF = TRUE)

    ## convert xml to ndjson -----------------------------------------------

    if (length(.ctrdataenv$ct) == 0L) initTranformers()

    # for each file of an imported trial create new ndjson file

    # - get all result xml files
    xmlFileList <- dir(
      path = tempDir, pattern = "EU-CTR.+Results.xml",
      recursive = TRUE, full.names = TRUE)

    # - keep those related to protocol data just imported
    xmlFileList <- xmlFileList[vapply(xmlFileList, function(i) any(
      stringi::stri_detect_fixed(i, eudractnumbersimported)), logical(1L))]

    # - mark for removal
    on.exit(unlink(xmlFileList), add = TRUE)

    # - create target ndjson file names
    jsonFileList <- file.path(tempDir, paste0(
      "EU_Results_", sub(
        ".+ ([0-9]{4}-[0-9]{6}-[0-9]{2}) .+",
        "\\1.ndjson", xmlFileList)))

    # - mark for removal
    on.exit(unlink(jsonFileList), add = TRUE)

    # iterate conversion over eudract result files
    message(
      "- Converting to NDJSON (estimate: ",
      signif(length(xmlFileList) * 53 / 1889, 1L), " s)...")

    # run conversion
    # print(system.time(
    sapply(seq_along(xmlFileList), function(f) {

      # get eudract id
      idEuctr <- sub(
        paste0(".+", .Platform$file.sep, "(",
               regEuctr, ")", .Platform$file.sep, ".+"),
        "\\1", xmlFileList[f])

      # get country specific eudract id
      idTrials <- imported$success[grepl(idEuctr, imported$success)]

      # convert xml to ndjson, correct and save
      writeLines(
        stringi::stri_replace_all_fixed(
          stringi::stri_replace_all_fixed(
            #
            # convert xml to ndjson
            .ctrdataenv$ct$call(
              "parsexml",
              # read source file
              paste0(readLines(xmlFileList[f], warn = FALSE), collapse = ""),
              # important parameters for conversion
              V8::JS("{trim: true, ignoreAttrs: false, mergeAttrs: true,
                      explicitRoot: false, explicitArray: false, xmlns: false}")),
            #
            # remove remnants, replace conformity-breaking characters
            pattern = c(
              '"xmlns:ns0":"http://eudract.ema.europa.eu/schema/clinical_trial_result",',
              '"xmlns:xsi":"http://www.w3.org/2001/XMLSchema-instance","xsi:nil":"true"',
              "&", "'", "\n", "\r", "\t"),
            replacement = c(
              "", "",
              "&amp;", "&apos;", " ", " ", " "),
            vectorize_all = FALSE),
          #
          # add country specific eudract id
          pattern = paste0(
            '{"eudractNumber":"', idEuctr, '",'),
          replacement = paste0(
            '{"eudractNumber":"', idEuctr, '","_id":"', idTrials,'",'),
          vectorize_all = TRUE),
        #
        con = file(jsonFileList[f])
      )

    }) # sapply xmlFileList
    # )) # print system time

    # iterate over results files
    message("- Importing ", length(jsonFileList),
            " results into database (may take some time)...")

    # initiate counter
    importedresults <- 0L
    eudractnumbersimportedresults <- NULL

    # import results data from ndjson files
    for (f in jsonFileList) {

      if (!file.exists(f) || file.size(f) == 0L) next

      # get eudract id
      eudractNumber <- sub(
        paste0(".+", .Platform$file.sep, "(",
               regEuctr, ")", .Platform$file.sep, ".+"),
        "\\1", f)

      # update record(s) with results
      res <- try({
        tmpnodbi <-
          nodbi::docdb_update(
            src = con,
            key = con$collection,
            value = f,
            query = "{}")
        max(tmpnodbi, na.rm = TRUE)
      },
      silent = TRUE)

      # inform user on failed trial
      if (inherits(res, "try-error")) {

        warning(paste0(
          "Import of results failed for trial ", eudractNumber),
          immediate. = TRUE)
        res <- 0L

      } else {

        # output is number of trials updated
        eudractnumbersimportedresults <- c(
          eudractnumbersimportedresults, eudractNumber)
        importedresults <- importedresults + 1L

      }

      # inform user on records
      message(
        importedresults,
        " trial records updated with results\r",
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
        return(list(
          "eudractNumber" = eudractNumber,
          "tmpFirstDate" = tmpFirstDate,
          "tmpThisDate" = tmpThisDate,
          "tmpChanges" = tmpChanges))
      }

      # this does not include the retrieval of information
      # about amendment to the study, as presented at the bottom
      # of the webpage for the respective trial results
      message("- Retrieving results history:                           ")

      res <- suppressWarnings(
        httr2::req_perform_parallel(
          reqs = lapply(
            paste0(
              "https://www.clinicaltrialsregister.eu/ctr-search/trial/",
              eudractnumbersimportedresults, "/results"),
            FUN = function(u) {
              # start with basic request
              r <- httr2::request(u)
              r <- httr2::req_user_agent(r, ctrdataUseragent)

              # curl::curl_options("vers")
              r <- httr2::req_options(r, http_version = 2)
              r <- httr2::req_options(r, range = "0-30000")

              r <- httr2::req_throttle(
                req = r,
                # ensures that you never make more
                # than capacity requests in fill_time_s
                capacity = 20L * 10L,
                fill_time_s = 10L
              )
              return(r)
            }
          ),
          on_error = "continue",
          max_active = 10L
        ))

      # mangle results info
      message("- processing...", appendLF = FALSE)
      retdat <- lapply(res, extractResultsInformation)

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
            res <- nodbi::docdb_update(
              src = con,
              key = con$collection,
              value = list(
                "firstreceived_results_date" = as.character(r[["tmpFirstDate"]]),
                "this_results_date" = as.character(r[["tmpThisDate"]]),
                "version_results_history" = r[["tmpChanges"]]),
              query = paste0('{"a2_eudract_number":"', r[["eudractNumber"]], '"}')
            )

            # return if successful
            ifelse(inherits(res, "try-error"), 0L, 1L)
          }
        }) # apply resultsHistory

      # reset date time
      Sys.setlocale("LC_TIME", lct)

      # sum up successful downloads
      importedresultshistory <- sum(unlist(
        importedresultshistory, use.names = FALSE), na.rm = TRUE)

    } else {

      message("- Results history: not retrieved ",
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
      message("= Documents saved in '", documents.path, "'")
    }

  } # if euctrresults

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr
