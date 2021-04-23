### ctrdata package
### main functions

#' Retrieve or update information on clinical trials from register
#' and store in database
#'
#' This is the main function of package \link{ctrdata} for accessing
#' registers. Note that re-rerunning this function adds or updates
#' trial records in a database, even if from different queries or
#' different registers. Updating means that the previously stored
#' record is overwritten; see \code{annotation.text} for persisting
#' user comments added to a record.
#'
#' @param queryterm Either a string with the full URL of a search in
#' a register, or the data frame returned by the
#' \link{ctrGetQueryUrl} or the
#' \link{dbQueryHistory} functions, or, together with parameter
#' \code{register}, a string with query elements of a search URL.
#' The queryterm is recorded in the \code{collection} for later
#' use to update records.
#'
#' @param register String with abbreviation of register to query,
#' at this time either "EUCTR" (default) or "CTGOV". Not needed
#' if \code{queryterm} provide the information which register to
#' query (see \code{queryterm}).
#'
#' @param querytoupdate Either the word "last" or the number of the
#' query (based on \link{dbQueryHistory}) that should be run to
#' retrieve any trial records that are new or have been updated
#' since this query was run the last time.
#' This parameter takes precedence over \code{queryterm}.
#' For EUCTR, updates are available only for the last seven days;
#' the query is run again if more time has passed since it was
#' run last.
#'
#' @param forcetoupdate If \code{TRUE}, run again the query
#' given in \code{querytoupdate}, irrespective of when it was
#' run last (default is \code{FALSE}).
#'
#' @param euctrresults If \code{TRUE}, also download available
#' results when retrieving and loading trials from EUCTR. This
#' slows down this function. (For CTGOV, all available results
#' are always retrieved and loaded.)
#'
#' @param euctrresultshistory If \code{TRUE}, also download
#' available history of results publication in EUCTR.
#' This is quite time-consuming (default is \code{FALSE}).
#'
#' @param euctrresultspdfpath If a valid directory is specified,
#' save PDF files of result publications from EUCTR into
#' this directory (default is \code{NULL}).
#'
#' @param annotation.text Text to be including in the records
#' retrieved with the current query, in the field "annotation".
#'
#' @param annotation.mode One of "append" (default), "prepend"
#' or "replace" for new annotation.text with respect to any
#' existing annotation for the records retrieved with the
#' current query.
#'
#' @param parallelretrievals Number of parallel downloads of
#' information from the register, defaults to 10.
#'
#' @param only.count Set to \code{TRUE} to return only the
#' number of trial records found in the register for the query.
#' Does not load trial information into the database.
#' Default is \code{FALSE}.
#'
#' @inheritParams ctrDb
#'
#' @param verbose Printing additional information if set to
#' \code{TRUE}; default is \code{FALSE}.
#'
#' @return A list with elements "n" (the number of trials that
#' were newly imported or updated with this function call),
#' "ids" (a vector of the _id[s] of these trials) and the
#' "queryterm" used, with several attributes set
#' (database connection details and a data frame of
#' the query history in this database).
#'
#' @examples
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "test"
#' )
#' # Retrieve protocol-related information on a
#' # single trial identified by EudraCT number
#' ctrLoadQueryIntoDb(
#'   queryterm = "2013-001291-38", con = db
#' )
#' # Retrieve protocol-related information on
#' # ongoing interventional cancer trials in children
#' ctrLoadQueryIntoDb(
#'   queryterm = "cancer&recr=Open&type=Intr&age=0",
#'   register = "CTGOV",
#'   con = db
#' )
#' }
#'
#' @export
#'
ctrLoadQueryIntoDb <- function(
  queryterm = "",
  register = "EUCTR",
  querytoupdate = 0L,
  forcetoupdate = FALSE,
  euctrresults = FALSE,
  euctrresultshistory = FALSE,
  euctrresultspdfpath = NULL,
  annotation.text = "",
  annotation.mode = "append",
  parallelretrievals = 10L,
  only.count = FALSE,
  con = NULL,
  verbose = FALSE) {

  ## system check, in analogy to onload.R
  if (.Platform$OS.type == "windows") {
    if (!installCygwinWindowsTest()) {
      stop(call. = FALSE)
    }
  }

  ## check params for update request
  if ((class(querytoupdate) != "character" &&
       querytoupdate != trunc(querytoupdate)) ||
      (class(querytoupdate) == "character" &&
       querytoupdate != "last")) {
    stop("Parameter 'querytoupdate' is not an integer value or 'last'.",
         call. = FALSE)
  }

  ## deduce queryterm and register
  ## if not querytoupdate
  if (querytoupdate == 0L) {

    # check queryterm
    if (!is.data.frame(queryterm)) {

      # obtain url and register
      tmpTest <- try(
        ctrGetQueryUrl(
          url = queryterm,
          register = register),
        silent = TRUE)

      if (inherits(tmpTest, "try-error")) {
        stop("Cannot use 'queryterm' ", deparse(substitute(queryterm)),
             " and / or 'register' ", deparse(substitute(register)),
             call. = FALSE)
      }

      queryterm <- tmpTest
    }

    # deal with data frame as returned from
    # ctrQueryHistoryInDb and ctrGetQueryUrl
    if (!all(substr(names(queryterm), 1, 6) == "query-") ||
        !is.data.frame(queryterm)) {
      stop("'queryterm' does not seem to result from ctrQueryHistoryInDb() ",
           "or ctrGetQueryUrl(): ", deparse(queryterm), call. = FALSE)
    }

    # process queryterm dataframe
    nr <- nrow(queryterm)
    #
    if (nr > 1) {
      warning(
        "Using last row of queryterm parameter.",
        call. = FALSE, immediate. = TRUE)
    }
    #
    register  <- queryterm[nr, "query-register"]
    queryterm <- queryterm[nr, "query-term"]

    # check queryterm
    if (length(queryterm) != 1L ||
        class(queryterm) != "character" ||
        is.na(queryterm) ||
        nchar(queryterm) == 0L) {
      stop("'queryterm' has to be a non-empty string: ",
           deparse(queryterm), call. = FALSE)
    }

    # check register
    if (length(register) != 1L ||
        class(register) != "character" ||
        is.na(register)) {
      stop("'register' has to be a non-empty string: ",
           register, call. = FALSE)
    }

    ## sanity checks
    if (grepl("[^.a-zA-Z0-9=+&%_:\", -]",
              gsub("\\[", "", gsub("\\]", "", queryterm)))) {
      stop("Parameter 'queryterm' is not an URL showing results ",
           "of a query or has unexpected characters: ", queryterm,
           ", expected are: a-zA-Z0-9=+&%_-,.: []\"",
           call. = FALSE)
    }
    #
    if ((queryterm == "") &
        querytoupdate == 0L) {
      stop("Parameter 'queryterm' is empty.",
           call. = FALSE)
    }
    #
    if (!grepl(register, "CTGOVEUCTR")) {
      stop("Parameter 'register' not known: ",
           register, call. = FALSE)
    }

    # remove trailing or leading whitespace, line breaks
    queryterm <- gsub("^\\s+|\\s+$|\n|\r", "", queryterm)

  } # if not querytoupdate

  # check annotation parameters
  if (annotation.text != "" &
      !any(annotation.mode == c("append", "prepend", "replace"))) {
    stop("'annotation.mode' incorrect", call. = FALSE)
  }

  # initialise variable that is filled if an update is to be made
  queryupdateterm <- ""

  # check and set proxy if needed to access internet
  setProxy()

  ## handle if we need to rerun previous query

  # check if parameters are consistent
  if ((querytoupdate > 0) &&
      (!is.atomic(queryterm) || queryterm != "")) {
    stop("'queryterm' and 'querytoupdate' specified,",
         " which is inconsistent, cannot continue.",
         call. = FALSE)
  }

  # rewrite parameters for running as update
  querytermoriginal <- queryterm
  if ((querytoupdate > 0) &&
      (queryterm == "")) {
    #
    rerunparameters <- ctrRerunQuery(
      querytoupdate = querytoupdate,
      forcetoupdate = forcetoupdate,
      con = con,
      verbose = verbose,
      queryupdateterm = queryupdateterm)
    #
    # check rerunparameters and stop ctrLoadQueryIntoDb
    # without error if rerunparameters cannot be used for
    # running and loading a query
    if (!is.data.frame(rerunparameters)) {
      return(invisible(rerunparameters))
    }
    #
    # set main parameters
    querytermoriginal <- rerunparameters$querytermoriginal
    queryupdateterm   <- rerunparameters$queryupdateterm
    queryterm         <- rerunparameters$queryterm
    register          <- rerunparameters$register
  }

  ## main function

  # parameters for core functions
  params <- list(queryterm = queryterm,
                 register = register,
                 euctrresults = euctrresults,
                 euctrresultshistory = euctrresultshistory,
                 euctrresultspdfpath = euctrresultspdfpath,
                 annotation.text = annotation.text,
                 annotation.mode = annotation.mode,
                 parallelretrievals = parallelretrievals,
                 only.count = only.count,
                 con = con, verbose = verbose,
                 queryupdateterm = queryupdateterm)

  # call core functions
  imported <- switch(
    as.character(register),
    "CTGOV" = do.call(ctrLoadQueryIntoDbCtgov, params),
    "EUCTR" = do.call(ctrLoadQueryIntoDbEuctr, params)
  )

  # add query used for function
  imported <- c(imported, "queryterm" = querytermoriginal)

  ## finalise

  # only count?
  if (only.count) {
    # return number of trials
    return(imported)
  }

  # return some useful information or break
  if (!exists("imported") ||
      (imported$n == 0)) {
    message("Function did not result in any trial information imports.")
    return(invisible(list(n = 0, ids = "")))
  }

  # inform user
  if (verbose) {
    message("DEBUG: 'queryterm'=", queryterm,
            "\n'queryupdateterm'=", queryupdateterm,
            "\n'imported'=", imported$n,
            "\n'register'=", register,
            "\n'collection'=", con$collection,
            "\nImported trials: ", paste0(imported$success, collapse = " "))
  }

  # add query parameters to database
  dbCTRUpdateQueryHistory(register = register,
                          queryterm = querytermoriginal,
                          recordnumber = imported$n,
                          con = con,
                          verbose = verbose)

  # invalidate any cached list of keys in collection
  if (exists(".dbffenv")) {
    suppressWarnings({
      remove(list = paste0(con$db, "/", con$collection),
             envir = .dbffenv)
    })
  }

  # add metadata
  imported <- addMetaData(x = imported,
                          con = con)

  ## return
  return(imported)

}
# end ctrLoadQueryIntoDb


#' ctrRerunQuery
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom httr content GET
#'
ctrRerunQuery <- function(
  querytoupdate = querytoupdate,
  forcetoupdate = forcetoupdate,
  con = con,
  verbose = verbose,
  queryupdateterm = queryupdateterm) {

  ## check database connection
  con <- ctrDb(con = con)

  ## prepare

  # get history
  rerunquery <- dbQueryHistory(con = con,
                               verbose = verbose)

  # check parameters
  if (is.null(rerunquery))
    stop("'querytoupdate': no previous queries found in collection, ",
         "aborting query update.", call. = FALSE)

  # select last query if specified
  if (querytoupdate == "last")
    querytoupdate <- nrow(rerunquery)

  # try to select the query to be updated
  if (querytoupdate > nrow(rerunquery)) {
    stop("'querytoupdate': specified number not found, check ",
         "'dbQueryHistory()'.", call. = FALSE)
  }

  # set query values as retrieved
  queryterm  <- rerunquery[querytoupdate, "query-term"]
  register   <- rerunquery[querytoupdate, "query-register"]

  # when was this query last run?
  #
  # - dates of all the same queries
  initialday <- rerunquery[["query-timestamp"]][
    rerunquery[querytoupdate, "query-term"] ==
      rerunquery[["query-term"]]]
  #
  # - remove time, keep date
  initialday <- substr(
    initialday,
    start = 1,
    stop = 10)
  #
  # - change to Date class and get
  #   index of latest (max) date,
  initialdayindex <- try(
    which.max(
      as.Date(initialday,
              format = "%Y-%m-%d")))
  if (!inherits(initialdayindex, "try-error")) {
    # - keep initial (reference) date of this query
    initialday <- initialday[initialdayindex]
  } else {
    # - fallback to number (querytoupdate)
    #   as specified by user
    initialday <- rerunquery[querytoupdate, "query-timestamp"]
  }

  # secondary check parameters
  if (queryterm == "") {
    stop("Parameter 'queryterm' is empty - cannot update query ",
         querytoupdate, call. = FALSE)
  }
  #
  if (!grepl(register, "CTGOVEUCTR")) {
    stop("Parameter 'register' not known - cannot update query ",
         querytoupdate, call. = FALSE)
  }

  ## adapt updating procedure to respective register
  querytermoriginal <- queryterm

  ## mangle parameter only if not forcetoupdate,
  # which stipulates to just rerun original query
  if (!forcetoupdate) {

    # ctgov
    if (register == "CTGOV") {

      # ctgov:
      # speficy any date - "lup_s/e" last update start / end:
      # https://clinicaltrials.gov/ct2/results?term=&recr=&rslt=&type=Intr&cond=
      # Cancer&intr=&titles=&outc=&spons=&lead=
      # &id=&state1=&cntry1=&state2=&cntry2=&state3=&cntry3=&locn=&gndr=&age=0
      # &rcv_s=&rcv_e=&lup_s=01%2F01%2F2015&lup_e=12%2F31%2F2016

      # if "lup_s" is already in query term, just re-run full query to avoid
      # multiple queries in history that only differ in the timestamp:
      if (grepl("&lup_[se]=[0-9]{2}", queryterm)) {
        #
        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ",
                "('&lup_'); running again with these limits.",
                immediate. = TRUE)
        #
      } else {
        #
        queryupdateterm <- strftime(
          strptime(initialday,
                   format = "%Y-%m-%d"),
          format = "%m/%d/%Y")
        #
        queryupdateterm <- paste0("&lup_s=", queryupdateterm)
        #
        if (verbose) {
          message("DEBUG: Updating using this additional query term: ",
                  queryupdateterm)
        }
        #
      }
      #
      message("Rerunning query: ", queryterm,
              "\nLast run: ", initialday)
    }

    # euctr
    if (register == "EUCTR") {

      # euctr: studies added or updated in the last 7 days:
      # "https://www.clinicaltrialsregister.eu/ctr-search/rest/feed/
      # bydates?query=cancer&age=children"

      # check if update request is in time window of the register (7 days)
      if (difftime(Sys.Date(), initialday, units = "days") > 7) {
        #
        warning("'querytoupdate=", querytoupdate, "' not possible because ",
                "it was last run more than 7 days ago and the register ",
                "provides information on changes only for the last 7 days. ",
                "Reverting to normal download.",
                immediate. = TRUE)
        #
        message("Rerunning query: ", queryterm,
                "\nLast run: ", initialday)
        #
      } else {
        #
        # obtain rss feed with list of recently updated trials
        rssquery <- utils::URLencode(
          paste0("https://www.clinicaltrialsregister.eu/ctr-search/",
                 "rest/feed/bydates?", queryterm))
        #
        if (verbose) {
          message("DEBUG (rss url): ", rssquery)
        }
        #
        resultsRss <- try(httr::content(
          httr::GET(url = rssquery),
          encoding = "UTF-8",
          as = "text"), silent = TRUE)

        if (verbose) {
          message("DEBUG (rss content): ", resultsRss)
        }
        #
        # attempt to extract euctr number(s)
        resultsRssTrials <- gregexpr(
          "eudract_number:[0-9]{4}-[0-9]{6}-[0-9]{2}</link>",
          resultsRss)[[1]]
        #
        if (length(resultsRssTrials) == 1L &&
            resultsRssTrials == -1L) {
          message("First result page empty - no (new) trials found?")
          return(invisible(list(
            # return structure as in dbCTRLoadJSONFiles
            # which is handed through to ctrLoadQueryIntoDb
            n = 0L,
            success = character(0L),
            failed = character(0L),
            queryterm = querytermoriginal)))
        }
        #
        # if new trials found, construct
        # differential query to run
        resultsRssTrials <- vapply(
          resultsRssTrials, FUN = function(x)
            substr(resultsRss, x + 15, x + 28), character(1L))
        #
        resultsRssTrials <- paste0(
          "query=",
          paste(
            resultsRssTrials,
            collapse = "+OR+"))
        #
        if (verbose) {
          message("DEBUG (rss trials): ", resultsRssTrials)
        }
        #
        # run query for extracted euctr number(s)
        # store original query in update term
        queryupdateterm <- queryterm
        queryterm <- resultsRssTrials
        #
        if (verbose) {
          message("DEBUG: Updating using this queryterm: ",
                  queryupdateterm)
        }
        #
        message("Rerunning query: ", queryupdateterm,
                "\nLast run: ", initialday)
        #
      }
    } # register euctr
  } # forcetoupdate

  ## return main parameters needed
  return(data.frame(
    "querytermoriginal" = querytermoriginal,
    "queryupdateterm"   = queryupdateterm,
    "queryterm"         = queryterm,
    "register"          = register,
    stringsAsFactors = FALSE))

} # end ctrRerunQuery


#' dbCTRLoadJSONFiles
#'
#' @param dir Path to local directory with JSON files
#' from downloading and converting
#'
#' @importFrom jsonlite validate
#' @importFrom nodbi docdb_create docdb_update
#' @importFrom stats na.omit
#'
#' @inheritParams ctrDb
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return List with elements n (number of imported trials),
#' _id's of successfully imported trials and
#' _id's of trials that failed to import
#'
#' @keywords internal
#'
dbCTRLoadJSONFiles <- function(dir, con, verbose) {

  # find files
  tempFiles <- dir(path = dir,
                   pattern = ".json",
                   full.names = TRUE)

  # initialise counters
  fc <- 0L
  ic <- 0L

  # iterate over files
  retimp <- sapply(
    X = tempFiles,
    function(tempFile) {

      # main function for fast reading,
      # switching off warning about final EOL missing
      fd <- file(description = tempFile)
      tmplines <- readLines(con = fd, warn = FALSE)
      close(fd)

      # inform user
      if (verbose) message("DEBUG: read file ", tempFile,
                           "\nImporting line: ", appendLF = FALSE)

      # readLines produces: \"_id\": \"2007-000371-42-FR\"
      ids <- sub(".*_id\":[ ]*\"(.*?)\".*", "\\1", tmplines)

      # ids should always be found and later,
      # one id will be assumed to be on one line each
      if (all(ids == "")) {
        stop("No _id(s) detected in JSON file ",
             tempFile, call. = FALSE)
      }

      # update counters
      lc <- length(ids)
      ic <- ic + 1L
      fc <- fc + 1L

      # check if in database, create or update
      tmpinsert <- lapply(
        X = seq_along(ids),
        function(i) {

          # inform user
          message("JSON record #: ", i, " / ", lc,
                  ", file #: ", ic, " / ", fc, "\r",
                  appendLF = FALSE)

          # one row is one trial record

          # check validity
          tmpvalidate <- jsonlite::validate(tmplines[i])
          if (!tmpvalidate) {
            warning("Invalid json for trial ", ids[i], "\n",
                    "Line ", i, " in file ", tempFile, "\n",
                    attr(x = tmpvalidate, which = "err"),
                    noBreaks. = TRUE,
                    call. = FALSE,
                    immediate. = TRUE)
          }

          # slice data to be processed
          value <- data.frame("_id" = ids[i],
                              "json" = tmplines[i],
                              stringsAsFactors = FALSE,
                              check.names = FALSE)

          # load into database
          # - first, try update
          tmp <- try({
            nodbi::docdb_update(src = con,
                                key = con$collection,
                                value = value)
          }, silent = TRUE)
          # - if error, try insert
          if (inherits(tmp, "try-error") ||
              tmp == 0L) {
            tmp <- try({
              nodbi::docdb_create(src = con,
                                  key = con$collection,
                                  value = value)
            }, silent = TRUE)
          }

          # return values for lapply
          if (inherits(tmp, "try-error") ||
              tmp == 0L) {
            # inform user
            message(ids[i], ": ", tmp)
            list(success = NA,
                 failed = ids[i],
                 n = 0L)
          } else {
            list(success = ids[i],
                 failed = NA,
                 n = tmp)
          }

        }) # lapply

      # output
      if (verbose) message(" ")
      tmpinsert

    }, simplify = TRUE) # sapply tempFiles

  # prepare return values, n is successful only
  n <- sum(sapply(retimp, "[[", "n"), na.rm = TRUE)
  success <- sapply(retimp, "[[", "success")
  success <- as.character(na.omit(success))
  failed <- sapply(retimp, "[[", "failed")
  failed <- as.character(na.omit(failed))

  # return
  return(list(n = n,
              success = success,
              failed = failed))

} # end dbCTRLoadJSONFiles


#' dbQueryAnnotateRecords
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_update
#'
dbCTRAnnotateQueryRecords <- function(
  recordnumbers,
  annotation.text,
  annotation.mode,
  con,
  verbose) {

  # debug
  if (verbose) message("* Running dbCTRAnnotateQueryRecords...")
  if (verbose) message(recordnumbers)
  if (verbose) message(annotation.mode)

  # get any existing annotations
  annotations <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = paste0('{"_id": {"$ne": "meta-info"}}'),
    fields = '{"_id": 1, "annotation": 1}')

  # keep only those annotations that are to be modified
  annotations <- annotations[annotations[["_id"]] %in%
                               recordnumbers, ,
                             drop = FALSE]

  # check if dataframe is as expected: columns _id and annotation
  # dataframe could be empty if _ids not yet imported
  if (nrow(annotations) == 0) {
    annotations <- data.frame("_id" = recordnumbers,
                              "annotation" = "",
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  }

  # modify the annotations
  annotations[["annotation"]] <- trimws(
    switch(
      annotation.mode,
      "replace" = paste0(annotation.text),
      "prepend" = paste0(annotation.text, " ", annotations$annotation),
      paste0(annotations$annotation, " ", annotation.text)
    ))

  # ensure columns including order
  annotations <- annotations[, c("_id", "annotation")]

  # debug
  if (verbose) message(annotations)

  # update the database
  for (i in annotations[["_id"]]) {
    nodbi::docdb_update(
      src = con,
      key = con$collection,
      value = annotations[annotations[["_id"]] == i, ])
  }

  # inform user
  message("= Annotated retrieved records")

} # end dbCTRAnnotateQueryRecords


#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_delete docdb_create
#'
dbCTRUpdateQueryHistory <- function(
  register,
  queryterm,
  recordnumber,
  con,
  verbose) {

  ## check database connection
  con <- ctrDb(con = con)

  # debug
  if (verbose) message("Running dbCTRUpdateQueryHistory...")

  # retrieve existing history data
  hist <- suppressMessages(
    dbQueryHistory(con, verbose)
  )

  # debug
  if (verbose) message(hist)

  # append current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  hist <- rbind(
    hist,
    cbind(
      "query-timestamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "query-register"  = register,
      "query-records"   = recordnumber,
      "query-term"      = queryterm),
    stringsAsFactors = FALSE)


  # debug
  if (verbose) message(hist)

  # update database
  tmp <- nodbi::docdb_delete(
    src = con,
    key = con$collection,
    query = '{"_id": {"$eq": "meta-info"}}')

  # transform into array and encapsulate in element meta-info
  tmp <- jsonlite::toJSON(hist)
  tmp <- paste0('{"queries": ',  as.character(tmp), "}")

  # create history
  tmp <- nodbi::docdb_create(
    src = con,
    key = con$collection,
    value = data.frame("_id" = "meta-info",
                       "json" = tmp,
                       stringsAsFactors = FALSE,
                       check.names = FALSE))

  # inform user
  message('* Updated history in meta-info of "',
          con$collection, '"')

}
# end dbCTRUpdateQueryHistory


#' ctrLoadQueryIntoDbCtgov
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
#' @importFrom httr content headers progress write_disk GET HEAD
#' @importFrom nodbi docdb_query
#'
ctrLoadQueryIntoDbCtgov <- function(
  queryterm = queryterm,
  register,
  euctrresults,
  euctrresultshistory,
  euctrresultspdfpath,
  annotation.text,
  annotation.mode,
  parallelretrievals,
  only.count,
  con,
  verbose,
  queryupdateterm) {

  ## sanity correction for naked terms
  #
  # test cases:
  # queryterm = c("cancer&age=adult",                      # correct
  #               "cancer",                                # correct
  #               "cancer&age=adult&phase=0",              # correct
  #               "cancer&age=adult&phase=1&results=true", # correct
  #               "&age=adult&phase=1&abc=xyz&cancer&results=true", # correct
  #               "age=adult&cancer",                      # correct
  #               "2010-024264-18",                        # correct
  #               "NCT1234567890",                         # correct
  #               "term=cancer&age=adult",                 # keep
  #               "age=adult&term=cancer")                 # keep
  queryterm <- sub(
    "(^|&|[&]?\\w+=\\w+&)(\\w+|[NCT0-9-]+)($|&\\w+=\\w+)",
    "\\1term=\\2\\3",
    queryterm)

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)

  # CTGOV standard identifiers
  # updated 2017-07 with revised ctgov website links, e.g.
  # "https://clinicaltrials.gov/ct2/results/download_studies?
  # rslt=With&cond=Neuroblastoma&age=0&draw=3"
  queryUSRoot   <- "https://clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download_studies?"
  queryUSType2  <- "ct2/results?"

  ## deal with special cases
  # ctgov queryterm is just NCT number, prefix with variable name
  if (grepl("^NCT[0-9]{8}$", queryterm)) {
    queryterm <- paste0("term=", queryterm)
  }
  #
  # ctgov queryterm is just text, prefix with variable name
  if (!grepl("=", queryterm)) {
    queryterm <- paste0("term=", queryterm)
  }

  ## inform user and prepare url for downloading
  message("(1/3) Checking trials in CTGOV:")
  ctgovdownloadcsvurl <- paste0(
    queryUSRoot, queryUSType1, "&", queryterm, queryupdateterm)
  #
  if (verbose) message("DEBUG: ", ctgovdownloadcsvurl)

  # check number of trials to be downloaded
  ctgovdfirstpageurl <- paste0(
    queryUSRoot, queryUSType2, "&", queryterm, queryupdateterm)
  #
  tmp <- try(httr::content(httr::GET(
    url = utils::URLencode(ctgovdfirstpageurl)), as = "text"),
    silent = TRUE)
  #
  if (inherits(tmp, "try-error")) {
    stop("Host ", queryUSRoot, " does not respond, cannot continue.",
         call. = FALSE)
  }
  #
  tmp <- gsub("\n|\t|\r", " ", tmp)
  tmp <- gsub("<.*?>", " ", tmp)
  tmp <- gsub("  +", " ", tmp)
  tmp <- sub(".* (.*?) Stud(y|ies) found for.*", "\\1", tmp)

  # safeguard against no or unintended large numbers
  tmp <- suppressWarnings(as.integer(tmp))
  if (is.na(tmp) ||
      !length(tmp)) {
    message("No trials or number of trials could not be determined: ", tmp)
    return(invisible(list(n = 0L, ids = "")))
  }

  # inform user
  message("Retrieved overview, records of ", tmp, " ",
          "trial(s) are to be downloaded.")

  # only count?
  if (only.count) {

    # return
    return(list(n = tmp,
                success = NULL,
                failed = NULL))
  }

  # exit if too many records
  if (as.integer(tmp) > 10000L) {
    stop("These are ", tmp, " (more than 10,000) trials, this may be ",
         "unintended. Downloading more than 10,000 trials is not supported ",
         "by the register. Consider correcting or splitting queries.")
  }

  ## check database connection
  con <- ctrDb(con = con)

  ## system check, in analogy to onload.R
  message("Checking helper binaries: ", appendLF = FALSE)
  #
  if (!checkBinary(c("php", "phpxml", "phpjson"))) stop(
    "ctrLoadQueryIntoDb() cannot continue. ", call. = FALSE)
  # if (!suppressWarnings(
  #   installFindBinary(
  #     commandtest = "php --version"))) {
  #   stop("php not found, ctrLoadQueryIntoDb() will not work.",
  #        call. = FALSE)
  # }
  # #
  # if (!suppressWarnings(
  #   installFindBinary(
  #     commandtest = "php -r 'simplexml_load_string(\"\");'"))) {
  #   stop("php xml not found, ctrLoadQueryIntoDb() will not work.",
  #        call. = FALSE)
  # }
  # #
  # if (!suppressWarnings(
  #   installFindBinary(
  #     commandtest = "php -r 'json_encode(\"<foo>\");'"))) {
  #   stop("php json not found, ctrLoadQueryIntoDb() will not work.",
  #        call. = FALSE)
  # }
  #
  message("done.")

  # prepare a file handle for temporary directory
  f <- paste0(tempDir, "/", "ctgov.zip")

  # inform user
  message("Downloading trials ", appendLF = FALSE)

  # get (download) trials in single zip file f
  tmp <- httr::GET(
    url = utils::URLencode(ctgovdownloadcsvurl),
    httr::progress(),
    httr::write_disk(path = f,
                     overwrite = TRUE))

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    stop("No studies downloaded. Please check 'queryterm' or run ",
         "again with verbose = TRUE.", call. = FALSE)
  }

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## compose commands to transform xml into json, into
  # a single alltrials.json in the temporaray directory
  # special command handling on windows
  if (.Platform$OS.type == "windows") {
    #
    xml2json <- utils::shortPathName(
      path = system.file("exec/xml2json.php",
                         package = "ctrdata",
                         mustWork = TRUE))
    #
    xml2json <- paste0(
      "php -f ",
      shQuote(xml2json), " ",
      utils::shortPathName(path = tempDir))
    #
    # xml2json requires cygwin's php. transform paths for cygwin use:
    xml2json <- gsub("\\\\", "/", xml2json)
    xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
    #
    xml2json <- paste0(
      "cmd.exe /c ",
      rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
      ' --login -c "', xml2json, '"')
    #
  } else {
    #
    xml2json <- system.file("exec/xml2json.php",
                            package = "ctrdata",
                            mustWork = TRUE)
    #
    xml2json <- paste0(
      "php -f ",
      shQuote(xml2json), " ",
      tempDir)
    #
  } # if windows

  # run conversion of downloaded xml to json
  message("\n(2/3) Converting to JSON...")
  if (verbose) message("DEBUG: ", xml2json)
  imported <- system(xml2json, intern = TRUE)

  ## run import
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## add annotations
  if ((annotation.text != "") &
      (length(imported$success) > 0L)) {

    # dispatch
    dbCTRAnnotateQueryRecords(
      recordnumbers = imported$success,
      annotation.text = annotation.text,
      annotation.mode = annotation.mode,
      con = con,
      verbose = verbose)

  }

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s).")

  # clean up temporary directory
  if (!verbose) unlink(tempDir, recursive = TRUE)

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtgov



#' ctrLoadQueryIntoDbEuctr
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom httr content headers progress write_disk GET HEAD
#' @importFrom curl curl_fetch_multi multi_run new_pool
#' @importFrom nodbi docdb_query docdb_update
#'
ctrLoadQueryIntoDbEuctr <- function(
  queryterm = queryterm,
  register,
  euctrresults,
  euctrresultshistory,
  euctrresultspdfpath,
  annotation.text,
  annotation.mode,
  parallelretrievals,
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

  # create empty temporary directory on localhost for
  # download from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)

  # check results parameters
  if (is.null(euctrresultspdfpath)) {
    euctrresultspdfpath <- tempDir
  }
  if (euctrresults &&
      (is.na(file.info(euctrresultspdfpath)[["isdir"]]) ||
       !file.info(euctrresultspdfpath)[["isdir"]])) {
    warning("Invalid directory specified for 'euctrresultspdfpath': ",
            euctrresultspdfpath, call. = FALSE, immediate. = TRUE)
    euctrresultspdfpath <- tempDir
  }
  # canonical directory path
  euctrresultspdfpath <- normalizePath(euctrresultspdfpath, mustWork = TRUE)

  # inform user
  message("* Checking trials in EUCTR: ", appendLF = FALSE)

  # EUCTR standard identifiers
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
  resultsEuPages <- try(httr::GET(url = q), silent = TRUE)
  #
  if (inherits(resultsEuPages, "try-error")) {
    if (grepl("SSL certificate.*local issuer certificate", resultsEuPages)) {
      stop("Host ", queryEuRoot, " cannot be queried as expected, error:\n",
           trimws(resultsEuPages), "\nFor a potential workaround, check ",
           "https://github.com/rfhb/ctrdata/issues/19#issuecomment-820127139",
           call. = FALSE)
    } else {
    stop("Host ", queryEuRoot, " does not respond as expected, error:\n",
         resultsEuPages, call. = FALSE)
    }
  }
  # - store options from request
  requestOptions <- resultsEuPages$request$options
  # - get content of response
  resultsEuPages <- httr::content(resultsEuPages, as = "text")

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
    stop("ctrLoadQueryIntoDb(): register does not deliver ",
         "search results as expected, check if working with ",
         "'browseURL(\"", q, "\")'",
         call. = FALSE)
  }

  # calculate number of results pages
  resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20)

  # check for plausibility and stop function without erro
  if (is.na(resultsEuNumPages) ||
      is.na(resultsEuNumTrials) ||
      (resultsEuNumTrials == 0)) {
    message("First result page empty - no (new) trials found?")
    return(invisible(list(
      # return structure as in dbCTRLoadJSONFiles
      # which is handed through to ctrLoadQueryIntoDb
      n = 0L,
      success = character(0L),
      failed = character(0L),
      queryterm = queryterm)))
  }

  # inform user
  message("Retrieved overview, multiple records of ",
          resultsEuNumTrials, " trial(s) from ",
          resultsEuNumPages, " page(s) to be downloaded.")

  # only count?
  if (only.count) {

    # return
    return(list(n = resultsEuNumTrials,
                success = NULL,
                failed = NULL))
  }

  # suggest chunking if large number of trials found
  if (as.integer(resultsEuNumTrials) > 10000L) {
    stop("These are ", resultsEuNumTrials, " (more than 10,000) trials. ",
         "Consider correcting or splitting into separate queries.")
  }

  ## check database connection
  con <- ctrDb(con = con)

  ## system check, in analogy to onload.R
  message("Checking helper binaries: ", appendLF = FALSE)
  if (!checkBinary(c("sed", "perl"))) stop(
    "ctrLoadQueryIntoDb() cannot continue. ", call. = FALSE)
  if (euctrresults && !checkBinary(c("php", "phpxml", "phpjson"))) stop(
    "ctrLoadQueryIntoDb() cannot continue. ", call. = FALSE)
  message("done.")

  ## download all text files from pages

  # inform user
  message("(1/3) Downloading trials (max. ",
          parallelretrievals, " page[s] in parallel)...")

  # prepare download and saving

  # prepare curl operations
  #
  # - make handle work with cookies
  cf <- tempfile(
    pattern = "cookies_",
    fileext = ".txt")
  # - new handle
  h <- curl::new_handle(
    useragent = "",
    accept_encoding = "gzip,deflate,zstd,br",
    cookiejar = cf,
    cookiefile = cf)
  # - add any user options specified for httr
  curl::handle_setopt(h, .list = requestOptions)
  # - do fetch
  tmp <- curl::curl_fetch_memory(
    url = paste0(queryEuRoot, queryEuType3,
                 "query=2008-003606-33", "&page=1", queryEuPost),
    handle = h)
  # inform user about capabilities
  stime <- curl::handle_data(h)[["times"]][["total"]]
  sgzip <- curl::parse_headers(tmp$headers)
  sgzip <- sgzip[grepl("Transfer-Encoding", sgzip)]
  sgzip <- grepl("gzip|deflate", sgzip)
  if (length(sgzip) && !sgzip) {
    message("Note: register server cannot compress data, ",
            "transfer takes longer, about ", signif(stime, digits = 1),
            "s per trial")
  }

  # create pool for concurrent connections
  pool <- curl::new_pool(
    total_con = parallelretrievals,
    host_con = parallelretrievals,
    multiplex = TRUE)

  # generate vector with URLs of all pages
  urls <- vapply(
    paste0(queryEuRoot, queryEuType3,
           queryterm, "&page=", 1:resultsEuNumPages, queryEuPost),
    utils::URLencode, character(1L))

  # generate vector with file names for saving pages
  fp <- tempfile(
    pattern = paste0(
      "euctr_trials_",
      formatC(seq_len(resultsEuNumPages),
              digits = 0L,
              width = nchar(resultsEuNumPages),
              flag = 0), "_"),
    tmpdir = tempDir,
    fileext = ".txt"
  )

  # success handling: saving to file
  # and progress indicator function
  pc <- 0L
  curlSuccess <- function(res) {
    pc <<- pc + 1L
    # note that the number in euctr_trials_nnn
    # is not expected to correspond to the page
    # number in the URL that was downloaded
    # save to file
    if (res$status_code == 200L) {
      writeLines(text = rawToChar(res$content), con = fp[pc], useBytes = TRUE)
    }
    # inform user
    message("Pages: ", pc, " done, ",
            length(curl::multi_fdset(pool = pool)[["reads"]]), " ongoing   ",
            "\r", appendLF = FALSE)
  }

  # add randomised URLs and file names into pool
  tmp <- lapply(
    sample(seq_along(urls),
           size = length(urls),
           replace = FALSE,
           prob = NULL),
    function(u) {
      h <- curl::new_handle()
      curl::handle_setopt(h, .list = requestOptions)
      curl::curl_fetch_multi(
        url = urls[u],
        done = curlSuccess,
        pool = pool,
        handle = h)
    })

  # inform user on first page
  message("Pages: 0 done...\r", appendLF = FALSE)

  # do download and saving
  tmp <- curl::multi_run(
    pool = pool)

  # check plausibility
  if (inherits(tmp, "try-error")) {
    stop("Download from EUCTR failed; last error: ",
         class(tmp), call. = FALSE)
  }
  if (tmp[["success"]] != resultsEuNumPages) {
    stop("Download from EUCTR failed; incorrect number of records.",
         call. = FALSE)
  }

  ## compose commands: for external script on
  # all files in temporary directory and for import
  # special handling in case of windows
  if (.Platform$OS.type == "windows") {
    #
    euctr2json <- utils::shortPathName(
      path = system.file("exec/euctr2json.sh",
                         package = "ctrdata",
                         mustWork = TRUE))
    #
    euctr2json <- paste0(
      shQuote(euctr2json), " ",
      utils::shortPathName(path = tempDir))
    #
    # euctr2json requires cygwin's perl, sed
    # transform paths for cygwin use
    euctr2json <- gsub("\\\\", "/", euctr2json)
    euctr2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", euctr2json)
    euctr2json <- paste0(
      "cmd.exe /c ",
      rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
      ' --login -c "', euctr2json, '"')
    #
  } else {
    #
    euctr2json <- system.file(
      "exec/euctr2json.sh",
      package = "ctrdata",
      mustWork = TRUE)
    #
    euctr2json <- paste0(
      shQuote(euctr2json), " ",
      tempDir)
    #
  } # if windows

  # run conversion of text files saved
  # into file system to json file
  message("\n(2/3) Converting to JSON...")
  if (verbose) message("DEBUG: ", euctr2json)
  imported <- system(euctr2json, intern = TRUE)

  # run import into mongo from json files
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## read in the eudract numbers of the
  ## trials just retrieved and imported
  eudractnumbersimported <- imported$success

  ## add annotations
  if ((annotation.text != "") &
      (length(eudractnumbersimported) > 0)) {

    # dispatch
    dbCTRAnnotateQueryRecords(
      recordnumbers = eudractnumbersimported,
      annotation.text = annotation.text,
      annotation.mode = annotation.mode,
      con = con,
      verbose = verbose)
  }

  ## inform user on final import outcome
  message("= Imported or updated ",
          imported$n, " records on ",
          resultsEuNumTrials, " trial(s).")

  ## results: load also euctr trials results if requested
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
    message("* Retrieve results if available from EUCTR for ",
            length(eudractnumbersimported), " trials: ")

    ## parallel download and unzipping into temporary directory

    # "https://www.clinicaltrialsregister.eu/ctr-search/rest/
    # download/result/zip/xml/..."
    # first version:  "2007-000371-42/1"
    # second version: "2007-000371-42/2"
    # latest version: "2007-000371-42"

    # calculate batches to get data from all results pages
    resultsNumBatches <- length(
      eudractnumbersimported) %/% parallelretrievals
    #
    resultsNumModulo  <- length(
      eudractnumbersimported) %%  parallelretrievals

    # inform user
    message("(1/4) Downloading results (max. ",
            parallelretrievals,
            " trials in parallel):",
            appendLF = FALSE)

    # iterate over batches of results
    for (i in 1:(resultsNumBatches +
                 ifelse(resultsNumModulo > 0, 1, 0))) {

      # calculated indices for eudractnumbersimported vector
      startindex <- (i - 1) * parallelretrievals + 1
      stopindex  <- ifelse(
        i > resultsNumBatches,
        startindex + resultsNumModulo,
        startindex + parallelretrievals) - 1

      # inform user
      message("\n t ", startindex, "-",
              stopindex, " ", appendLF = FALSE)

      # prepare download and save
      pool <- curl::new_pool(
        total_con = parallelretrievals,
        host_con = parallelretrievals,
        multiplex = TRUE)
      #
      urls <- vapply(
        paste0(queryEuRoot, queryEuType4,
               eudractnumbersimported[startindex:stopindex]),
        utils::URLencode, character(1L), USE.NAMES = FALSE)
      #
      fp <- tempfile(
        pattern = paste0(
          "euctr_results_",
          startindex:stopindex, "_"),
        tmpdir = tempDir,
        fileext = ".zip"
      )
      #
      # success handling: saving to file
      # and progress indicator function
      pc <- 0
      curlSuccess <- function(res) {
        pc <<- pc + 1
        # save to file
        if (res$status_code == 200L) {
          writeBin(object = res$content, con = fp[pc])
        }}
      #
      tmp <- lapply(
        seq_along(urls),
        function(i) {
          h <- curl::new_handle()
          curl::handle_setopt(h, .list = requestOptions)
          curl::curl_fetch_multi(
            url = urls[i],
            done = curlSuccess,
            pool = pool,
            handle = h)
        })

      # do download and save
      tmp <- curl::multi_run(
        pool = pool)

      # unzip downloaded file and rename
      tmp <- lapply(
        fp, function(f) {

          if (file.exists(f) &&
              file.size(f) != 0L) {

            tmp <- utils::unzip(
              zipfile = f,
              exdir = tempDir)
            # results in files such as
            # EU-CTR 2008-003606-33 v1 - Results.xml

            if (any(grepl("pdf$", tmp))) {
              message("PDF ", appendLF = FALSE)
              if (euctrresultspdfpath != tempDir) {
                euctrnr <- gsub(paste0(".*(", regEuctr, ").*"),
                                "\\1", tmp[!grepl("pdf$", tmp)])
                # move PDF file(s) to user specified directory
                saved <- try(file.rename(
                  from = tmp[grepl("pdf$", tmp)],
                  to = normalizePath(
                    paste0(euctrresultspdfpath, "/",
                           euctrnr, "--",
                           basename(tmp[grepl("pdf$", tmp)])
                    ), mustWork = FALSE)), silent = TRUE)
                if (any(!saved)) {
                  warning("Could not save ", tmp[!saved],
                          call. = FALSE, immediate. = TRUE)
                }
              } # if paths
            } # if any pdf

            # inform user
            message(". ", appendLF = FALSE)
          } else {
            # unsuccessful
            message("x ", appendLF = FALSE)
          }

          # clean up
          if (!verbose) unlink(f)

        }) # lapply fp

    } # iterate over batches of results

    ## use system commands to convert
    ## xml to json and to import json

    # compose command

    # special command handling on windows
    if (.Platform$OS.type == "windows") {
      #
      xml2json <- utils::shortPathName(
        path = system.file(
          "exec/xml2json_euctrresults.php",
          package = "ctrdata",
          mustWork = TRUE))
      #
      xml2json <- paste0(
        "php -f ",
        shQuote(xml2json), " ",
        utils::shortPathName(path = tempDir))
      #
      # xml2json requires cygwin's php. transform paths for cygwin use:
      xml2json <- gsub("\\\\", "/", xml2json)
      xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
      xml2json <- paste0(
        "cmd.exe /c ",
        rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
        ' --login -c "', xml2json, '"')
      #
    } else {
      #
      xml2json <- system.file(
        "exec/xml2json_euctrresults.php",
        package = "ctrdata",
        mustWork = TRUE)
      #
      xml2json <- paste0(
        "php -f ",
        shQuote(xml2json),
        " ", tempDir)
      #
    } # if windows

    # run conversion of downloaded xml to json
    message("\n(2/4) Converting to JSON...")
    if (verbose) message("DEBUG: ", xml2json)
    importedresults <- system(xml2json, intern = TRUE)

    # iterate over results files
    message("(3/4) Importing JSON into database...")
    importedresults <- sapply(
      # e.g., EU-CTR 2008-003606-33 v1 - Results.json
      dir(path = tempDir,
          pattern = "EU.*Results[.]json",
          full.names = TRUE),
      function(fileName) {

        # check file
        if (file.exists(fileName) &&
            file.size(fileName) > 0L) {

          # read contents
          tmpjson <- readChar(
            con = fileName,
            nchars = file.info(fileName)$size,
            useBytes = TRUE)

          # get eudract number
          # "{\"@attributes\":{\"eudractNumber\":\"2004-004386-15\",
          euctrnumber <- sub(
            '^\\{\"@attributes\":\\{\"eudractNumber\":\"([0-9]{4}-[0-9]{6}-[0-9]{2})\".*$',
            "\\1", tmpjson)
          if (!grepl("^[0-9]{4}-[0-9]{6}-[0-9]{2}$", euctrnumber)) {
            warning("No EudraCT number recognised in file ", fileName, call. = FALSE)
          }

          # update database with results
          tmp <- try({
            tmpnodbi <-
              nodbi::docdb_update(
                src = con,
                key = con$collection,
                value = data.frame(
                  "a2_eudract_number" = euctrnumber,
                  "json" = tmpjson,
                  stringsAsFactors = FALSE))

            max(tmpnodbi, na.rm = TRUE)
          },
          silent = TRUE)

          # inform user on failed trial
          if (inherits(tmp, "try-error")) {
            warning(paste0("Import into mongo failed for trial ", euctrnumber),
                    immediate. = TRUE)
            tmp <- 0
          }

        } else {

          # file did not exist
          tmp <- 0

        }

        # return for accumulating information
        return(tmp)

      }) # end batchresults

    # iterate over batches of result history from webpage
    if (euctrresultshistory) {

      # TODO this does not include the retrieval of information
      # about amendment to the study, as presented at the bottom
      # of the webpage for the respective trial results
      message("(4/4) Retrieving results history and importing ",
              "into database...", appendLF = FALSE)
      for (i in 1:(resultsNumBatches +
                   ifelse(resultsNumModulo > 0, 1, 0))) {

        # calculated indices for eudractnumbersimported vector
        startindex <- (i - 1) * parallelretrievals + 1
        stopindex  <- ifelse(
          i > resultsNumBatches,
          startindex + resultsNumModulo,
          startindex + parallelretrievals) - 1

        # inform user
        message("\n h ", startindex, "-", stopindex,
                " ", appendLF = FALSE)

        # prepare download and save
        pool <- curl::new_pool(
          total_con = parallelretrievals,
          host_con = parallelretrievals,
          multiplex = TRUE)
        #
        done <- function(res) {
          if (res$status_code == 206L) {
            retdat <<- c(retdat, list(res))
          }}

        # compose urls to access results page
        urls <- vapply(paste0(
          "https://www.clinicaltrialsregister.eu/ctr-search/trial/",
          eudractnumbersimported[startindex:stopindex], "/results"),
          utils::URLencode, character(1L))

        tmp <- lapply(
          seq_along(urls),
          function(i) {
            h <- curl::new_handle(
              url = urls[i],
              range = "0-30000", # only top of page needed
              accept_encoding = "identity")
            curl::handle_setopt(h, .list = requestOptions)
            curl::multi_add(
              handle = h,
              done = done,
              pool = pool)
          })

        # do download and save into batchresults
        # TODO preferably retdat is pre-allocated
        retdat <- list()
        tmp <- curl::multi_run(
          pool = pool)

        # process top of results pages
        batchresults <- lapply(
          retdat,
          function(i) rawToChar(i[["content"]]))

        # curl return sequence is not predictable
        # therefore recalculate eudract numbers
        eudractnumberscurled <- sapply(
          retdat, function(i) i[["url"]])
        #
        eudractnumberscurled <- sub(
          ".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*",
          "\\1", eudractnumberscurled)

        # for date time conversion
        lct <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME", "C")

        # extract information about results
        tmpFirstDate <- as.Date(
          vapply(batchresults, function(t) {
            trimws(sub(
              ".+First version publication date</div>.*?<div>(.+?)</div>.*",
              "\\1",
              ifelse(grepl(
                "First version publication date", t),
                t, "")))}, character(1L)),
          format = "%d %b %Y")

        # global end date is variably represented in euctr:
        # 'p_date_of_the_global_end_of_the_trial',
        # 'Global completion date' or 'Global end of trial date'
        # tmpEndDate <- as.Date(sapply(batchresults, function(x)
        #   trimws(sub(".*Global .+? date</div>.*?<div>(.*?)</div>.*", "\\1",
        #      ifelse(grepl("Global .+? date", x), x, "")))),
        #           format = "%d %b %Y")

        # reset date time
        Sys.setlocale("LC_TIME", lct)

        tmpChanges <- vapply(batchresults, function(t) {
          trimws(
            gsub("[ ]+", " ",
            gsub("[\n\r]", "",
            gsub("<[a-z/]+>", "",
            sub(".+Version creation reason.*?<td class=\"valueColumn\">(.+?)</td>.+",
                "\\1", ifelse(grepl("Version creation reason", t), t, ""))
            ))))},
          character(1L))

        # clean up large object
        rm(batchresults)

        # create data frame for apply
        resultHistory <- data.frame(
          eudractnumberscurled,
          tmpFirstDate,
          tmpChanges,
          stringsAsFactors = FALSE
        )

        tmp <- apply(
          resultHistory, 1,
          function(r) {
            r <- as.list(r)

            # check, add default, inform user
            if (r$tmpChanges == "") {

              message("x ", appendLF = FALSE)

            } else {

              # update record
              message(". ", appendLF = FALSE)
              nodbi::docdb_update(
                src = con,
                key = con$collection,
                value = data.frame(
                  "a2_eudract_number" = r$eudractnumberscurled,
                  "firstreceived_results_date" = as.character(r$tmpFirstDate),
                  "version_results_history" = r$tmpChanges,
                  stringsAsFactors = FALSE))
            }
          }) # apply resultsHistory

      } # for batch
    } else {

      message("(4/4) Retrieving results history: not done ",
              "(euctrresultshistory = FALSE).",
              appendLF = FALSE)

    } # if euctrresultshistory

    # sum up successful downloads
    importedresults <- sum(unlist(
      importedresults, use.names = FALSE), na.rm = TRUE)

    ## inform user on final import outcome
    message("\n= Imported or updated results for ",
            importedresults, " records for ",
            resultsEuNumTrials, " trial(s).")
    if (euctrresultspdfpath != tempDir) {
      message("= Results PDF files if any saved in '",
              euctrresultspdfpath, "'")
    }

  } # if euctrresults


  # clean up temporary directory
  if (!verbose) {
    unlink(tempDir,
           recursive = TRUE)
  }

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr
