### ctrdata package
### main functions

#' Load and store register trial information
#'
#' Retrieves or updates information on clinical trials from registers
#' and stores it in a collection in a database.
#' This is the main function of \link{ctrdata-package} for accessing
#' registers and loading trial information into a database collection,
#' even if from different queries or different registers.
#' The query details are stored in the database collection and can
#' be accessed using \link{dbQueryHistory}.
#' A previous query can be re-run, which replaces or adds trial
#' records. However, user annotations of trial records are kept.
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
#' either "EUCTR", "CTGOV" or "ISRCTN". Not needed
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
#' @return A list with elements
#' `n` (number of trial records newly imported or updated),
#' `success` (a vector of _id's of successfully loaded records),
#' `failed` (a vector of identifiers of records that failed to load)
#' and `queryterm` (the query term used).
#' The returned list has several attributes set
#' (database and collection name, as well as the query history
#' of this database collection).
#'
#' @examples
#' \dontrun{
#' dbc <- nodbi::src_sqlite(
#'   collection = "test"
#' )
#' # Retrieve protocol-related information on a
#' # single trial identified by EudraCT number
#' ctrLoadQueryIntoDb(
#'   queryterm = "2013-001291-38",
#'   con = dbc
#' )
#' # Retrieve information on ongoing interventional
#' # cancer trials involving children
#' ctrLoadQueryIntoDb(
#'   queryterm = "cancer&recr=Open&type=Intr&age=0",
#'   register = "CTGOV",
#'   con = dbc
#' )
#' }
#'
#' @export
#'
ctrLoadQueryIntoDb <- function(
  queryterm = NULL,
  register = "",
  querytoupdate = NULL,
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

  ## check params

  # - minimum information
  if (is.null(queryterm) && is.null(querytoupdate)) {
    stop("neither 'queryterm' nor 'querytoupdate' specified.")
  }

  # - parameters consistent
  if (!is.null(querytoupdate) && !is.null(queryterm)) {
    stop("only one of 'queryterm' and 'querytoupdate' should be ",
         "specified, cannot continue", call. = FALSE)
  }

  ## deduce queryterm and register

  # - if not querytoupdate
  if (is.null(querytoupdate)) {

    # check queryterm
    if (!is.data.frame(queryterm)) {

      # obtain url and register
      queryterm <- try(
        ctrGetQueryUrl(
          url = queryterm,
          register = register),
        silent = TRUE)

    }

    # - deal with data frame as returned from
    #   ctrQueryHistoryInDb and ctrGetQueryUrl
    if (!all(substr(names(queryterm), 1, 6) == "query-") ||
        !is.data.frame(queryterm)) {
      stop("'queryterm' does not seem to result from ctrQueryHistoryInDb() ",
           "or ctrGetQueryUrl(): ", deparse(queryterm), call. = FALSE)
    }

    # - process queryterm dataframe
    nr <- nrow(queryterm)
    if (nr > 1L) {
      warning(
        "Using last row of queryterm parameter",
        call. = FALSE, immediate. = TRUE)
    }
    register  <- queryterm[nr, "query-register", drop = TRUE]
    queryterm <- queryterm[nr, "query-term", drop = TRUE]

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
    if (grepl("[^.a-zA-Z0-9=?+&%_:\"/, -]",
              gsub("\\[", "", gsub("\\]", "", queryterm)))) {
      stop("Parameter 'queryterm' has unexpected characters: ",
           queryterm, ", expected are: a-zA-Z0-9=?+&%_-,.: []/\"",
           call. = FALSE)
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

  ## handle if we need to rerun previous query

  # rewrite parameters for running as update
  querytermoriginal <- queryterm
  if (!is.null(querytoupdate)) {
    #
    rerunparameters <- ctrRerunQuery(
      querytoupdate = querytoupdate,
      forcetoupdate = forcetoupdate,
      con = con,
      verbose = verbose,
      queryupdateterm = queryupdateterm)
    #
    # set main parameters
    querytermoriginal <- rerunparameters$querytermoriginal
    queryupdateterm   <- rerunparameters$queryupdateterm
    queryterm         <- rerunparameters$queryterm
    register          <- rerunparameters$register
    failed            <- rerunparameters$failed
    #
    # early exit if ctrRerunQuery failed
    if (failed) return(invisible(emptyReturn))
    #
  } # if querytermtoupdate

  # set user agent for httr and curl to inform registers
  httr::set_config(httr::user_agent(
    paste0("ctrdata/", utils::packageDescription("ctrdata")$Version)))

  ## system check
  if (!only.count) {

    # check binaries
    message("Checking helper binaries: ", appendLF = FALSE)
    suppressMessages(installCygwinWindowsTest())
    if (register != "EUCTR") testBinaries <- c("php", "phpxml", "phpjson")
    if (register == "EUCTR") testBinaries <- c("sed", "perl")
    if (euctrresults) testBinaries <- c("sed", "perl", "php", "phpxml", "phpjson")
    if (!checkBinary(b = testBinaries)) stop(
      "ctrLoadQueryIntoDb() cannot continue. ", call. = FALSE)
    message("done")

    # check database connection
    con <- ctrDb(con = con)

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
    "EUCTR" = do.call(ctrLoadQueryIntoDbEuctr, params),
    "ISRCTN" = do.call(ctrLoadQueryIntoDbIsrctn, params)
  )

  # add annotations
  if ((annotation.text != "") &
      (length(imported$success) > 0L)) {

    # dispatch
    dbCTRAnnotateQueryRecords(
      recordnumbers = imported$success,
      recordannotations = imported$annotations,
      annotation.text = annotation.text,
      annotation.mode = annotation.mode,
      con = con,
      verbose = verbose)

  }

  # add query used for function
  imported <- c(
    imported[c("n", "success", "failed")],
    "queryterm" = querytermoriginal)

  ## finalise

  # only count?
  if (only.count) {
    # return number of trials
    return(imported)
  }

  # return some useful information or break
  if (!exists("imported") ||
      (imported$n == 0)) {
    message("Function did not result in any trial information imports")
    return(invisible(emptyReturn))
  }

  # inform user
  if (verbose) {
    message("DEBUG: \n'queryterm'=", queryterm,
            "\n'queryupdateterm'=", queryupdateterm,
            "\n'imported'=", imported$n,
            "\n'register'=", register,
            "\n'collection'=", con$collection)
  }

  # add query parameters to database
  dbCTRUpdateQueryHistory(register = register,
                          queryterm = querytermoriginal,
                          recordnumber = imported$n,
                          con = con,
                          verbose = verbose)

  # add metadata
  imported <- addMetaData(x = imported, con = con)

  ## return
  return(imported)

}
# end ctrLoadQueryIntoDb


#' ctrRerunQuery
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
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
  failed <- FALSE

  # get history
  rerunquery <- dbQueryHistory(con = con,
                               verbose = verbose)

  # check parameters
  if (is.null(rerunquery) || !nrow(rerunquery))
    stop("'querytoupdate': no previous queries found in collection, ",
         "aborting query update", call. = FALSE)

  # select last query if specified
  if (querytoupdate == "last")
    querytoupdate <- nrow(rerunquery)

  # check parameters
  if (!is.integer(querytoupdate))
    stop("'querytoupdate' needs to be an integer number", call. = FALSE)

  # try to select the query to be updated
  if (querytoupdate > nrow(rerunquery) ||
      querytoupdate < 1L) {
    stop("'querytoupdate': specified query number ", querytoupdate,
         " not found, check 'dbQueryHistory()'", call. = FALSE)
  }

  # set query values as retrieved
  queryterm  <- rerunquery[querytoupdate, "query-term", drop = TRUE]
  register   <- rerunquery[querytoupdate, "query-register", drop = TRUE]

  # when was this query last run?
  #
  # - dates of all the same queries
  initialday <- rerunquery[["query-timestamp"]][
    rerunquery[querytoupdate, "query-term", drop = TRUE] ==
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
    initialday <- rerunquery[querytoupdate, "query-timestamp", drop = TRUE]
  }

  # secondary check parameters
  if (!length(queryterm) || queryterm == "") {
    stop("Parameter 'queryterm' is empty - cannot update query ",
         querytoupdate, call. = FALSE)
  }
  #
  if (!any(register == registerList)) {
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
                "('&lup_'); running again with these limits",
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
    } # end ctgov

    # euctr
    if (register == "EUCTR") {

      # euctr: studies added or updated in the last 7 days:
      # "https://www.clinicaltrialsregister.eu/ctr-search/rest/feed/
      # bydates?query=cancer&age=children"

      # check if update request is in time window of the register (7 days)
      if (difftime(Sys.Date(), initialday, units = "days") > 7L) {
        #
        warning("'querytoupdate=", querytoupdate, "' not possible because ",
                "it was last run more than 7 days ago and the register ",
                "provides information on changes only for the last 7 days. ",
                "Reverting to normal download",
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
        if (verbose) message("DEBUG (rss url): ", rssquery)
        #
        resultsRss <- try(httr::content(
          httr::GET(url = rssquery),
          encoding = "UTF-8",
          as = "text"), silent = TRUE)

        # check plausibility
        if (inherits(resultsRss, "try-error")) {
          message("Download from EUCTR failed; last error: ", class(resultsRss))
          failed <- TRUE
        }

        # inform user
        if (verbose) message("DEBUG (rss content): ", resultsRss)
        #
        # attempt to extract euctr number(s)
        resultsRssTrials <- gregexpr(
          "eudract_number:[0-9]{4}-[0-9]{6}-[0-9]{2}</link>",
          resultsRss)[[1]]
        #
        if (length(resultsRssTrials) == 1L &&
            resultsRssTrials == -1L) {
          # inform user
          message("First result page empty - no (new) trials found?")
          failed <- TRUE
          #
        } else {
          # new trials found, construct
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
          if (verbose) message("DEBUG (rss trials): ", resultsRssTrials)
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
        }
        #
      }
    } # register euctr

    # isrctn
    if (register == "ISRCTN") {

      # isrctn last edited:
      # "&filters=condition:Cancer,
      #  GT+lastEdited:2021-04-01T00:00:00.000Z,
      #  LE+lastEdited:2021-04-25T00:00:00.000Z&"

      # if already in query term, just re-run full query to avoid
      # multiple queries in history that only differ in timestamp:
      if (grepl("lastEdited:", queryterm)) {
        #
        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ",
                "('lastEdited'); running again with these limits",
                immediate. = TRUE)
        #
      } else {
        #
        queryupdateterm <- strftime(
          strptime(initialday,
                   format = "%Y-%m-%d"),
          format = "%Y-%m-%d")
        #
        queryupdateterm <- paste0(" AND lastEdited GE ",
                                  queryupdateterm,
                                  "T00:00:00.000Z")
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
    } # end isrctn

  } # forcetoupdate

  ## return main parameters needed
  return(list(
    "querytermoriginal" = querytermoriginal,
    "queryupdateterm"   = queryupdateterm,
    "queryterm"         = queryterm,
    "register"          = register,
    "failed"            = failed))

} # end ctrRerunQuery





#' ctrConvertToJSON
#'
#' @param tempDir Name of temporary directory with downloaded
#'  trial information
#' @param scriptName Name of PHP or shell script to run
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return System messages from converting
#'
#' @keywords internal
#' @noRd
#'
ctrConvertToJSON <- function(tempDir, scriptName, verbose) {

  ## compose commands to transform into json
  scriptFile <- system.file(paste0("exec/", scriptName),
                            package = "ctrdata",
                            mustWork = TRUE)

  # special command handling on windows
  if (.Platform$OS.type == "windows") {
    #
    script2Json <- utils::shortPathName(path = scriptFile)
    #
    script2Json <- paste0(
      ifelse(grepl("[.]php$", scriptName), "php -f ", ""),
      shQuote(script2Json), " ",
      utils::shortPathName(path = tempDir))
    #
    # transform paths for cygwin use
    script2Json <- gsub("\\\\", "/", script2Json)
    script2Json <- gsub("([A-Z]):/", "/cygdrive/\\1/", script2Json)
    #
    script2Json <- paste0(
      rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
      ' --noprofile --norc --noediting -c ',
      shQuote(paste0(
        "PATH=/usr/local/bin:/usr/bin; ",
        script2Json)))
    #
  } else {
    #
    # platforms other than windows
    #
    script2Json <- system.file(paste0("exec/", scriptName),
                               package = "ctrdata",
                               mustWork = TRUE)
    #
    script2Json <- paste0(
      ifelse(grepl("[.]php$", scriptName), "php -f ", ""),
      shQuote(script2Json), " ",
      tempDir)
    #
  } # if windows

  # run conversion of download to json
  message("\n(2/3) Converting to JSON...", appendLF = FALSE)
  imported <- system(script2Json, intern = TRUE)
  message("\b\b\b, ", imported, " records converted")
  if (verbose) message("DEBUG: ", script2Json)

  # return
  return(imported)

}



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
#' @noRd
#'
dbCTRLoadJSONFiles <- function(dir, con, verbose) {

  # find files
  tempFiles <- dir(path = dir,
                   pattern = ".+_trials_.*.ndjson",
                   full.names = TRUE)

  # initialise counters
  fc <- length(tempFiles)

  # iterate over files
  retimp <- lapply(
    X = seq_along(tempFiles),
    function(tempFile) {

      ## initialise output
      idSuccess <- NULL
      idFailed <- NULL
      idAnnotation <- NULL
      nImported <- 0
      ids <- NULL
      annotations <- NULL

      ## get _id's

      # main function for fast reading,
      # switching off warning about final EOL missing
      fd <- file(description = tempFiles[tempFile],
                 open = "rt", blocking = TRUE)
      on.exit(try(close(fd), silent = TRUE), add = TRUE)

      # inform user
      message(
        "JSON file #: ", tempFile, " / ", fc,
        "                               \r",
        appendLF = FALSE)

      # get any annotations, delete
      # existing docs in chunks
      while (TRUE) {

        # read line
        tmpline <- readLines(con = fd, n = 1L, warn = FALSE)

        # exit while loop if empty
        if (length(tmpline) == 0L) break

        # readLines produces: \"_id\": \"2007-000371-42-FR\"
        id <- sub(".*_id\":[ ]*\"(.*?)\".*", "\\1", tmpline)

        # ids should always be found and later,
        # one id will be assumed to be on one line each
        if (length(id) == 1L && nchar(id)) ids <- c(ids, id)

      } # while

      # get annotations
      annoDf <- try({
        nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), ']}}'),
          fields = '{"_id": 1, "annotation": 1}')
      }, silent = TRUE)
      if (!inherits(annoDf, "try-error") &&
          length(annoDf[["_id"]])) {
        annotations <- merge(
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE),
          annoDf, all.x = TRUE
        )[["annotation"]]
      } else {
        annotations <- rep("", length(ids))
      }

      # delete any existing records
      deleteIds <- try({
        nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), ']}}'),
          fields = '{"_id": 1}')
      }, silent = TRUE)
      if (!inherits(deleteIds, "try-error") &&
          length(deleteIds[["_id"]])) {
        nodbi::docdb_delete(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', deleteIds[["_id"]], '"', collapse = ","), ']}}'))
      }

      ## import
      tmp <- try({
        suppressMessages(
          nodbi::docdb_create(
            src = con,
            key = con$collection,
            value = tempFiles[tempFile]
          ))}, silent = TRUE)

      ## return values for lapply
      if (inherits(tmp, "try-error") || tmp == 0L) {
        idFailed <- c(idFailed, ids)
        warning(tempFiles[tempFile], ": ", tmp, call. = FALSE)
      } else {
        idSuccess <- c(idSuccess, ids)
        nImported <- nImported + tmp
        idAnnotation <- c(idAnnotation, annotations)
      }

      # close this file
      close(fd)

      # return values
      list(success = idSuccess,
           failed = idFailed,
           n = nImported,
           annotations = idAnnotation)

    }) # sapply tempFiles

  # prepare return values, n is successful only
  n <- sum(sapply(retimp, "[[", "n"), na.rm = TRUE)
  success <- as.vector(unlist(sapply(retimp, "[[", "success")))
  failed <- as.vector(unlist(sapply(retimp, "[[", "failed")))
  annotations <- as.vector(unlist(sapply(retimp, "[[", "annotations")))

  # return
  return(list(n = n,
              success = success,
              failed = failed,
              annotations = annotations))

} # end dbCTRLoadJSONFiles


#' dbQueryAnnotateRecords
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_update
#'
dbCTRAnnotateQueryRecords <- function(
  recordnumbers,
  recordannotations,
  annotation.text,
  annotation.mode,
  con,
  verbose) {

  # debug
  if (verbose) message("Annotating records...")
  if (verbose) message(recordnumbers)
  if (verbose) message(annotation.mode)

  # df from existing annotations
  if (is.null(recordannotations)) recordannotations <- ""
  annotations <- data.frame(
    "_id" = recordnumbers,
    "annotation" = recordannotations,
    stringsAsFactors = FALSE,
    check.names = FALSE)

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
  result <- 0L
  for (i in annotations[["_id"]]) {
    result <- result +
      nodbi::docdb_update(
        src = con,
        key = con$collection,
        value = annotations[annotations[["_id"]] == i, "annotation", drop = FALSE],
        query = paste0('{"_id":"', i, '"}'))
  }

  # inform user
  message("= Annotated retrieved records (", result, " records)")

} # end dbCTRAnnotateQueryRecords


#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
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

  # compose history entry from current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  newHist <- data.frame(
    "query-timestamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "query-register"  = register,
    "query-records"   = recordnumber,
    "query-term"      = queryterm,
    check.names = FALSE,
    stringsAsFactors = FALSE)

  # retrieve existing history data
  hist <- dbQueryHistory(con, verbose)

  # append current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  if (nrow(hist)) {

    newHist <- rbind(hist, newHist)
    newHist <- list("queries" = newHist)

    tmp <- suppressMessages(
      nodbi::docdb_update(
        src = con,
        key = con$collection,
        value = newHist,
        query = '{"_id": "meta-info"}'
      ))

  } else {

    # to list
    newHist <- list(list(
      "_id" = "meta-info",
      "queries" = newHist))

    # write new document
    tmp <- suppressMessages(
      nodbi::docdb_create(
        src = con,
        key = con$collection,
        value = newHist
      ))
  }

  # inform user
  if (tmp == 1L) {
    message('Updated history ("meta-info" in "', con$collection, '")')
  } else {
    warning('Could not update history ("meta-info" in "', con$collection,
            '")', call. = FALSE, immediate. = FALSE)
  }
}
# end dbCTRUpdateQueryHistory


#' ctrLoadQueryIntoDbCtgov
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom httr content headers progress write_disk GET HEAD status_code
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

  # CTGOV standard identifiers
  # updated 2017-07 with revised ctgov website links, e.g.
  # "https://clinicaltrials.gov/ct2/results/download_studies?
  # rslt=With&cond=Neuroblastoma&age=0&draw=3"
  queryUSRoot   <- "https://clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download_studies?"
  queryUSType2  <- "ct2/results?"

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
  tmp <- try(httr::GET(
    url = utils::URLencode(ctgovdfirstpageurl)),
    silent = TRUE)
  #
  if (inherits(tmp, "try-error") ||
      !any(httr::status_code(tmp) == c(200L, 404L))) {
    stop("Host ", queryUSRoot, " not working as expected, ",
         "cannot continue: ", tmp[[1]], call. = FALSE)
  }
  #
  tmp <- httr::content(tmp, as = "text")
  tmp <- gsub("\n|\t|\r", " ", tmp)
  tmp <- gsub("<.*?>", " ", tmp)
  tmp <- gsub("  +", " ", tmp)
  tmp <- sub(".* (.*?) Stud(y|ies) found for.*", "\\1", tmp)
  tmp <- sub("^No$", "0", tmp)

  # safeguard against no or unintended large numbers
  tmp <- suppressWarnings(as.integer(tmp))
  if (is.na(tmp) || !length(tmp) || !tmp) {
    message("Search result page empty - no (new) trials found?")
    return(invisible(emptyReturn))
  }

  # inform user
  message("Retrieved overview, records of ", tmp, " ",
          "trial(s) are to be downloaded")

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
         "by the register; consider correcting or splitting queries")
  }

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # prepare a file handle for temporary directory
  f <- paste0(tempDir, "/", "ctgov.zip")

  # inform user
  message("Downloading trials ", appendLF = FALSE)

  # get (download) trials in single zip file f
  tmp <- try(httr::GET(
    url = utils::URLencode(ctgovdownloadcsvurl),
    httr::progress(),
    httr::write_disk(path = f,
                     overwrite = TRUE)),
    silent = TRUE)

  # inform user, exit gracefully
  if (inherits(tmp, "try-error") ||
      !any(httr::status_code(tmp) == c(200L))) {
    message("Host ", queryUSRoot, " not working as expected, ",
            "cannot continue: ", tmp[[1]])
    return(invisible(emptyReturn))
  }

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    stop("No studies downloaded. Please check 'queryterm' or run ",
         "again with verbose = TRUE", call. = FALSE)
  }

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## run conversion
  ctrConvertToJSON(tempDir, "ctgov2ndjson.php", verbose)

  ## run import
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtgov



#' ctrLoadQueryIntoDbEuctr
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom httr content headers progress write_disk GET HEAD status_code
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

  # inform user
  message("(1/3) Checking trials in EUCTR:")

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
  if (inherits(resultsEuPages, "try-error") ||
      httr::status_code(resultsEuPages) != 200L) {
    if (grepl("SSL certificate.*local issuer certificate", resultsEuPages)) {
      stop("Host ", queryEuRoot, " cannot be queried as expected, error:\n",
           trimws(resultsEuPages), "\nFor a potential workaround, check ",
           "https://github.com/rfhb/ctrdata/issues/19#issuecomment-820127139",
           call. = FALSE)
    } else {
      message("Host ", queryEuRoot, " not working as expected, ",
              "cannot continue: ", tmp[[1]])
      return(invisible(emptyReturn))
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
          resultsEuNumPages, " page(s) to be downloaded")

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

  # create empty temporary directory on localhost for
  # download from register into temporary directory
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

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

  ## download all text files from pages

  # inform user
  message("Downloading trials (",
          min(parallelretrievals, resultsEuNumPages),
          " pages in parallel)...")

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
  tmp <- try(curl::multi_run(
    pool = pool), silent = TRUE)

  # check plausibility
  if (inherits(tmp, "try-error")) {
    message("Download from EUCTR failed; last error: ", class(tmp))
    return(invisible(emptyReturn))
  }
  if (tmp[["success"]] != resultsEuNumPages) {
    message("Download from EUCTR failed; incorrect number of records")
    return(invisible(emptyReturn))
  }

  ## run conversion
  ctrConvertToJSON(tempDir, "euctr2ndjson.sh", verbose)

  # run import into mongo from json files
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## inform user on final import outcome
  message("= Imported or updated ",
          imported$n, " records on ",
          resultsEuNumTrials, " trial(s)")

  ## read in the eudract numbers of the
  ## trials just retrieved and imported
  eudractnumbersimported <- imported$success

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
    message("Retrieving results if available from EUCTR for ",
            length(eudractnumbersimported), " trials: ")

    ## parallel download and unzipping into temporary directory

    # "https://www.clinicaltrialsregister.eu/ctr-search/rest/
    # download/result/zip/xml/..."
    # first version:  "2007-000371-42/1"
    # second version: "2007-000371-42/2"
    # latest version: "2007-000371-42"

    # inform user
    message("(1/4) Downloading results (max. ",
            parallelretrievals,
            " trials in parallel):")

    # prepare download and save
    pool <- curl::new_pool(
      total_con = parallelretrievals,
      host_con = parallelretrievals,
      multiplex = TRUE)
    #
    urls <- vapply(
      paste0(queryEuRoot, queryEuType4,
             eudractnumbersimported),
      utils::URLencode, character(1L), USE.NAMES = FALSE)
    #
    fp <- tempfile(
      pattern = paste0(
        "euctr_results_",
        formatC(seq_along(eudractnumbersimported),
                digits = 0L,
                width = nchar(length(eudractnumbersimported)),
                flag = 0), "_"),
      tmpdir = tempDir,
      fileext = ".zip"
    )
    #
    # success handling: saving to file
    # and progress indicator function
    pc <- 0L
    curlSuccess <- function(res) {
      pc <<- pc + 1L
      # save to file
      if (res$status_code == 200L) {
        writeBin(object = res$content, con = fp[pc])
        message("\r", pc, " downloaded", appendLF = FALSE)
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
    tmp <- try(curl::multi_run(
      pool = pool), silent = TRUE)

    # check plausibility
    if (inherits(tmp, "try-error")) {
      message("Download from EUCTR failed; last error: ", class(tmp))
      return(invisible(emptyReturn))
    }

    # new line
    message(", extracting ", appendLF = FALSE)

    # unzip downloaded file and rename any PDF files
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

    ## run conversion
    ctrConvertToJSON(tempDir, "euctr2ndjson_results.php", verbose)

    # iterate over results files
    message("(3/4) Importing JSON into database...")

    # import results data from json file
    importedresults <- sapply(
      # e.g., EU-CTR 2008-003606-33 v1 - Results.xml
      # was converted into EU_Results_1234.json
      dir(path = tempDir,
          pattern = "EU_Results_[0-9]+[.]ndjson",
          full.names = TRUE),
      function(fileName) {

        # initialise import counter
        nSuccess <- 0L

        # check file
        if (file.exists(fileName) &&
            file.size(fileName) > 0L) {

          # main function for fast reading,
          # switching off warning about final EOL missing
          fd <- file(description = fileName,
                     open = "rt", blocking = TRUE)

          # iterate over lines in fileName
          while (TRUE) {

            # read line
            tmpjson <- readLines(con = fd, n = 1L, warn = FALSE)

            # exit while loop if empty
            if (length(tmpjson) == 0L) break

            # get eudract number
            # "{\"@attributes\":{\"eudractNumber\":\"2004-004386-15\",
            euctrnumber <- sub(
              paste0('^\\{\"@attributes\":\\{\"eudractNumber\":\"(',
                     regEuctr, ')\".*$'), "\\1", tmpjson)
            if (!grepl(paste0("^", regEuctr, "$"), euctrnumber)) {
              warning("No EudraCT number recognised in file ",
                      fileName, call. = FALSE)
            }

            # update database with results
            tmp <- try({
              tmpnodbi <-
                nodbi::docdb_update(
                  src = con,
                  key = con$collection,
                  value = tmpjson,
                  query = paste0('{"a2_eudract_number":"', euctrnumber, '"}')
                )
              max(tmpnodbi, na.rm = TRUE)
            },
            silent = TRUE)

            # inform user on failed trial
            if (inherits(tmp, "try-error")) {
              warning(paste0("Import of results failed for trial ",
                             euctrnumber), immediate. = TRUE)
              tmp <- 0
            }

            # however output is number of trials updated
            nSuccess <- nSuccess + 1L

            # inform user on records
            message(nSuccess, " trials' records updated with results\r",
                    appendLF = FALSE)

          } # while

          # close this file
          close(fd)

        } # if file exists

        # return accumulated counts
        nSuccess

      }) # end importedresults

    # sum up successful downloads
    importedresults <- sum(unlist(
      importedresults, use.names = FALSE), na.rm = TRUE)

    # get result history from result webpage, section Results information
    importedresultshistory <- NULL
    if (euctrresultshistory) {

      # for date time conversion
      lct <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")

      # helper function
      extractResultsInformation <- function(t) {

        # get content of partial webpage
        wpText <- rawToChar(t[["content"]])
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
      message("(4/4) Retrieving results history (max. ",
              parallelretrievals, " in parallel):")

      # prepare download and save
      pool <- curl::new_pool(
        total_con = parallelretrievals,
        host_con = parallelretrievals,
        multiplex = TRUE)
      #
      pc <- 0L
      curlSuccess <- function(res) {
        pc <<- pc + 1L
        # incomplete data received 206L
        if (res$status_code == 206L) {
          retdat <<- c(retdat, list(extractResultsInformation(res)))
          message("\r", pc, " downloaded", appendLF = FALSE)
        }}

      # compose urls to access results page
      urls <- vapply(paste0(
        "https://www.clinicaltrialsregister.eu/ctr-search/trial/",
        eudractnumbersimported, "/results"),
        utils::URLencode, character(1L))
      # add urls to pool
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
            done = curlSuccess,
            pool = pool)
        })
      # do download and save into batchresults
      retdat <- list()
      tmp <- try(curl::multi_run(
        pool = pool), silent = TRUE)

      # check plausibility
      if (inherits(tmp, "try-error")) {
        message("Download from EUCTR failed; last error: ", class(tmp))
        return(invisible(emptyReturn))
      }

      # combine results
      resultHistory <- do.call(
        rbind,
        c(lapply(retdat, as.data.frame),
          stringsAsFactors = FALSE,
          make.row.names = FALSE))

      # apply to store in database
      message(", updating records ", appendLF = FALSE)
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
    if (euctrresultspdfpath != tempDir) {
      message("= Results PDF files if any saved in '",
              euctrresultspdfpath, "'")
    }

  } # if euctrresults

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr


#' ctrLoadQueryIntoDbIsrctn
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom httr content headers progress write_disk GET HEAD status_code
#' @importFrom nodbi docdb_query
#' @importFrom utils URLdecode
#'
ctrLoadQueryIntoDbIsrctn <- function(
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

  # ISRCTN translation to API v0.4 2021-02-04
  # - limit can be set to arbitrarily high number
  # - no pagination or batching
  # - internal means XML
  queryIsrctnRoot <- "https://www.isrctn.com/"
  queryIsrctnType1 <- "api/query/format/internal?limit="
  queryIsrctnType2 <- "api/query/format/internal?limit=1&"
  #
  # convert parameters from search queryterm such as
  # "q=neuroblastoma&filters=LE+lastEdited%3A2021-04-01T00%3A00%3A00.000Z"
  # into to api format such as
  # "q=neuroblastoma AND lastEdited LE 2021-03-28T00:00:00.000Z"
  # - ensure we can use text processing
  queryterm <- utils::URLdecode(queryterm)
  # - generate api terms
  apiterm <- queryterm
  apiterm <- sub("&filters=", ",", apiterm)
  apiterm <- strsplit(apiterm, ",")[[1]]
  # - remove naked q
  apiterm <- apiterm[!grepl("^q=$", apiterm)]
  # - translate "LE+lastEdited:2021-04-01T00:00:00.000Z"
  #   into      "lastEdited LE 2021-03-28T00:00:00.000Z"
  apiterm <- vapply(
    apiterm,
    function(a) sub("^(.*?)[+](.*?)[:](.*)$", "\\2 \\1 \\3", a),
    character(1L), USE.NAMES = FALSE)
  apiterm <- paste0(apiterm, collapse = " AND ")
  # - add q again if missing
  if (!grepl("^q=", apiterm)) apiterm <- paste0("q=", apiterm)
  # - inform user
  if (verbose) message("DEBUG: apiterm is ", apiterm)

  ## inform user and prepare url for downloading
  message("(1/3) Checking trials in ISRCTN:")

  # - check number of trials to be downloaded
  isrctnfirstpageurl <- paste0(
    queryIsrctnRoot, queryIsrctnType2, apiterm, queryupdateterm)
  #
  tmp <- try(suppressWarnings(
    xml2::read_xml(
      x = url(utils::URLencode(isrctnfirstpageurl)))),
    silent = TRUE)
  #
  if (inherits(tmp, "try-error")) {
    message("Host ", queryIsrctnRoot, " not working as expected, ",
            "cannot continue: ", tmp[[1]])
    return(invisible(emptyReturn))
  }
  #
  tmp <- try(xml2::xml_attr(tmp, "totalCount"), silent = TRUE)
  #
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
  message("Retrieved overview, records of ", tmp, " ",
          "trial(s) are to be downloaded")

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
         "by the register; consider correcting or splitting queries")
  }

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # prepare a file handle for temporary directory
  f <- paste0(tempDir, "/", "isrctn.xml")

  # inform user
  message("(1/3) Downloading trials ", appendLF = FALSE)

  # construct API call setting limit to number found above
  isrctndownloadurl <- paste0(
    queryIsrctnRoot, queryIsrctnType1, tmp, "&", apiterm, queryupdateterm)

  # get (download) trials in single file f
  tmp <- try(httr::GET(
    url = utils::URLencode(isrctndownloadurl),
    httr::progress(),
    httr::write_disk(path = f,
                     overwrite = TRUE)),
    silent = TRUE)

  # check plausibility
  if (inherits(tmp, "try-error")) {
    message("Download from EUCTR failed; last error: ", class(tmp))
    return(invisible(emptyReturn))
  }

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    message("No studies downloaded. Please check 'queryterm' ",
            " or run again with verbose = TRUE")
  }

  ## run conversion
  ctrConvertToJSON(tempDir, "isrctn2ndjson.php", verbose)

  ## run import
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbIsrctn
