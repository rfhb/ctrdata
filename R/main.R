### ctrdata package
### main functions

#' Load and store register trial information
#'
#' Retrieves information on clinical trials from registers
#' and stores it in a collection in a database. Main function
#' of \link{ctrdata} for accessing registers.
#' A collection can store trial information from different queries
#' or different registers. Query details are stored in the
#' collection and can be accessed using \link{dbQueryHistory}.
#' A previous query can be re-run, which replaces or adds trial
#' records while keeping any user annotations of trial records.
#'
#' @param queryterm Either a string with the full URL of a search
#' query in a register, or the data frame returned by the
#' \link{ctrGetQueryUrl} or the
#' \link{dbQueryHistory} functions, or, together with parameter
#' \code{register}, a string with query elements of a search URL.
#' The query details are recorded in the \code{collection} for
#' later use to update records.
#' For "CTIS", \code{queryterm} can be an empty string to obtain
#' all trial records. For automatically copying the user's
#' query of a register in a web browser to the clipboard, see
#' \ifelse{latex}{\out{\href{https://github.com/rfhb/ctrdata\#3-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://github.com/rfhb/ctrdata#3-script-to-automatically-copy-users-query-from-web-browser}{here}}
#'
#' @param register String with abbreviation of register to query,
#' either "EUCTR", "CTGOV", "ISRCTN" or "CTIS". Not needed
#' if \code{queryterm} provides a full URL to query results.
#'
#' @param querytoupdate Either the word "last", or the row number of
#' a query in the data frame returned by \link{dbQueryHistory} that
#' should be run to retrieve any new or update trial records since
#' this query was run the last time.
#' This parameter takes precedence over \code{queryterm}.
#' For "EUCTR", updates are available only for the last seven days;
#' the query is run again if more time has passed since it was
#' run last.
#' Does not work with "CTIS" at this time.
#'
#' @param forcetoupdate If \code{TRUE}, run again the query
#' given in \code{querytoupdate}, irrespective of when it was
#' run last. Default is \code{FALSE}.
#'
#' @param documents.path If this is a relative or absolute
#' path to a directory that exists or can be created,
#' save any documents into it that are directly available from
#' the register ("EUCTR", "CTGOV", "CTIS") such as PDFs on results,
#' analysis plans, spreadsheets, assessments or product information
#' Default is \code{NULL}, which disables saving documents.
#'
#' @param documents.regexp Regular expression, case insensitive,
#' to select documents, if saving documents is requested
#' (see \code{documents.path}).
#' If set to \code{NULL}, empty placeholder files are saved for
#' every document that could be saved. Default is
#' \code{"prot|sample|statist|sap_|p1ar|p2ars|ctaletter"}.
#' Used with "CTGOV" and "CTIS" (but not "EUCTR" for which all
#' available documents are saved, and not with "ISRCTN" which
#' does not hold documents).
#'
#' @param euctrresults If \code{TRUE}, also download available
#' results when retrieving and loading trials from EUCTR. This
#' slows down this function. (For "CTGOV" and "CTIS", all
#' available results are always retrieved and loaded into the
#' collection.)
#'
#' @param euctrresultshistory If \code{TRUE}, also download
#' available history of results publication in "EUCTR."
#' This is quite time-consuming. Default is \code{FALSE}.
#'
#' @param annotation.text Text to be including into the field
#' "annotation" in the records retrieved with the query
#' that is to be loaded into the collection.
#' The contents of the field "annotation" for a trial record
#' are preserved e.g. when running this function again and
#' loading a record of a with an annotation, see parameter
#' \code{annotation.mode}.
#'
#' @param annotation.mode One of "append" (default), "prepend"
#' or "replace" for new annotation.text with respect to any
#' existing annotation for the records retrieved with the query
#' that is to be loaded into the collection.
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
#' @param ... Do not use (capture deprecated parameters).
#'
#' @return A list with elements
#' `n` (number of trial records newly imported or updated),
#' `success` (a vector of _id's of successfully loaded records),
#' `failed` (a vector of identifiers of records that failed to load)
#' and `queryterm` (the query term used).
#' The returned list has several attributes (including database and
#' collection name, as well as the query history of this database
#' collection) to facilitate documentation.
#'
#' @examples
#' \dontrun{
#'
#' dbc <- nodbi::src_sqlite(collection = "my_collection")
#'
#' # Retrieve protocol- and results-related information
#' # on two specific trials identified by their EU number
#' ctrLoadQueryIntoDb(
#'   queryterm = "2005-001267-63+OR+2008-003606-33",
#'   register = "EUCTR",
#'   euctrresults = TRUE,
#'   con = dbc
#' )
#'
#' # Retrieve all information on about 2,000 ongoing
#' # interventional cancer trials involving children
#' # into the same collection as used before
#' ctrLoadQueryIntoDb(
#'   queryterm = "cancer&recr=Open&type=Intr&age=0",
#'   register = "CTGOV",
#'   con = dbc
#' )
#'
#' # Retrieve all information on more than 40 trials
#' # that are labelled as phase 3 and that mention
#' # either neuroblastoma or lymphoma from ISRCTN,
#' # into the same collection as used before
#' ctrLoadQueryIntoDb(
#'   queryterm = "https://www.isrctn.com/search?q=neuroblastoma+OR+lymphoma&filters=phase%3APhase+III",
#'   con = dbc
#' )
#'
#' # Retrieve all information on completed trials in CTIS
#' ctrLoadQueryIntoDb(
#'   queryterm = "https://euclinicaltrials.eu/app/#/search?status=Ended",
#'   con = dbc
#' )
#'
#' }
#'
#' @export
#'
#' @importFrom httr set_config user_agent
#' @importFrom utils packageDescription
#'
ctrLoadQueryIntoDb <- function(
  queryterm = NULL,
  register = "",
  querytoupdate = NULL,
  forcetoupdate = FALSE,
  euctrresults = FALSE,
  euctrresultshistory = FALSE,
  documents.path = NULL,
  documents.regexp = "prot|sample|statist|sap_|p1ar|p2ars|ctaletter",
  annotation.text = "",
  annotation.mode = "append",
  only.count = FALSE,
  con = NULL,
  verbose = FALSE, ...) {

  ## check params

  # - deprecated
  params <- list(...)
  if (!is.null(params$euctrresultspdfpath)) {
    warning("Parameter 'euctrresultspdfpath' is deprecated, ",
            "use 'documents.path'", call. = FALSE)
    documents.path <- params$euctrresultspdfpath
  }
  if (!is.null(params$euctrresultsfilespath)) {
    warning("Parameter 'euctrresultsfilespath' is deprecated, ",
            "use 'documents.path'", call. = FALSE)
    documents.path <- params$euctrresultsfilespath
  }
  if (!is.null(params$parallelretrievals)) {
    warning("Parameter 'parallelretrievals' is deprecated and ignored")
  }

  # - parameters consistent
  if (!is.null(querytoupdate) && !is.null(queryterm)) {
    stop("only one of 'queryterm' and 'querytoupdate' should be ",
         "specified, cannot continue", call. = FALSE)
  }

  ## obtain queryterm register --------------------------------------------

  # - if not querytoupdate
  if (is.null(querytoupdate)) {

    # check queryterm
    if (!is.data.frame(queryterm)) {

      # obtain url and register
      queryterm <- try(
        ctrGetQueryUrl(
          url = ifelse(is.null(queryterm), "", queryterm),
          register = register),
        silent = TRUE)

    }

    # - deal with data frame as returned from
    #   ctrQueryHistoryInDb and ctrGetQueryUrl
    if (!all(substr(names(queryterm), 1, 6) == "query-") ||
        !is.data.frame(queryterm)) {
      stop("'queryterm' does not seem to result from ctrQueryHistoryInDb() ",
           "or ctrGetQueryUrl(): ", queryterm, call. = FALSE)
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

    # check register
    if (length(register) != 1L ||
        !all(class(register) %in% "character") ||
        is.na(register)) {
      stop("'register' has to be a non-empty string: ",
           register, call. = FALSE)
    }

    # check queryterm
    if (register != "CTIS" &&
        (length(queryterm) != 1L ||
         !all(class(queryterm) %in% "character") ||
         is.na(queryterm) ||
         nchar(queryterm) == 0L)) {
      stop("'queryterm' has to be a non-empty string: ",
           deparse(queryterm), call. = FALSE)
    }

    ## sanity checks
    if (grepl(regQueryterm, gsub("\\[", "", gsub("\\]", "", queryterm)))) {
      stop("Parameter 'queryterm' has unexpected characters: ",
           queryterm, ", expected are: a-zA-Z0-9=?+&%_-,.#: []/\"",
           call. = FALSE)
    }

    # remove trailing or leading whitespace, line breaks
    queryterm <- gsub("^\\s+|\\s+$|\n|\r", "", queryterm)

  } # if not querytoupdate

  # check annotation parameters
  if (annotation.text != "" &&
      !any(annotation.mode == c("append", "prepend", "replace"))) {
    stop("'annotation.mode' incorrect", call. = FALSE)
  }

  # set user agent for httr and curl to inform registers
  httr::set_config(httr::user_agent(
    paste0(
      "ctrdata/", utils::packageDescription("ctrdata")$Version,
      " (https://cran.r-project.org/package=ctrdata)")))

  ## handle querytoupdate -----------------------------------------------------

  # initialise variable that is filled if an update is to be made
  queryupdateterm <- ""

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

  ## . checkbinaries ---------------------------------------------------------

  if (!only.count) {

    # check extra binaries as needed for register
    if (grepl("^(ISRCTN|CTGOV|EUCTR)$", register)) {
      message("Checking helper binaries: ", appendLF = FALSE)
      suppressMessages(installCygwinWindowsTest())
      if (register == "ISRCTN") testBinaries <- c("php", "phpxml", "phpjson")
      if (register == "CTGOV") testBinaries <- c("php", "phpxml", "phpjson")
      if (register == "EUCTR") testBinaries <- c("sed", "perl")
      if (euctrresults) testBinaries <- c("sed", "perl", "php", "phpxml", "phpjson")
      if (!checkBinary(b = testBinaries)) stop(
        "ctrLoadQueryIntoDb() cannot continue", call. = FALSE)
      message("done")
    }

    # check database connection
    con <- ctrDb(con)

  }

  ## . main function -----------------------------------------------------

  # parameters for core functions
  params <- list(queryterm = queryterm,
                 register = register,
                 euctrresults = euctrresults,
                 euctrresultshistory = euctrresultshistory,
                 documents.path = documents.path,
                 documents.regexp = documents.regexp,
                 annotation.text = annotation.text,
                 annotation.mode = annotation.mode,
                 only.count = only.count,
                 con = con,
                 verbose = verbose,
                 queryupdateterm = queryupdateterm)

  # call core functions
  imported <- switch(
    as.character(params$register),
    "CTGOV2" = do.call(ctrLoadQueryIntoDbCtgov2, params),
    "CTGOV" = do.call(ctrLoadQueryIntoDbCtgov, params),
    "EUCTR" = do.call(ctrLoadQueryIntoDbEuctr, params),
    "ISRCTN" = do.call(ctrLoadQueryIntoDbIsrctn, params),
    "CTIS" = do.call(ctrLoadQueryIntoDbCtis, params)
  )

  ## annotate records ---------------------------------------------------------

  # add annotations
  if ((annotation.text != "") &&
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

  # add query parameters to database
  if (imported$n > 0L || !is.null(querytoupdate)) {
    dbCTRUpdateQueryHistory(
      register = register,
      queryterm = querytermoriginal,
      recordnumber = imported$n,
      con = con,
      verbose = verbose)

    # add metadata
    imported <- addMetaData(x = imported, con = con)

  }

  # return some useful information or break
  if (imported$n == 0L) message(
    "Function did not result in any trial records having been imported")

  # inform user
  if (verbose) {
    message(
      "DEBUG: \n'queryterm'=", queryterm,
      "\n'queryupdateterm'=", queryupdateterm,
      "\n'imported'=", imported$n,
      "\n'register'=", register,
      "\n'collection'=", con$collection)
  }

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
  con <- ctrDb(con)

  ## prepare
  failed <- FALSE

  ## handle query history -----------------------------------------------------
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

  # mangle parameter only if not forcetoupdate,
  # which just returns parameters of original query
  if (!forcetoupdate) {

    # ctgov --------------------------------------------------------------------
    if (register == "CTGOV") {

      # ctgov:
      # specify any date - "lup_s/e" last update start / end:
      # https://classic.clinicaltrials.gov/ct2/results?term=&recr=&rslt=&type=Intr&cond=
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
                call. = FALSE, immediate. = TRUE)
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

    # ctgov2 -------------------------------------------------------------------
    if (register == "CTGOV2") {

      # ctgov:
      # specify last update start / end:
      # https://www.clinicaltrials.gov/search?cond=Cancer&lastUpdPost=2022-01-01_2023-12-31

      # if "lastUpdPost" is already in query term, just re-run full query to avoid
      # multiple queries in history that only differ in the timestamp:
      if (grepl("&lastUpdPost=[0-9]{2}", queryterm)) {
        #
        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ",
                "('&lastUpdPost'); running again with these limits",
                call. = FALSE, immediate. = TRUE)
        #
      } else {
        #
        queryupdateterm <- strftime(
          strptime(initialday,
                   format = "%Y-%m-%d"),
          format = "%Y-%m-%d")
        #
        queryupdateterm <- paste0("&lastUpdPost=", queryupdateterm, "_")
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

    # euctr -------------------------------------------------------------------
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
                "Reverting to normal download. ",
                call. = FALSE, immediate. = TRUE)
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
          stop("Download from EUCTR failed; last error: ",
               class(resultsRss), call. = FALSE)
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
          # only for EUCTR, update history here because
          # for EUCTR the query to be used with function
          # ctrLoadQueryIntoDb cannot be specified to only
          # query for updated trials;
          # unless technical failure of retrieval
          if (!failed) dbCTRUpdateQueryHistory(
            register = register,
            queryterm = queryterm,
            recordnumber = 0L,
            con = con,
            verbose = verbose)
          #
          # set indicator
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

    # isrctn ------------------------------------------------------------------
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

    # ctis ------------------------------------------------------------------
    if (register == "CTIS") {

      # https://euclinicaltrials.eu/ct-public-api-services/services/ct/rss?basicSearchInputAND=cancer
      # issues: returned data do not include trial identifiers, thus no efficient loading possible;
      # returned data include all trials found with search, not only those updated or added in last
      # seven days; timestamp is the same for every trial listed, corresponding to time when called.
      # checked from: 2023-04-22 to last: 2023-08-29

      warning("'querytoupdate=", querytoupdate, "' not possible because no ",
              "way to query CTIS for recent changes was found thus far ",
              "(last checked 2023-08-29). Reverting to normal download. ",
              call. = FALSE, immediate. = TRUE)

      message("Rerunning query: ", queryterm,
              "\nLast run: ", initialday)
    } # end ctis

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
#' @importFrom nodbi docdb_create
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
                   pattern = "^.+_trials_.*.ndjson$",
                   full.names = TRUE)

  # check
  if (!length(tempFiles)) stop("no .+_trials_.*.ndjson files found in ", dir)

  # initialise counters
  fc <- length(tempFiles)

  ## iterate ndjson files -----------------------------------------------------------------

  retimp <- lapply(
    X = seq_along(tempFiles),
    function(tempFile) {

      ## initialise output
      idSuccess <- NULL
      idFailed <- NULL
      idAnnotation <- NULL
      nImported <- 0
      ids <- NULL

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

      ## existing annotations -------------------------------------------------

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
      if (!inherits(annoDf, "try-error") && length(annoDf[["_id"]])) {
        annoDf <- merge(
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE),
          annoDf, all.x = TRUE) # only need input ids, do not need all.y
      } else {
        annoDf <-
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE)
      }
      if (is.null(annoDf[["annotation"]]))
        annoDf[["annotation"]] <- rep(NA, length(ids))

      ## delete and import ----------------------------------------------------

      # delete any existing records
      try({
        nodbi::docdb_delete(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), ']}}'),
          fields = '{"_id": 1}')
      }, silent = TRUE)

      ## import
      tmp <- try({
        suppressWarnings(
          suppressMessages(
            nodbi::docdb_create(
              src = con,
              key = con$collection,
              value = tempFiles[tempFile]
            )))}, silent = TRUE)

      ## return values for lapply
      if (inherits(tmp, "try-error") || tmp == 0L || tmp != nrow(annoDf)) {

        # step into line by line mode
        fdLines <- file(tempFiles[tempFile], open = "rt", blocking = TRUE)
        fLineOut <- tempfile(pattern = "tmpOneLine", tmpdir = dir, fileext = ".ndjson")
        fTmp <- NULL
        while (TRUE) {
          tmpOneLine <- readLines(con = fdLines, n = 1L, warn = FALSE)
          if (length(tmpOneLine) == 0L || !nchar(tmpOneLine)) break
          id <- sub(".*\"_id\":[ ]*\"(.*?)\".*", "\\1", tmpOneLine)
          cat(tmpOneLine, file = fLineOut)
          tmp <- suppressWarnings(suppressMessages(nodbi::docdb_create(
            src = con, key = con$collection, value = fLineOut)))
          nImported <- nImported + tmp
          if (tmp) idSuccess <- c(idSuccess, id)
          if (!tmp) idFailed <- c(idFailed, id)
          if (!tmp) warning("Failed to load: ", id, call. = FALSE)
          if (tmp) idAnnotation <- c(idAnnotation, annoDf[
            annoDf[["_id"]] == id, "annotation", drop = TRUE][1])
        }
        close(fdLines)

      } else {
        nImported <- nImported + tmp
        idSuccess <- c(idSuccess, annoDf[ , "_id", drop = TRUE])
        idAnnotation <- c(idAnnotation, annoDf[ , "annotation", drop = TRUE])
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
      "prepend" = paste0(annotation.text, " ", ifelse(
        is.na(annotations[["annotation"]]), "", annotations[["annotation"]])),
      paste0(ifelse(is.na(annotations[["annotation"]]), "", annotations[["annotation"]]),
             " ", annotation.text)
    ))

  # ensure columns including order
  annotations <- annotations[, c("_id", "annotation"), drop = FALSE]

  # debug
  if (verbose) message(annotations)

  # update the database
  result <- nodbi::docdb_update(
      src = con,
      key = con$collection,
      value = annotations,
      query = "")

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
#' @importFrom nodbi docdb_delete docdb_create docdb_update
#'
dbCTRUpdateQueryHistory <- function(
  register,
  queryterm,
  recordnumber,
  con,
  verbose) {

  ## check database connection
  con <- ctrDb(con)

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
#' @importFrom jsonlite toJSON stream_in
#' @importFrom httr content GET status_code config set_config
#' @importFrom nodbi docdb_query
#' @importFrom curl multi_download
#' @importFrom jqr jq jq_flags
#'
ctrLoadQueryIntoDbCtgov <- function(
  queryterm = queryterm,
  register,
  euctrresults,
  euctrresultshistory,
  documents.path,
  documents.regexp,
  annotation.text,
  annotation.mode,
  only.count,
  con,
  verbose,
  queryupdateterm) {

  ## ctgov api ----------------------------------------------------------------

  # CTGOV standard identifiers
  # updated 2017-07 with revised ctgov website links, e.g.
  # "https://classic.clinicaltrials.gov/ct2/results/download_studies?
  # rslt=With&cond=Neuroblastoma&age=0&draw=3"
  queryUSRoot   <- "https://classic.clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download_studies?"
  queryUSType2  <- "ct2/results?"

  ## inform user and prepare url for downloading
  message("* Checking trials in CTGOV classic...")
  ctgovdownloadcsvurl <- paste0(
    queryUSRoot, queryUSType1, "&", queryterm, queryupdateterm)
  #
  if (verbose) message("DEBUG: ", ctgovdownloadcsvurl)

  ## checks -------------------------------------------------------------------

  # set configuration option
  httr::set_config(httr::config(forbid_reuse = 1))

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
  tmp <- sub(".*[> ](.*?) Stud(y|ies) found for: .*", "\\1", tmp)
  tmp <- sub("^No$", "0", tmp)

  # safeguard against no or unintended large numbers
  tmp <- suppressWarnings(as.integer(tmp))
  if (is.na(tmp) || !length(tmp) || !tmp) {
    message("Search result page empty - no (new) trials found?")
    return(invisible(emptyReturn))
  }

  # inform user
  message("Retrieved overview, records of ", tmp, " ",
          "trial(s) are to be downloaded (estimate: ",
          format(tmp * 0.008, digits = 2), " MB)")

  # only count?
  if (only.count) {

    # return
    return(list(n = tmp,
                success = NULL,
                failed = NULL))
  }

  # exit if too many records
  if (tmp > 10000L) {
    stop("These are ", tmp, " (more than 10,000) trials, this may be ",
         "unintended. Downloading more than 10,000 trials may not be supported ",
         "by the register; consider correcting or splitting queries")
  }

  ## download -----------------------------------------------------------------

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # prepare a file handle for temporary directory
  f <- file.path(tempDir, "ctgov.zip")

  # inform user
  message("(1/3) Downloading trial file...")

  # get (download) trials in single zip file f
  tmp <- ctrMultiDownload(ctgovdownloadcsvurl, f)

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    stop("No studies downloaded. Please check 'queryterm' or run ",
         "again with verbose = TRUE", call. = FALSE)
  }

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## run conversion
  message("(2/3) Converting to JSON...", appendLF = FALSE)
  tmp <- ctrConvertToJSON(tempDir, "ctgov2ndjson.php", verbose)

  ## run import
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")


  ## documents -----------------------------------------------------------------

  ## save any documents
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
      message("Downloading documents into 'documents.path' = ", documents.path)

      # canonical directory path
      documents.path <- normalizePath(documents.path, mustWork = TRUE)
      if (createdDir) message("- Created directory ", documents.path)

      # get documents urls, file names
      fDocsOut <- file.path(tempDir, "ctgov_docs.ndjson")
      unlink(fDocsOut)
      for (f in dir(path = tempDir, pattern = "^ctgov_trials_[0-9]+[.]ndjson$", full.names = TRUE)) {
        cat(jqr::jq(
          file(f),
          ' { _id: ._id, docs: [ .provided_document_section.provided_document[].document_url ] } ',
          flags = jqr::jq_flags(pretty = FALSE)
        ),
        sep = "\n",
        file = fDocsOut,
        append = TRUE)
      }

      # create directory per trial
      dlFiles <- jsonlite::stream_in(file(fDocsOut), verbose = FALSE)
      invisible(sapply(
        dlFiles[["_id"]], function(i) {
          d <- file.path(documents.path, i)
          if (!dir.exists(d))
            dir.create(d, showWarnings = FALSE, recursive = TRUE)
        }))

      if (!nrow(dlFiles)) {

        message("No documents for downloading identified.")

      } else {

        # create data frame with file info
        dlFiles <- apply(dlFiles, 1, function(r) {
          data.frame(url = unlist(r[-1], use.names = TRUE), r[1],
                     check.names = FALSE, stringsAsFactors = FALSE)
        })
        dlFiles <- do.call(rbind, dlFiles)
        dlFiles$filename <- sub("^.+/(.+?)$", "\\1", dlFiles$url)
        dlFiles$destfile <- file.path(
          documents.path, dlFiles$`_id`, dlFiles$filename)
        dlFiles$exists <- file.exists(dlFiles$destfile) &
          file.size(dlFiles$destfile) > 10L

        if (is.null(documents.regexp)) {

          message("Creating empty document placeholders (max. ", nrow(dlFiles), ")")

          # create empty files
          tmp <-
            sapply(
              dlFiles$destfile,
              function(i) if (!file.exists(i))
                file.create(i, showWarnings = TRUE),
              USE.NAMES = FALSE)

          tmp <- sum(unlist(tmp), na.rm = TRUE)

        } else {

          message("Applying 'documents.regexp' to ",
                  nrow(dlFiles), " documents")
          dlFiles <- dlFiles[
            grepl(documents.regexp, dlFiles$filename, ignore.case = TRUE), ,
            drop = FALSE]

          # download and save
          message("Downloading ", nrow(dlFiles), " documents:")

          tmp <- ctrMultiDownload(dlFiles$url[!dlFiles$exists],
                                  dlFiles$destfile[!dlFiles$exists])

          if (!nrow(tmp)) tmp <- 0L else {

            # handle failures despite success is true
            invisible(sapply(
              tmp[tmp$status_code != 200L, "destfile", drop = TRUE], unlink
            ))

            tmp <- nrow(tmp[tmp$status_code == 200L, , drop = FALSE])

          }

        } # if documents.regexp

        message(sprintf(paste0(
          "Newly saved %i ",
          ifelse(is.null(documents.regexp), "placeholder ", ""),
          "document(s) for %i trial(s); ",
          "%i document(s) for %i trial(s) already existed in %s"),
          tmp,
          length(unique(dlFiles$`_id`)),
          sum(dlFiles$fileexists),
          length(unique(dlFiles$`_id`[dlFiles$fileexists])),
          documents.path
        ))

      } # if !nrow

    } # if documents.path available

  } # if documents.path

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
#' @importFrom httr content GET status_code config
#' @importFrom curl new_handle handle_data handle_setopt parse_headers new_pool
#' @importFrom curl curl_fetch_multi multi_run curl_fetch_memory multi_fdset
#' @importFrom curl multi_run multi_add multi_download
#' @importFrom nodbi docdb_query docdb_update
#'
ctrLoadQueryIntoDbEuctr <- function(
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
  resultsEuPages <- try(
    httr::GET(url = q),
    silent = TRUE)
  #
  if (inherits(resultsEuPages, "try-error") ||
      httr::status_code(resultsEuPages) != 200L) {
    if (grepl("SSL certificate.*local issuer certificate", resultsEuPages)) {
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
          resultsEuNumPages, " page(s) to be downloaded ",
          "(estimate: ", format(resultsEuNumTrials * 0.05, digits = 2), " MB)")

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

  # create empty temporary directory on localhost for
  # download from register into temporary directory
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # check results parameters
  if (is.null(documents.path)) {
    documents.path <- tempDir
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

  # inform user
  message("(1/3) Downloading trials...")

  # new handle
  h <- do.call(
    curl::new_handle,
    c(accept_encoding = "gzip,deflate,zstd,br",
      getOption("httr_config")[["options"]])
  )
  # test fetch
  tmp <- curl::curl_fetch_memory(
    url = paste0(
      queryEuRoot, queryEuType3,
      "query=2008-003606-33", "&page=1", queryEuPost),
    handle = h
  )
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

  # do download and saving
  tmp <- ctrMultiDownload(urls, fp)

  if (nrow(tmp) != resultsEuNumPages) {
    message("Download from EUCTR failed; incorrect number of records")
    return(invisible(emptyReturn))
  }

  ## run conversion
  message("(2/3) Converting to JSON...", appendLF = FALSE)
  ctrConvertToJSON(tempDir, "euctr2ndjson.sh", verbose)

  # run import into database from json files
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
    message("(1/4) Downloading and extracting results ",
            "(. = data, F = file[s] and data, x = none):")

    # prepare download and save

    # urls
    urls <- vapply(
      paste0(queryEuRoot, queryEuType4,
             eudractnumbersimported),
      utils::URLencode, character(1L), USE.NAMES = FALSE)

    # destfiles
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

    # do download and save
    tmp <- ctrMultiDownload(urls, fp)

    # work only on successful downloads
    tmp <- tmp[tmp[["status_code"]] == 200L, , drop = FALSE]

    # unzip downloaded files and move non-XML extracted files
    tmp <- lapply(
      tmp[["destfile"]], function(f) {

        if (file.exists(f) &&
            file.size(f) != 0L) {

          tmp <- utils::unzip(
            zipfile = f,
            exdir = tempDir)
          if (is.null(tmp)) return(NULL)

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
              saved <- try(file.rename(
                from = nonXmlFiles,
                to = paste0(
                  normalizePath(path = documents.path, mustWork = TRUE),
                  "/", euctrnr, "--", basename(nonXmlFiles)
                )), silent = TRUE)
              # 2023/10/11 Commenting this out as it causes an error with saved being an invalid argument to any()
              #if (any(!saved)) {
              #  warning("Could not save ", nonXmlFiles[!saved],
              #          call. = FALSE, immediate. = TRUE)
              #}
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

        # clean up
        if (!verbose) unlink(f)

      }) # lapply fp

    # line break
    message("", appendLF = TRUE)

    ## run conversion of XML files
    ctrConvertToJSON(tempDir, "euctr2ndjson_results.php", verbose)

    # iterate over results files
    message("(3/4) Importing JSON into database...")

    # initiate counter
    importedresults <- 0L

    # import results data from json file
    sapply(
      # e.g., EU-CTR 2008-003606-33 v1 - Results.xml
      # was converted into EU_Results_1234.json
      dir(path = tempDir,
          pattern = "^EU_Results_[0-9]+[.]ndjson$",
          full.names = TRUE),
      function(fileName) {

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
                  query = paste0('{"_id": {"$regex": "^', euctrnumber, '.+"}}')
                )
              max(tmpnodbi, na.rm = TRUE)
            },
            silent = TRUE)

            # inform user on failed trial
            if (inherits(tmp, "try-error")) {
              warning(paste0("Import of results failed for trial ",
                             euctrnumber), immediate. = TRUE)
              tmp <- 0L
            }

            # however output is number of trials updated
            importedresults <<- importedresults + 1L

            # inform user on records
            message(
              importedresults,
              " trials' records updated with results\r",
              appendLF = FALSE)

          } # while

          # close this file
          close(fd)

        } # if file exists

      }) # end import results

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
      message("(4/4) Retrieving results history:")

      # prepare download and save
      pool <- curl::new_pool(
        multiplex = TRUE)
      #
      pc <- 0L
      curlSuccess <- function(res) {
        pc <<- pc + 1L
        # incomplete data is 206L but some results pages are complete
        if (any(res$status_code == c(200L, 206L))) {
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
          curl::handle_setopt(h, .list = getOption("httr_config")[["options"]])
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
      if (inherits(tmp, "try-error") || tmp[["error"]] || !length(retdat)) {
        stop("Download from EUCTR failed; last error with one or more of:\n",
        paste0(urls, collapse = "\n"), call. = FALSE
        )
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
    if (documents.path != tempDir) {
      message("= documents saved in '", documents.path, "'")
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
#' @importFrom nodbi docdb_query
#' @importFrom utils URLdecode
#'
ctrLoadQueryIntoDbIsrctn <- function(
  queryterm = queryterm,
  register,
  euctrresults,
  euctrresultshistory,
  documents.path,
  documents.regexp,
  annotation.text,
  annotation.mode,
  only.count,
  con,
  verbose,
  queryupdateterm) {

  ## isrctn api ---------------------------------------------------------------

  # ISRCTN translation to API v0.4 2021-02-04
  # - limit can be set to arbitrarily high number
  # - no pagination or batching
  # - internal means XML
  queryIsrctnRoot <- "https://www.isrctn.com/"
  queryIsrctnType1 <- "api/query/format/internal?limit="
  queryIsrctnType2 <- "api/query/format/internal?limit=0&"
  #
  # convert parameters from search queryterm such as
  # "q=neuroblastoma+OR+lymphoma&filters=phase%3APhase+III%2CLE+lastEdited%3A2021-01-01"
  # "q=&filters=phase%3APhase+III%2CLE+lastEdited%3A2021-01-01"
  # into to api format such as
  # "q=(neuroblastoma OR lymphoma) AND phase:"Phase+III" AND lastEdited LE 2021-01-01T00:00:00.000Z"
  #
  # - ensure we can use text processing
  queryterm <- utils::URLdecode(queryterm)
  # - generate api terms
  apiterm <- queryterm
  apiterm <- sub("&filters=", ",", apiterm)
  apiterm <- strsplit(apiterm, ",")[[1]]
  # - remove naked q
  apiterm <- apiterm[!grepl("^q=$", apiterm)]
  # - translate "LE+lastEdited:2021-04-01"
  #   into      "lastEdited LE 2021-04-01T00:00:00.000Z"
  apiterm <- vapply(
    apiterm,
    function(a) sub("^(.*?)[+](.*?)[:](.*)$", "\\2 \\1 \\3", a),
    character(1L), USE.NAMES = FALSE)
  # - add time if date does not end with it
  apiterm <- vapply(
    apiterm,
    function(a) sub("(.+[0-9]{4}-[0-9]{2}-[0-9]{2})$", "\\1T00:00:00.000Z", a),
    character(1L), USE.NAMES = FALSE)
  #
  # - quote anything right of colon; this is an advanced search URL:
  #   https://www.isrctn.com/search?q=&filters=phase%3APhase+III
  #   which needs to be changed to phase:"Phase III", noting
  #   `+` is interpreted by the API as space, thus unchanged
  termstoquote <- grepl("[ +]", sub("^.*?[:](.+)$", "\\1", apiterm))
  apiterm[termstoquote] <- vapply(
    apiterm[termstoquote],
    function(a) sub("^(.*?)[:](.+)$", "\\1:\"\\2\"", a),
    character(1L), USE.NAMES = FALSE)
  # - put q in brackets to respect logical operators
  qtoquote <- grepl("^q=.+$", apiterm)
  apiterm[qtoquote] <- sub("^q=(.+)$", "q=(\\1)", apiterm[qtoquote])
  # - collapse
  apiterm <- paste0(apiterm, collapse = " AND ")
  # - add empty q if q is missing
  if (!grepl("^q=", apiterm)) apiterm <- paste0("q=", apiterm)
  # - inform user
  if (verbose) message("DEBUG: apiterm is ", apiterm)

  ## checks -------------------------------------------------------------------

  message("* Checking trials in ISRCTN...")

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
    stop("Host ", queryIsrctnRoot, " not working as expected, ",
         "cannot continue: ", tmp[[1]], call. = FALSE)
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
          "trial(s) are to be downloaded (estimate: ",
          format(tmp * 0.018, digits = 2), " MB)")

  # only count?
  if (only.count) {

    # return
    return(list(n = tmp,
                success = NULL,
                failed = NULL))
  }

  # exit if too many records
  if (tmp > 10000L) {
    stop("These are ", tmp, " (more than 10,000) trials, this may be ",
         "unintended. Downloading more than 10,000 trials may not be supported ",
         "by the register; consider correcting or splitting queries")
  }

  ## download -----------------------------------------------------------------

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # prepare a file handle for temporary directory
  f <- paste0(tempDir, "/", "isrctn.xml")

  # inform user
  message("(1/3) Downloading trial file... ")

  # construct API call setting limit to number found above
  isrctndownloadurl <- paste0(
    queryIsrctnRoot, queryIsrctnType1, tmp, "&", apiterm, queryupdateterm)

  # get (download) trials in single file f
  tmp <- ctrMultiDownload(isrctndownloadurl, f)

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    message("No studies downloaded. Please check 'queryterm' ",
            " or run again with verbose = TRUE")
  }

  ## run conversion
  message("(2/3) Converting to JSON...", appendLF = FALSE)
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


#' ctrLoadQueryIntoDbCtis
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jqr jq jq_flags
#' @importFrom utils read.table URLencode
#' @importFrom nodbi docdb_update
#' @importFrom jsonlite stream_in fromJSON
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed
#' @importFrom httr GET status_code content
#'
ctrLoadQueryIntoDbCtis <- function(
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

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directory
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  # register function to remove files after use for streaming
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)
  if (verbose) message("DEBUG: ", tempDir)

  ## output mangle helper -----------------------------------------------

  mangleText <- function(t) {

    stringi::stri_replace_all_fixed(str = t, pattern = "'", replacement = "")

  }

  ## ctis api -----------------------------------------------------------

  ctisEndpoints <- c(
    #
    # 1 trial overview - %s is for pagination
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup?&paging=%s,%s&sorting=+ctNumber&%s",
    #
    # 2-3 trial information - %s is ctNumber
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/%s/publicview", # partI and partsII
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/%s/publicevents", # serious breach, unexpected event, urgent safety measure, inspection outside EEA, temporary halt
    #
    # 4-8 additional information - %s is ctNumber
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/public/%s/summary/list",
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/public/%s/layperson/list",
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/public/%s/csr/list", # clinical study report
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/public/%s/cm/list", # corrective measures
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/public/%s/inspections/list",
    #
    # 9 trial information - %s is an application id
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/%s/publicEvaluation",
    #
    # 10 download files - %s is document url
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/public/download/%s",
    #
    # 11-18 documents - %s is entity identifier, lists
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/part1/%s/list", # appl auth
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/part2/%s/list", # appl auth
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/product-group-common/%s/list",
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/part1/%s/list?documentType=274", # p1 AR
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/part2/%s/list/?documentType=43", # auth letter
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/part2/%s/list/?documentType=42", # p2 ARs
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/rfi/%s/list", # rfis
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/notification/%s/list?documentType=101", # events
    "https://euclinicaltrials.eu/ct-public-api-services/services/document/cm/%s/list", # corrective measures
    #
    # sub 3 additional info public events - %s is id, %s is type of notification
    "https://euclinicaltrials.eu/ct-public-api-services/services/ct/%s/eventdetails?notificationType=%s"
    #
    # unclear or not publicly accessible
    # https://euclinicaltrials.eu/ct-public-api-services/services/document/part1/4433/list?documentType=93
    # https://euclinicaltrials.eu/ct-public-api-services/services/document/part1/4433/list?documentType=94
    # https://euclinicaltrials.eu/ct-public-api-services/services/document/part2/14808/list/?documentType=41
    # https://euclinicaltrials.eu/ct-public-api-services/services/document/considerationDoc/32137/list
    #
  )

  ## api_1: overviews ---------------------------------------------------------

  # this is for importing overview (recruitment, status etc.) into database
  message("* Checking trials in EUCTR...")

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  message("(1/5) Downloading trials list ", appendLF = FALSE)

  # prepare
  i <- 0L
  di <- 200L
  idsTrials <- NULL
  fTrialsNdjson <- file.path(tempDir, "ctis_add_1.ndjson")
  unlink(fTrialsNdjson)

  # need to iterate / paginate as total number cannot be determined
  while (TRUE) {

    # {"totalSize":299,"pageInfo":{"offset":200,"limit":200,"pageNumber":2}
    url <- sprintf(ctisEndpoints[1], i, di, queryterm)
    url <- utils::URLencode(url)
    trialsJson <- httr::GET(url)
    message(". ", appendLF = FALSE)

    # early exit
    if (httr::status_code(trialsJson) != 200L) {
      warning("Could not be retrieved, check 'queryterm' and / or 'register'. ",
              "\nAPI returned: ", httr::content(trialsJson),
              call. = FALSE
      )
      message("API call: ", url)
      return(emptyReturn)
    }

    # extract json
    trialsJson <- suppressMessages(
      httr::content(trialsJson, as = "text")
    )

    # get total size
    totalSize <- as.numeric(
      jqr::jq(trialsJson, " {name: .totalSize} | .[]")
    )

    # extract trial information
    # and convert to ndjson
    trialsJson <- jqr::jq(
      trialsJson,
      paste0(
        # extract trial records
        " .elements | .[] ",
        # add element _id
        '| .["_id"] = .ctNumber',
        # keep only standardised fields
        "| del(.id, .ctNumber, .product, .endPoint, .eudraCtInfo, .ctTitle,
             .eudraCtInfo, .primaryEndPoint, .sponsor, .conditions) "
      ),
      flags = jqr::jq_flags(pretty = FALSE)
    )

    # get ids
    idsTrialsBatch <- gsub(
      '"', "", as.character(
        jqr::jq(
          trialsJson,
          ' ."_id" '
        )))

    # check for any duplicates
    nonDuplicates <- !(idsTrialsBatch %in% idsTrials)
    idsTrials <- c(idsTrials, idsTrialsBatch[nonDuplicates])

    # sanitise
    trialsJson <- mangleText(trialsJson)

    # save and append to ndjson
    cat(
      trialsJson[nonDuplicates],
      sep = "\n",
      file = fTrialsNdjson,
      append = TRUE
    )

    # iterate or break
    if (totalSize < (i + di)) break

    # update batch parameters
    i <- i + di
  }

  # early exit
  if (!totalSize) {
    warning("No trials found, check 'queryterm' and 'register'")
    return(emptyReturn)
  }

  # duplicates?
  if (totalSize != length(idsTrials)) {
    warning("Overview retrieval resulted in duplicate ",
            "trial records, only first record was kept. ")
  }

  # inform user
  message("found ", length(idsTrials), " trials")

  # only count?
  if (only.count) {
    # return
    return(list(
      n = length(idsTrials),
      success = NULL,
      failed = NULL
    ))
  }

  ## api_2: partI, partsII ---------------------------------------------------

  # this is imported as the main data into the database

  message("(2/5) Downloading and processing part I and parts II... (",
          "estimate: ", length(idsTrials) * 0.15, " Mb)")

  urls <- sprintf(ctisEndpoints[2], idsTrials)

  fPartIPartsIIJson <- function(i) {
    file.path(tempDir, paste0("ctis_trial_partIpartsII_", i, ".json"))
  }

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  tmp <- ctrMultiDownload(urls, fPartIPartsIIJson(idsTrials))

  importString <- paste0(
    '"ctrname":"CTIS",\\1,"record_last_import":"',
    strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), '",')

  # convert partI and partsII details into ndjson file
  fPartIPartsIINdjson <- file.path(tempDir, "ctis_trials_partIpartsII_.ndjson")
  unlink(fPartIPartsIINdjson)

  for (fn in tmp[["destfile"]]) {
    if (!file.exists(fn)) next
    cat(
      # files include id, ctNumber and others repeatedly
      # only replace first instance for updating records
      # sanitise texts removing various quotation marks
      sub("(\"id\":[0-9]+),", importString,
          sub("(\"ctNumber\"):(\"[-0-9]+\"),", '\\1:\\2,"_id":\\2,',
              mangleText(readLines(fn, warn = FALSE))
          )),
      file = fPartIPartsIINdjson,
      append = TRUE,
      sep = "\n")
    message(". ", appendLF = FALSE)
  }

  # address and mangle "applications" which has (as only field in ctis)
  # '"partIIInfo": "<integer number>": {...}' by replacing with array
  # '{"partIIInfo": [{"partIIIinfoKey": <integer number>, ...}]}'
  ctisMangle <- list(
    # file in, file out, pattern, replacement
    c(fPartIPartsIINdjson, paste0(fPartIPartsIINdjson, "1"), '"([0-9]+)": ?[{]', '{"partIIIinfoKey":$1,'),
    c(paste0(fPartIPartsIINdjson, "1"), paste0(fPartIPartsIINdjson, "2"), '"partIIInfo":[{]', '"partIIInfo":['),
    c(paste0(fPartIPartsIINdjson, "2"), fPartIPartsIINdjson, '[}],"(reporting|decision)Date"', '],"$1Date"')
  )
  for (i in ctisMangle) {
    cat(stringi::stri_replace_all_regex(
      str = readLines(i[1], warn = FALSE),
      pattern = i[3], replacement = i[4]
    ), file = i[2], sep = "\n")
    message(". ", appendLF = FALSE)
  }

  ## api_3: public events ----------------------------------------------------

  # helper function
  publicEventsMerger <- function(publicEvents) {

    # get event types that have data with ids of events
    eventTypes <- jqr::jq(
      publicEvents,
      " to_entries[] | select(.value | length > 0) | ([.key] + (.value[] | [.id])) ")

    # loop over event type
    for (eventType in eventTypes) {

      # get ids
      ids <- jqr::jq(eventType, " .[] ")
      ids[1] <- gsub("\"", "", ids[1])

      # get data
      urls <- sprintf(ctisEndpoints[20], ids[2],
                      toupper(gsub("([A-Z])", "_\\1", gsub("List", "", ids[1]))))
      eventData <- httr::GET(urls)
      if (httr::status_code(eventData) != 200L) next
      eventData <- suppressMessages(httr::content(eventData, as = "text"))

      # update input json with event data
      eventData <- paste0(
        " .", ids[1], " |= map( [select( .id == ", ids[2], ") | .details = ",
        eventData, "], [select( .id != ", ids[2], ")] | select( . | length > 0) ) ")
      publicEvents <- jqr::jq(publicEvents, eventData)

    }

    # return updated publicEvents json
    return(publicEvents)

  }

  ## api_3-8: more data ----------------------------------------------------

  message("\n(3/5) Downloading and processing additional data:")

  for (e in 3:8) {

    urls <- sprintf(ctisEndpoints[e], idsTrials)
    ep <- sub(".+/(.+?)$", "\\1", sub("/list$", "", urls[1]))
    message(ep, ", ", appendLF = FALSE)

    fAddJson <- function(i) {
      file.path(tempDir, paste0("ctis_add_", e, "_", i, ".json"))
    }

    # "HTTP server doesn't seem to support byte ranges. Cannot resume."
    tmp <- ctrMultiDownload(urls, fAddJson(idsTrials), progress = FALSE)

    # convert into ndjson file
    fAddNdjson <- file.path(tempDir, paste0("ctis_add_", e, ".ndjson"))
    unlink(fAddNdjson)

    for (fi in seq_len(nrow(tmp))) {

      fn <- tmp[["destfile"]][fi]
      if (file.size(fn) < 150L) next # sizes 99, 100, 134 byte

      # get data
      jOut <- readLines(fn, warn = FALSE)

      # sanitise
      jOut <- mangleText(jOut)

      # remove irrelevant information
      jOut <- sub('^.*"elements":(.*?)}?$', "\\1", jOut)
      jOut <- sub('(,?)"showWarning":(false|true)(,?)', "\\3", jOut)
      jOut <- sub('(,?)"totalSize":[0-9]+(,?)', "\\2", jOut)
      jOut <- sub('(,?)"pageInfo":[{].+?[}](,?)', "\\2", jOut)
      if (!nchar(jOut) || jOut == "[]") next

      # if publicevents, obtain additional data
      if (e == 3L) jOut <- publicEventsMerger(jOut)

      # reconstruct trial id
      id <- sub(paste0(".+/(", regCtis, ")/.+"), "\\1", tmp[["url"]][fi])

      # write out into ndjson file
      cat(
        paste0(
          # use endpoint as name for top level element
          '{"_id":"', id, '","', ep, '":', jOut, '}'),
        file = fAddNdjson,
        append = TRUE,
        sep = "\n")

    }

  }

  ## add_9: publicevaluation -----------------------------------------------------

  message("publicevaluation")

  fApplicationsJson <- file.path(tempDir, "ctis_add_9.json")

  # get ids of trial applications
  jqr::jq(
    file(fPartIPartsIINdjson),
    ' { ctNumber: .ctNumber, applicationIds: [ .applications[] | .id ] } ',
    flags = jqr::jq_flags(pretty = FALSE),
    out = fApplicationsJson
  )

  idsApplications <- jsonlite::stream_in(file(fApplicationsJson), verbose = FALSE)

  dlFiles <- apply(idsApplications, 1, function(r) {
    data.frame(
      "_id" = unlist(r[1], use.names = TRUE), r[-1],
      check.names = FALSE, stringsAsFactors = FALSE,
      row.names = NULL, check.rows = FALSE)
  })

  dlFiles <- do.call(rbind, dlFiles)
  dlFiles$url <- sprintf(ctisEndpoints[9], dlFiles$applicationIds)
  dlFiles$filepathname <- file.path(
    tempDir, paste0("ctis_add_9_", dlFiles$applicationIds, ".json"))

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  tmp <- ctrMultiDownload(dlFiles$url, dlFiles$filepathname)

  fApplicationsNdjson <- file.path(tempDir, "ctis_add_9.ndjson")
  unlink(fApplicationsNdjson)

  for (i in seq_len(nrow(idsApplications))) {

    # read all files for _id into vector
    fApps <- file.path(tempDir, paste0(
      "ctis_add_9_", unlist(idsApplications$applicationIds[i]), ".json"))

    fApps <- fApps[file.size(fApps) >= 50L]
    fn <- tmp[["destfile"]][fi]
    if (!length(fApps)) next

    # get data
    jApps <- sapply(fApps, readLines, warn = FALSE, USE.NAMES = FALSE)
    if (!length(jApps)) next

    # sanitise
    jApps <- mangleText(jApps)

    # add applicationId
    jApps <- mapply(function(i, j) sub(
      "^[{]", paste0('{"id":', i, ","), j),
      unlist(idsApplications$applicationIds[i]),
      jApps)

    # compose array
    jApps <- paste0(
      '{"_id":"', idsApplications[["ctNumber"]][i],
      '","publicEvaluation":[',
      paste0(jApps, collapse = ","),
      ']}')

    # write ndjson
    cat(
      jApps,
      file = fApplicationsNdjson,
      append = TRUE,
      sep = "\n")

    message(i, rep("\b", nchar(i)), appendLF = FALSE)

  }

  ## database import -----------------------------------------------------

  message("\n(4/5) Importing JSON records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)

  # iterating over any additional ndjson files
  resAll <- NULL
  message("(5/5) Updating with additional data: ", appendLF = FALSE)

  for (f in dir(path = tempDir, pattern = "^ctis_add_[1-9]+[0-9]*.ndjson$", full.names = TRUE)) {

    message(". ", appendLF = FALSE)
    res <- nodbi::docdb_update(src = con, key = con$collection, query = "", value = f)
    resAll <- c(resAll, res)

  }
  message("")

  ## api_10-19: documents -------------------------------------------------------

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

      # define order for factor for sorting
      orderedParts <- c(
        "ctaletter", "p1ar", "p2ars", "part1auth", "part1appl",
        "parts2auth", "parts2appl", "prodauth", "prodappl", "rfis",
        "events", "cms")

      # 1 - get ids of lists (which include urls to download)
      message("- Getting ids of lists with document information")

      # get temporary file
      downloadsNdjson <- file.path(tempDir, "ctis_downloads.ndjson")

      # extract ids of lists per parts per trial
      jqr::jq(
        file(fPartIPartsIINdjson),
        ' ._id |= gsub("\\""; "") | { _id: ._id,
          part1appl: [ .applications[].partI.id ],
          part1auth: [ .authorizedPartI.id ],
          parts2appl: [ .applications[].partIIInfo[].id ],
          parts2auth: [ .authorizedPartsII[].id ],
          prodappl: [ .applications[].partI.productRoleGroupInfos[].id ],
          prodauth: [ .authorizedPartI.productRoleGroupInfos[].id ],
          p1ar: [ .applications[].partI.id ],
          p2ars: [ .applications[].partIIInfo[].id ],
          ctaletter: [ .applications[].partIIInfo[].id ]
        } ',
        flags = jqr::jq_flags(pretty = FALSE),
        out = downloadsNdjson
      )

      # extract ids of rfis from publicevaluation
      rfiIds1 <- jqr::jq(
        file(fApplicationsNdjson),
        '{ _id: ._id, rfis1: [ .publicEvaluation[].partIRfis[].id ]}')
      rfiIds1 <- jsonlite::fromJSON(paste0("[", paste0(rfiIds1, collapse = ","), "]"))
      rfiIds2 <- jqr::jq(
        file(fApplicationsNdjson),
        '{ _id: ._id, rfis2: [ .publicEvaluation[].partIIEvaluationList[].partIIRfis[].id ]}')
      rfiIds2 <- jsonlite::fromJSON(paste0("[", paste0(rfiIds2, collapse = ","), "]"))

      # extract ids of documents from publicEvents (ep = 3)
      if (file.exists(file.path(tempDir, "ctis_add_3.ndjson"))) {
        eventIds <- jqr::jq(file(file.path(tempDir, "ctis_add_3.ndjson")),
                            " {_id: ._id, events: [ .publicevents[][][].id ]}")
        eventIds <- jsonlite::fromJSON(paste0("[", paste0(eventIds, collapse = ","), "]"))
      } else {
        eventIds <- data.frame()
      }

      # extract ids of documents from corrective events (ep = 7)
      if (file.exists(file.path(tempDir, "ctis_add_7.ndjson"))) {
        cmIds <- jqr::jq(file(file.path(tempDir, "ctis_add_7.ndjson")),
                            " {_id: ._id, cms: [ .cm[].id ]}")
        cmIds <- jsonlite::fromJSON(paste0("[", paste0(cmIds, collapse = ","), "]"))
      } else {
        cmIds <- data.frame()
      }

      # convert and merge ids
      dlFiles <- jsonlite::stream_in(file(downloadsNdjson), verbose = FALSE)
      if (nrow(rfiIds1)) {dlFiles <- merge(dlFiles, rfiIds1, all.x = TRUE)}
      if (nrow(rfiIds2)) {dlFiles <- merge(dlFiles, rfiIds2, all.x = TRUE)}
      if (nrow(eventIds)) {dlFiles <- merge(dlFiles, eventIds, all.x = TRUE)}
      if (nrow(cmIds)) {dlFiles <- merge(dlFiles, cmIds, all.x = TRUE)}

      # map
      epTyp <- list(
        "part1" = ctisEndpoints[11],
        "parts2" = ctisEndpoints[12],
        "prod" = ctisEndpoints[13],
        "p1ar" = ctisEndpoints[14],
        "p2ars" = ctisEndpoints[15],
        "ctaletter" = ctisEndpoints[16],
        "rfis" = ctisEndpoints[17],
        "events" = ctisEndpoints[18],
        "cms" = ctisEndpoints[19]
      )

      dlFiles <- apply(dlFiles, 1, function(r) {
        tmp <- data.frame(id = unlist(r[-1], use.names = TRUE), r[1],
                          check.names = FALSE, stringsAsFactors = FALSE)
        # if url occurs repeatedly, only use last from defined order
        tmp$part <- sub("[0-9]+$", "", row.names(tmp))
        tmp$part <- ordered(tmp$part, orderedParts)
        tmp$typ <- sub("appl|auth", "", tmp$part)
        #
        tmp$url <- mapply(
          function(t, i) sprintf(epTyp[t][[1]], i), tmp$typ, tmp$id)
        #
        tmp <- tmp[order(tmp$url, tmp$part), , drop = FALSE]
        rl <- rle(tmp$url)
        rl <- unlist(sapply(rl$lengths, function(i) c(TRUE, rep(FALSE, i - 1L))))
        tmp[rl, , drop = FALSE]
      })

      dlFiles <- do.call(rbind, dlFiles)
      dlFiles <- na.omit(dlFiles)

      # do downloads of list files
      message("- Downloading ", nrow(dlFiles),
              " lists with document information (estimate: ",
              nrow(dlFiles) * 0.02, " Mb)")

      fFilesListJson <- function(t, p, id) {
        file.path(tempDir, paste0("ctis_fileslist_", t, "_", p, "_", id, ".json"))
      }

      dlFiles$destfile <- fFilesListJson(
        dlFiles[["_id"]], dlFiles[["part"]], dlFiles[["id"]])

      tmp <- ctrMultiDownload(dlFiles[["url"]], dlFiles[["destfile"]])

      if (sum(tmp$status_code != 200L, na.rm = TRUE)) {
        warning("Could not download these lists with document information: ",
                paste0(tmp$url[tmp$status_code != 200L], collapse = ", "))
        tmp <- tmp[tmp$status_code == 200L, , drop = FALSE]
      }

      # 2 - create data frame with info on documents (url, name, extension etc.)

      message("- Processing document information in ", nrow(tmp), " lists")
      epTypChars <- paste0(names(epTyp), "appl", "auth", collapse = "")
      epTypChars <- rawToChar(unique(charToRaw(epTypChars)))
      unlink(downloadsNdjson)

      for (fi in seq_len(nrow(tmp))) {

        fn <- tmp[["destfile"]][fi]
        if (file.size(fn) < 50L) next # size 49

        # reconstruct trial id
        id <- sub(paste0(".+_(", regCtis, ")_.+"), "\\1", tmp[["destfile"]][fi])

        # reconstruct part
        part <- sub(paste0(
          "^.+_([", epTypChars, "]+?)_[0-9]+[.]json$"),
          "\\1", tmp[["destfile"]][fi])

        # get data
        jOut <- readLines(fn, warn = FALSE)

        # remove irrelevant information
        jOut <- sub('^.*"elements":(.*?)}?$', "\\1", jOut)
        jOut <- sub('(,?)"showWarning":(false|true)(,?)', "\\3", jOut)
        jOut <- sub('(,?)"totalSize":[0-9]+(,?)', "\\2", jOut)
        jOut <- sub('(,?)"pageInfo":[{].+?[}](,?)', "\\2", jOut)
        jOut <- gsub('"versions":[[][{].+?[}][]],', "", jOut) # reconsider
        if (!nchar(jOut) || jOut == "[]") next

        jOut <- paste0(
          '{"_id":"', id, '",',
          stringi::stri_extract_all_regex(jOut, '"url":"[-a-z0-9]+?",')[[1]],
          stringi::stri_extract_all_regex(jOut, '"title":".+?",')[[1]],
          stringi::stri_extract_all_regex(jOut, '"fileTypeLabel":"[A-Z]+?",')[[1]],
          stringi::stri_extract_all_regex(jOut, '"documentIdentity":[0-9]+?,')[[1]],
          '"part":"', part, '"}'
        )

        jOut <- jOut[!grepl('",NA"', jOut)]
        if (!length(jOut)) next

        cat(
          jOut,
          file = downloadsNdjson,
          append = TRUE,
          sep = "\n")

        message(fi, rep("\b", nchar(fi)), appendLF = FALSE)

      } # for

      # 3 - documents download
      message("- Creating subfolder for each trial")

      dlFiles <- jsonlite::stream_in(file(downloadsNdjson), verbose = FALSE)

      # remove duplicate files based on their title
      dlFiles$part <- ordered(dlFiles$part, orderedParts)
      dlFiles <- dlFiles[order(dlFiles$title, dlFiles$part), , drop = FALSE]
      rl <- rle(dlFiles$title)
      rl <- unlist(sapply(rl$lengths, function(i) c(TRUE, rep(FALSE, i - 1L))))
      dlFiles <- dlFiles[rl, , drop = FALSE]

      # add destination file name
      dlFiles$filename <- paste0(
        dlFiles$part, "_",
        # robustly sanitise file name
        gsub("[^[:alnum:] ._-]", "",  dlFiles$title),
        ".", dlFiles$fileTypeLabel)

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
          urls = sprintf(ctisEndpoints[10], dlFiles$url[!dlFiles$fileexists]),
          destfiles = dlFiles$filepathname[!dlFiles$fileexists])

        if (!nrow(tmp)) tmp <- 0L else {

          # handle failures despite success is true
          invisible(sapply(
            tmp[tmp$status_code != 200L, "destfile", drop = TRUE], unlink
          ))

          tmp <- nrow(tmp[tmp$status_code == 200L, , drop = FALSE])

        }
      }

      # inform user
      message(sprintf(paste0(
        "= Newly saved %i ",
        ifelse(is.null(documents.regexp), "placeholder ", ""),
        "document(s) for %i trial(s) (latest versions only, ",
        "deduplicated if e.g. in application and authorised part); ",
        "%i document(s) for %i trial(s) already existed in %s"),
        tmp,
        length(unique(dlFiles$`_id`)),
        sum(dlFiles$fileexists),
        length(unique(dlFiles$`_id`[dlFiles$fileexists])),
        documents.path
      ))

    } # directory created

  } # end if documents.path

  ## inform user on final import outcome
  message("= Imported / updated ",
          paste0(c(imported$n, resAll), collapse = " / "),
          " records on ", length(idsTrials), " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtis


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

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)
  # register function to remove files after use for streaming
  if (!verbose) on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)
  if (verbose) message("DEBUG: ", tempDir)

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
