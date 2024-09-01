### ctrdata package

#' ctrRerunQuery
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom httr content GET
#' @importFrom stringi stri_extract_all_regex
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

      # ctgov2:
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
    } # end ctgov2

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
            resultsRssTrials, FUN = function(x) {
              substr(resultsRss, x + 15, x + 28)
            }, character(1L))
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

      # obtain rss feed with list of recently updated trials
      rssquery <- utils::URLencode(
        paste0(
          "https://euclinicaltrials.eu/ct-public-api-services/services/ct/rss?",
          queryterm))

      if (verbose) message("DEBUG (rss url): ", rssquery)

      resultsRss <- try(httr::content(
        httr::GET(url = rssquery),
        encoding = "UTF-8",
        as = "text"), silent = TRUE)

      message(
        "Since the query was last run, ",
        length(stringi::stri_extract_all_regex(
          resultsRss, "<item [^>]+>\\s*<title>")[[1]]),
        " trials have been updated.")

      # issues: returned data do not include trial identifiers, thus no efficient loading possible;
      # returned data include all trials found with search, not only those updated or added in last
      # seven days; timestamp is the same for every trial listed, corresponding to time when called.
      # checked from: 2023-04-22 to last: 2023-10-28

      warning("'querytoupdate=", querytoupdate, "' not possible because no effcient way ",
              "was found thus far to retrieve from CTIS only recently changed trials ",
              "(last checked 2023-10-28). Reverting to normal download. ",
              call. = FALSE, immediate. = TRUE)

      # end issues

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
