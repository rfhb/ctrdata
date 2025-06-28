### ctrdata package

#' ctrRerunQuery
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom stringi stri_extract_all_regex
#' @importFrom jqr jq
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_query docdb_update
#' @importFrom httr2 req_perform req_body_json request req_user_agent
#'
ctrRerunQuery <- function(
    querytoupdate = querytoupdate,
    forcetoupdate = forcetoupdate,
    ctishistory = ctishistory,
    only.count = only.count,
    con = con,
    verbose = verbose,
    queryupdateterm = queryupdateterm) {

  ## check database connection
  con <- ctrDb(con)

  ## prepare
  failed <- NULL

  ## handle query history -----------------------------------------------------
  rerunquery <- dbQueryHistory(con = con, verbose = verbose)

  # check parameters
  if (is.null(rerunquery) || !nrow(rerunquery))
    stop("'querytoupdate': no previous queries found in collection, ",
         "aborting query update", call. = FALSE)

  # select last query if specified
  if (querytoupdate == "last")
    querytoupdate <- nrow(rerunquery)

  # check parameters
  if (!(as.integer(querytoupdate) == querytoupdate))
    stop("'querytoupdate' needs to be an integer number", call. = FALSE)
  querytoupdate <- as.integer(querytoupdate)

  # try to select the query to be updated
  if (querytoupdate > nrow(rerunquery) ||
      querytoupdate < 1L) {
    stop("'querytoupdate': specified query number ", querytoupdate,
         " not found, check 'dbQueryHistory()'", call. = FALSE)
  }

  # set query values as retrieved
  queryterm  <- rerunquery[querytoupdate, "query-term", drop = TRUE]
  register   <- rerunquery[querytoupdate, "query-register", drop = TRUE]

  # secondary check parameters in case history queries need
  # to be translated or otherwise manipulated as for new queries
  query <- ctrGetQueryUrl(url = queryterm, register = register)
  queryterm <- query$`query-term`
  register <- query$`query-register`

  # when was this query last run?
  #
  # - dates of all the same queries
  initialday <- rerunquery[["query-timestamp"]][
    rerunquery[querytoupdate, "query-term", drop = TRUE] ==
      rerunquery[["query-term"]]]
  #
  # - remove time, keep date
  initialday <- substr(initialday, start = 1, stop = 10)
  #
  # - change to Date class and get
  #   index of latest (max) date,
  initialdayindex <- try(which.max(as.Date(initialday, format = "%Y-%m-%d")))
  if (!inherits(initialdayindex, "try-error")) {
    # - keep initial (reference) date of this query
    initialday <- initialday[initialdayindex]
    message("* Query last run: ", rerunquery[
      initialdayindex, "query-timestamp", drop = TRUE])
  } else {
    # - fallback to number (querytoupdate) as specified by user
    initialday <- rerunquery[querytoupdate, "query-timestamp", drop = TRUE]
    message("* Query last run: ", initialday)
  }

  ## adapt updating procedure to respective register
  querytermoriginal <- queryterm

  # mangle parameter only if not forcetoupdate,
  # which just returns parameters of original query
  if (!forcetoupdate) {

    # inform user
    message("* Checking for new or updated trials...")

    # ctgov --------------------------------------------------------------------
    if (register == "CTGOV") {

      # ctgov:
      # specify any date - "lup_s/e" last update start / end:
      # https://classic.clinicaltrials.gov/ct2/results?term=&recr=&rslt=&type=Intr&cond=
      # Cancer&intr=&titles=&outc=&spons=&lead=
      # &id=&state1=&cntry1=&state2=&cntry2=&state3=&cntry3=&locn=&gndr=&age=0
      # &rcv_s=&rcv_e=&lup_s=01%2F01%2F2015&lup_e=12%2F31%2F2016

      # if "lup_s" is already in query term, just re-run full query to avoid
      # multiple queries in history that only differ in timestamp:
      if (grepl("&lup_[se]=[0-9]{2}", queryterm)) {

        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ",
                "('&lup_'); running again with these limits",
                call. = FALSE, immediate. = TRUE)

      } else {

        queryupdateterm <- strftime(
          strptime(initialday,
                   format = "%Y-%m-%d"),
          format = "%m/%d/%Y")

        queryupdateterm <- paste0("&lup_s=", queryupdateterm)

        if (verbose) {
          message("DEBUG: Updating using this additional query term: ",
                  queryupdateterm)
        }

      }

      message("Rerunning query: ", queryterm,
              "\nLast run: ", initialday)
    } # end ctgov

    # ctgov2 -------------------------------------------------------------------
    if (register == "CTGOV2") {

      # ctgov2:
      # specify last update start / end:
      # https://www.clinicaltrials.gov/search?cond=Cancer&lastUpdPost=2022-01-01_2023-12-31

      # if "lastUpdPost" is already in query term, just re-run full query to avoid
      # multiple queries in history that only differ in timestamp:
      if (grepl("&lastUpdPost=[0-9]{2}", queryterm)) {

        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ",
                "('&lastUpdPost'); running again with these limits",
                call. = FALSE, immediate. = TRUE)

      } else {

        queryupdateterm <- strftime(
          strptime(initialday,
                   format = "%Y-%m-%d"),
          format = "%Y-%m-%d")

        queryupdateterm <- paste0("&lastUpdPost=", queryupdateterm, "_")

        if (verbose) {
          message("DEBUG: Updating using this additional query term: ",
                  queryupdateterm)
        }

      }

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

        warning("'querytoupdate=", querytoupdate, "' not possible because ",
                "it was last run more than 7 days ago and the register ",
                "provides information on changes only for the last 7 days. ",
                "Reverting to normal download. ",
                call. = FALSE, immediate. = TRUE)

        message("Rerunning query: ", queryterm,
                "\nLast run: ", initialday)

      } else {
        #
        # obtain rss feed with list of recently updated trials
        rssquery <- utils::URLencode(
          paste0("https://www.clinicaltrialsregister.eu/ctr-search/",
                 "rest/feed/bydates?", queryterm))
        #
        if (verbose) message("DEBUG (rss url): ", rssquery)
        #
        resultsRss <- try(rawToChar(
          httr2::req_perform(
            httr2::req_user_agent(
              httr2::request(
                rssquery),
              ctrdataUseragent
            ))$body), silent = TRUE)
        #
        # check plausibility
        if (inherits(resultsRss, "try-error")) {
          stop("Download from EUCTR failed; last error: ",
               class(resultsRss), call. = FALSE)
        }
        #
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
          # query for updated trials
          dbCTRUpdateQueryHistory(
            register = register,
            queryterm = queryterm,
            recordnumber = 0L,
            con = con,
            verbose = verbose)
          #
          # set indicator
          failed <- emptyReturn
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

        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ",
                "('lastEdited'); running again with these limits",
                immediate. = TRUE)

      } else {

        queryupdateterm <- strftime(
          strptime(initialday,
                   format = "%Y-%m-%d"),
          format = "%Y-%m-%d")

        queryupdateterm <- paste0(" AND lastEdited GE ",
                                  queryupdateterm,
                                  "T00:00:00.000Z")

        if (verbose) {
          message("DEBUG: Updating using this additional query term: ",
                  queryupdateterm)
        }

      }

      message("Rerunning query: ", queryterm,
              "\nLast run: ", initialday)
    } # end isrctn

    # ctis ------------------------------------------------------------------
    if (register == "CTIS") {

      # principles:
      # - historic ctis versions are only created in ctrRerunQuery
      #   because the user needs to "trigger" creating a version,
      #   since ctis does not on its own offer an API for retrieving
      #   versions of a record
      # - in ctrRerunQuery, updating trials identified in the last
      #   seven days is straightforward and can readily include
      #   creating historic versions
      # - for rerunning older queries,
      #   we need to get the concerned ids and this is only possible
      #   with a full ctrLoadQueryIntoDb but into a separate database
      #   so that we do not overwrite existing records;
      #   over the resulting $success trial identifiers,
      #   then iterate

      # helper function
      getIdsFromQuery <- function(queryterm) {

        # initialise
        idsUpdatedTrials <- NULL
        pageNumber <- 1L

        # iterate
        while (TRUE) {

          initialData <- try(rawToChar(
            httr2::req_perform(
              httr2::req_body_json(
                httr2::req_user_agent(
                  httr2::request(
                    "https://euclinicaltrials.eu/ctis-public-api/search"),
                  ctrdataUseragent),
                data = jsonlite::fromJSON(
                  paste0(
                    # add pagination parameters
                    '{"pagination":{"page":', pageNumber, ",",
                    # empirically found this as max
                    '"size":999},',
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
                  ), simplifyVector = FALSE)
              ))$body), silent = TRUE)

          # accumulate trial identifiers
          idsUpdatedTrials <- c(
            idsUpdatedTrials, gsub(
              '"', "", jqr::jq(initialData, " .data[].ctNumber ")
            ))

          pageNumber <- pageNumber + 1L
          if (jqr::jq(initialData, ".pagination.nextPage") == "false") break

        } # while

        return(idsUpdatedTrials)

      } # end getIdsFromQuery

      # helper function
      getIdsFromRss <- function(queryterm) {

        # ctis: studies added or updated in the last 7 days:
        # "https://euclinicaltrials.eu/ctis-public-api/rss/updates.rss?
        # search_criteria={"ageGroupCode":[2],"therapeuticAreaCode":[4]}"

        # obtain rss feed with list of recently updated trials
        rssquery <- paste0(
          "https://euclinicaltrials.eu/ctis-public-api/rss/updates.rss?search_criteria=",
          utils::URLencode(sub("searchCriteria=", "", queryterm)))

        if (verbose) message("DEBUG (rss url): ", utils::URLdecode(rssquery))

        resultsRss <- try(rawToChar(
          httr2::req_perform(
            httr2::req_user_agent(
              httr2::request(
                rssquery),
              ctrdataUseragent
            ))$body), silent = TRUE)

        idsUpdatedTrials <- stringi::stri_extract_all_regex(
          # <link>https://euclinicaltrials.eu/search-for-clinical-trials/?lang=en&amp;EUCT=2024-516838-35-00</link>
          resultsRss, "EUCT=[-0-9]+</link>")[[1]]
        idsUpdatedTrials <- na.omit(stringi::stri_replace_all_regex(
          idsUpdatedTrials, "EUCT=([-0-9]+)</link>", "$1"))

        return(idsUpdatedTrials)

      } # end getIdsFromRss

      # helper function
      updateOrLoadTrial <- function(trialId, con, ctishistory) {

        message(". ", appendLF = FALSE)

        # check if exists
        recExists <- nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = paste0('{"_id":"', trialId, '"}'),
          fields = '{"_id":1}')

        # get existing data in collection
        if (ctishistory) {
          exstJson <- nodbi::docdb_query(
            src = con,
            key = con$collection,
            query = paste0('{"_id":"', trialId, '"}'))
        }

        # get new data
        result <- suppressWarnings(
          suppressMessages(
            ctrLoadQueryIntoDbCtis(
              queryterm = paste0('searchCriteria={"number":"', trialId, '"}'),
              con = con,
              documents.path = NULL,
              only.count = FALSE,
              verbose = FALSE
            )))
        result$updated <- length(recExists)

        # if historical version is to be created
        if (ctishistory && nrow(exstJson)) {

          # move existing data into historical version
          exstJson <- jsonlite::toJSON(exstJson)
          exstJson <- jqr::jq(
            exstJson, paste0(
              '{ _id: .[] | ._id,
                history: [
                 .[] | del(.history) | .history_version = {
                 version_date: .lastUpdated,
                 version_number: 0},
                if has("history") then .history[] else empty end
               ] }'
            ))

          # temporary file and cleanup
          tfname <- tempfile()
          on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)
          writeLines(exstJson, con = file(tfname))

          # update record, adding historical versions
          # avoid SQL issues by using file-based json
          result$updated <- nodbi::docdb_update(
            src = con,
            key = con$collection,
            value = tfname,
            query = '{}'
          )

        } # if historical version

        # default return
        return(result)

      } # end updateOrLoadTrial

      # helper function
      histCreateRet <- function(res) {

        # querytermoriginal, queryupdateterm, queryterm, register

        # construct return object
        ret <- NULL
        ret$n <- sum(sapply(res, "[[", "n"))
        ret$success <- unlist(sapply(res, "[[", "success"), use.names = FALSE)
        ret$failed <- NULL
        ret$queryterm <- querytermoriginal
        ret$updated <- sum(sapply(res, "[[", "updated"))

        # annotate
        if (ret$n > 0L) {
          dbCTRUpdateQueryHistory(
            register = register,
            queryterm = querytermoriginal,
            recordnumber = ret$n,
            con = con,
            verbose = verbose
          )
        }

        # add meta-data
        ret <- addMetaData(x = ret, con = con)

        # failed is indicator to not run main function
        return(list(
          "querytermoriginal" = querytermoriginal,
          "queryupdateterm"   = queryupdateterm,
          "queryterm"         = queryterm,
          "register"          = register,
          "failed"            = ret))

      }

      #### .dispatch ####
      if (difftime(Sys.Date(), initialday, units = "days") <= 7L) {

        # get
        idsUpdatedTrials <- getIdsFromRss(queryterm)

        # early exit if only.count
        if (only.count || !length(idsUpdatedTrials)) {
          res <- NULL
          res$n <- length(idsUpdatedTrials)
          res$queryterm <- querytermoriginal
          message("Imported or updated ", res$n, " trial(s)")
          return(list(failed = res))
        }

        # user interim info
        message(
          "Query finds ", length(idsUpdatedTrials), " trials, ",
          "loading and updating trials one-by-one (estimate: ",
          signif(length(idsUpdatedTrials) * 8 / 23L / 60L, 2L), " min)")

        # iterate
        res <- list()
        for (trialId in idsUpdatedTrials) res <- c(
          res, list(updateOrLoadTrial(trialId, con, ctishistory)))

        # info
        message("\n",
                sum(sapply(res, "[[", "updated")), " updated, ",
                sum(sapply(res, "[[", "n")) - sum(sapply(res, "[[", "updated")),
                " new records")

        # return and signal to ctrLoadQueryIntoDb to exit early
        return(histCreateRet(res))

      } else {

        if (ctishistory) {

          # get
          idsUpdatedTrials <- getIdsFromQuery(queryterm)

          # early exit if only.count
          if (only.count) {
            res <- NULL
            res$n <- length(idsUpdatedTrials)
            res$queryterm <- querytermoriginal
            message("Imported or updated ", res$n, " trial(s)")
            return(list(failed = res))
          }

          # user interim info
          warning(
            "Query finds ", length(idsUpdatedTrials), " trials, ",
            "loading and updating trials one-by-one (estimate: ",
            signif(length(idsUpdatedTrials) * 78 / 233L / 60L, 2L), " min)")

          # iterate
          res <- list()
          for (trialId in idsUpdatedTrials) res <- c(
            res, list(updateOrLoadTrial(trialId, con, ctishistory)))

          # info
          message(
            "\n",
            sum(sapply(res, "[[", "updated")), " updated, ",
            sum(sapply(res, "[[", "n")) - sum(sapply(res, "[[", "updated")),
            " new records")

          # return and signal to ctrLoadQueryIntoDb to exit early
          return(histCreateRet(res))

        } else {

          # rerunning original query
          warning(
            "'querytoupdate=", querytoupdate, "' not possible because no effcient way ",
            "was found so far to query CTIS for data only from recently changed trials ",
            "(last checked 2025-04-05). Reverting to normal download. ",
            call. = FALSE, immediate. = TRUE)

          # standard case in main function ctrLoadQueryIntoDb
          message("Rerunning query: ", queryterm,
                  "\nLast run: ", initialday)

        } # if ctishistory

      } # if difftime

    } # end ctis

  } # forcetoupdate

  #### return ####

  ## return main parameters needed
  return(list(
    "querytermoriginal" = querytermoriginal,
    "queryupdateterm"   = queryupdateterm,
    "queryterm"         = queryterm,
    "register"          = register,
    "failed"            = failed))

} # end ctrRerunQuery
