### ctrdata package
### main functions


#' Retrieve or update information on clinical trials from register and store in
#' database
#'
#' Note that upsert is used to store data in database, which means that records
#' may accumulate in a data base from different queries. If you want to insert
#' into an empty database, include a mongo connection object to an empty database.
#'
#' @param queryterm Either a string with the full URL of a search in a register
#'   or the data frame returned by the \link{ctrGetQueryUrlFromBrowser} or the
#'   \link{dbQueryHistory} functions.
#'   The queryterm is recorded in the collection \code{ns} for later use to
#'   update records.
#' @param register Vector of abbreviations of registers to query, defaults to
#'   "EUCTR"
#' @param querytoupdate Either the word "last" or the number of query (based
#'   on \link{dbQueryHistory}) that should be run to retrieve any trial records
#'   that are new or have been updated since this query was run the last time.
#'   This parameter takes precedence over \code{queryterm}.
#'   For EUCTR, updates are available only for the last seven days;
#'   the query is run again if more time has passed since it was run last.
#' @param forcetoupdate If \code{TRUE}, run again the query given in
#'   \code{querytoupdate}, irrespective of when it was run last
#'   (default is \code{FALSE}).
#' @param euctrresults If \code{TRUE}, also download available results when
#'   retrieving and loading trials from EUCTR. This slows down this function.
#'   (For CTGOV, all available results are retrieved and loaded from
#'   ctrdata version 0.9.10 onwards.)
#' @param annotation.text Text to be including in the records retrieved
 #'   with the current query, in the field "annotation".
#' @param annotation.mode One of "append" (default), "prepend" or "replace"
#'   for new annotation.text with respect to any existing annotation for
#'   the records retreived with the current query.
#' @param details If \code{TRUE} (default), retrieve full protocol-related
#'   information from EUCTR or XML data from CTGOV, depending on the register
#'   selected. This gives all of the available details for the trials.
#'   Alternatively, set to \code{FALSE} to retrieve only summary information
#'   from EUCTR or CSV data from CTGOV. The full EUCTR information includes
#'   separate records for every country in which the trial is opened; use
#'   function \code{dbFindUniqueEuctrRecord} in a subsequent step to limit to
#'   one record from EUCTR per trial
#' @param parallelretrievals Number of parallel downloads of information from
#'   the register
#' @param debug Printing additional information if set to \code{TRUE}; default
#'   is \code{FALSE}.
#'
#' @inheritParams ctrMongo
#'
#'
#' @return Number of trials imported or updated in the database
#' @examples
#' # Retrieve protocol-related information on a single trial identified by EudraCT number
#' \dontrun{
#' ctrLoadQueryIntoDb (queryterm = "2013-001291-38")
#' }
#'
#' # For use with EudraCT: define paediatric population and cancer terms
#' \dontrun{
#' queryEuDefPaedPopulation  <- "age=under-18"
#' queryEuDef01paedOncTrials <- "cancer leukaem leukem sarcoma tumour tumor blastom gliom lymphom
#' malign hodgkin ewing rhabdo teratom tumeur leucemi"
#' queryEuDef01paedOncTrials <- gsub (" ", "%20OR%20", queryEuDef01paedOncTrials)
#' queryEuDef01paedOncTrials <- paste (queryEuDef01paedOncTrials, queryEuDefPaedPopulation, sep="&")
#' ctrLoadQueryIntoDb (queryterm = queryEuDef01paedOncTrials, parallelretrivals = 5)
#' }
#'
#' # Retrieve protocol-related information on ongoing interventional cancer trials in children
#' \dontrun{
#' ctrLoadQueryIntoDb (queryterm = "cancer&recr=Open&type=Intr&age=0", register = "CTGOV")
#' ctrLoadQueryIntoDb (queryterm = "NCT02239861", register = "CTGOV")
#' }
#'
#' @export
#'
ctrLoadQueryIntoDb <- function(queryterm = "", register = "EUCTR", querytoupdate = 0L, forcetoupdate = FALSE,
                               euctrresults = FALSE, annotation.text = "", annotation.mode = "append",
                               details = TRUE, parallelretrievals = 10, debug = FALSE,
                               collection = "ctrdata", uri = "mongodb://localhost/users",
                               password = Sys.getenv("ctrdatamongopassword"), verbose = FALSE) {

  ## parameter checks

  # deduce queryterm and register if a full url is provided
  if (class(queryterm) == "character" &&
      is.atomic(queryterm) &&
      length(queryterm) == 1L &&
      grepl("^https.+clinicaltrials.+", queryterm)) {
    #
    # remove any appended intrapage anchor from url, e.g. #tableTop
    queryterm <- sub("#.+$", "", queryterm)
    #
    queryterm <- ctrGetQueryUrlFromBrowser(queryterm)
    #
  }

  ## deal with data frame as returned from ctrQueryHistoryInDb and ctrGetQueryUrlFromBrowser
  if (is.data.frame(queryterm) &&
      all(substr(names(queryterm), 1, 6) == "query-")) {
    #
    nr <- nrow(queryterm)
    #
    if (nr > 1) warning("Using last row of queryterm parameter.", call. = FALSE, immediate. = TRUE)
    #
    register  <- queryterm[nr, "query-register"]
    queryterm <- queryterm[nr, "query-term"]
    #
  }

  ## sanity checks
  if ( (grepl("[^a-zA-Z0-9=+&%_-]", gsub("\\[", "", gsub("\\]", "", queryterm)))) & (register == ""))
    stop("Parameter 'queryterm' is not an URL showing results of a query or has unexpected characters: ",
         queryterm, ", expected are: a-zA-Z0-9=+&%_-[]. Perhaps additionally specify 'register = '?", call. = FALSE)
  #
  if ( (queryterm == "") & querytoupdate == 0L)
    stop("Parameter 'queryterm' is empty.", call. = FALSE)
  #
  if (!grepl(register, "CTGOVEUCTR"))
    stop("Parameter 'register' not known: ", register, call. = FALSE)
  #
  if (class(querytoupdate) != "character" &&
      querytoupdate != trunc(querytoupdate))
    stop("Parameter 'querytoupdate' is not an integer value or 'last'.", call. = FALSE)
  #
  if (class(querytoupdate) == "character" &&
      querytoupdate != "last")
    stop("Parameter 'querytoupdate' is not an integer value or 'last'.", call. = FALSE)

  # remove trailing or leading whitespace
  queryterm <- gsub("^\\s+|\\s+$", "", queryterm)

  # check annotation parameters
  if (annotation.text != "" & annotation.mode == "") stop(" annotation.mode empty", call. = FALSE)
  if (!(annotation.mode %in% c("append", "prepend", "replace"))) stop(" annotation.mode incorrect", call. = FALSE)

  # initialise variable that is filled only if an update is to be made
  queryupdateterm <- ""

  # check and set proxy if needed to access internet
  setProxy()

  ## check if we need to rerun previous query

  # check if parameters are consistent
  if ( (querytoupdate > 0) && (queryterm != "") ) warning("'queryterm' and 'querytoupdate' specified,",
                                                          " continuing only with new query", immediate. = TRUE)

  # rewrite parameters for running as update
  querytermoriginal <- queryterm
  if ( (querytoupdate > 0) && (queryterm == "") ) {
    #
    rerunparameters <- ctrRerunQuery(querytoupdate = querytoupdate, forcetoupdate = forcetoupdate,
                                     debug = debug,
                                     collection = collection, uri = uri,
                                     password = password, verbose = verbose,
                                     queryupdateterm = queryupdateterm)
    #
    # check rerunparameters and possibly stop function without error
    if (!is.data.frame(rerunparameters)) return(invisible(rerunparameters))
    #
    # set main parameters
    querytermoriginal <- rerunparameters$querytermoriginal
    queryupdateterm   <- rerunparameters$queryupdateterm
    queryterm         <- rerunparameters$queryterm
    register          <- rerunparameters$register
  }

  ## main function

  # parameters for core functions
  params <- list(queryterm = queryterm, register = register,
                 euctrresults = euctrresults, annotation.text = annotation.text, annotation.mode = annotation.mode,
                 details = details, parallelretrievals = parallelretrievals, debug = debug,
                 collection = collection, uri = uri,
                 password = password, verbose = verbose,
                 queryupdateterm = queryupdateterm)
  # call core functions
  imported <- switch(as.character(register),
                     "CTGOV" = do.call(ctrLoadQueryIntoDbCtgov, params),
                     "EUCTR" = do.call(ctrLoadQueryIntoDbEuctr, params)
  )

  ## finalise

  # return some useful information or break if not successful
  if (!exists("imported") || (imported$n == 0)) {
    message("Function did not result in any trial information imports.")
    return(invisible(list(n = 0, ids = "")))
  }

  # inform user
  if (debug) message("DEBUG: 'queryterm'=", queryterm,
                     "\n'queryupdateterm'=", queryupdateterm,
                     "\n'imported'=", imported,
                     "\n'register'=", register,
                     "\n'collection'=", collection,
                     "\nImported trials:", imported$ids)

  # add query parameters to database
  dbCTRUpdateQueryHistory(register = register,
                          queryterm = querytermoriginal,
                          recordnumber = imported$n,
                          collection = collection, uri = uri,
                          password = password, verbose = verbose)

  # invalidate any cached list of keys in collection
  if (exists(".dbffenv")) suppressWarnings({remove(list = paste0(uri, "/", collection), envir = .dbffenv)})

  # add metadata
  imported <- addMetaData(x = imported,
                          collection = collection, uri = uri,
                          password = password)

  ## return
  return(invisible(imported))

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
ctrRerunQuery <- function(querytoupdate = querytoupdate, forcetoupdate = forcetoupdate,
                          debug = debug,
                          collection = collection, uri = uri,
                          password = password, verbose = verbose,
                          queryupdateterm = queryupdateterm) {

  ## prepare

  # get history
  rerunquery <- dbQueryHistory(collection = collection, uri = uri,
                               password = password, verbose = verbose)

  # check parameters
  if (is.null(rerunquery))
    stop("'querytoupdate': no previous queries found in collection, aborting query update.", call. = FALSE)

  # select last query if specified
  if (querytoupdate == "last")
    querytoupdate <- nrow(rerunquery)

  # try to select the query to be updated
  if (querytoupdate > nrow(rerunquery))
    stop("'querytoupdate': specified number not found, check 'dbQueryHistory()'.", call. = FALSE)

  # set values retrieved
  rerunquery <- rerunquery[querytoupdate, ]
  queryterm  <- rerunquery$`query-term`
  register   <- rerunquery$`query-register`
  initialday <- substr(rerunquery$`query-timestamp`, start = 1, stop = 10)

  # secondary check parameters
  if (queryterm == "")
    stop("Parameter 'queryterm' is empty - cannot update query ", querytoupdate, call. = FALSE)
  #
  if (!grepl(register, "CTGOVEUCTR"))
    stop("Parameter 'register' not known - cannot update query ", querytoupdate, call. = FALSE)

  ## adapt updating procedure to respective register
  querytermoriginal <- queryterm

  ## mangle parameter only if not forcetoupdate,
  # which stipulates to just rerun original query
  if (!forcetoupdate) {

    # ctgov
    if (register == "CTGOV") {

      # ctgov:
      # speficy any date - "lup_s/e" last update start / end:
      # https://clinicaltrials.gov/ct2/results?term=&recr=&rslt=&type=Intr&cond=Cancer&intr=&titles=&outc=&spons=&lead=
      # &id=&state1=&cntry1=&state2=&cntry2=&state3=&cntry3=&locn=&gndr=&age=0&rcv_s=&rcv_e=&
      # lup_s=01%2F01%2F2015&lup_e=12%2F31%2F2016

      # if "lup_s" is already in query term, just re-run full query to avoid
      # multiple queries in history that only differ in the timestamp:
      if (grepl("&lup_[se]=[0-9]{2}", queryterm)) {
        #
        # remove queryupdateterm, thus running full again
        queryupdateterm <- ""
        warning("Query has date(s) for start or end of last update ('&lup_'); running again with these limits.",
                immediate. = TRUE)
        #
      } else {
        #
        queryupdateterm <- strftime(strptime(initialday, format = "%Y-%m-%d"), format = "%m/%d/%Y")
        queryupdateterm <- paste0("&lup_s=", queryupdateterm)
        if (debug) message("DEBUG: Updating using this additional query term: ", queryupdateterm)
        #
      }
      #
      message("Rerunning query: ", queryterm, "\nLast run: ", initialday)
    }

    # euctr
    if (register == "EUCTR") {

      # euctr:
      # studies added or updated in the last 7 days:
      # https://www.clinicaltrialsregister.eu/ctr-search/rest/feed/bydates?query=cancer&age=children

      # check if update request is within time windows offered by the register (7 days)
      if (difftime(Sys.Date(), initialday, units = "days") > 7) {
        #
        warning("'querytoupdate=", querytoupdate, "' not possible because it was last run more than 7 days ago",
                " and register provides information on changes only for the last 7 days. Reverting to normal download.",
                immediate. = TRUE)
        #
        message("Rerunning query: ", queryterm, "\nLast run: ", initialday)
        #
      } else {
        #
        # obtain rss feed with list of recently updated trials
        rssquery <- utils::URLencode(paste0("https://www.clinicaltrialsregister.eu/ctr-search/rest/feed/bydates?query=",
                                            queryterm))
        if (debug) message("DEBUG (rss url): ", rssquery)
        #
        resultsRss <- httr::content(httr::GET(url = rssquery,
                                              config = httr::config(ssl_verifypeer = FALSE)),
                                    as = "text")

        if (debug) message("DEBUG (rss content): ", resultsRss)
        #
        # attempt to extract euctr number(s)
        resultsRssTrials <- gregexpr("eudract_number:[0-9]{4}-[0-9]{6}-[0-9]{2}</link>", resultsRss)[[1]]
        #
        if (length(resultsRssTrials) == 1L && resultsRssTrials == -1L) {
          message("First result page empty - no (new) trials found?")
          return(invisible(0))
        }
        #
        # if new trials found, download
        resultsRssTrials <- sapply(resultsRssTrials, FUN = function(x) substr(resultsRss, x + 15, x + 28))
        resultsRssTrials <- paste(resultsRssTrials, collapse = "+OR+")
        if (debug) message("DEBUG (rss trials): ", resultsRssTrials)
        #
        # run query for extracted euctr number(s)
        # store original query in update term
        queryupdateterm <- queryterm
        queryterm <- resultsRssTrials
        #
        if (debug) message("DEBUG: Updating using this query term: ", queryupdateterm)
        #
        message("Rerunning query: ", queryupdateterm, "\nLast run: ", initialday)
        #
      }
    } # register euctr
  } # !forcetoupdate

  ## return main parameters needed
  return(data.frame("querytermoriginal" = querytermoriginal,
                    "queryupdateterm"   = queryupdateterm,
                    "queryterm"         = queryterm,
                    "register"          = register,
                    stringsAsFactors = FALSE))

} # end ctrRerunQuery


#' dbCTRLoadJSONFiles
#'
#' @param dir Path to local directory with JSON files from downloading and converting
#'
#' @param mongo ctrmongo Mongo DB database connection object
#'
#' @return List with elements n (number of imported trials) and ids (_ids of imported trials)
#'
#' @keywords internal
#'
dbCTRLoadJSONFiles <- function(dir, mongo) {

  # find files
  tempFiles <- dir(path = dir,
                   pattern = ".json",
                   full.names = TRUE)

  # initialise counters
  tmpids <- NULL
  tmpinsertall <- 0

  # iterate over files
  for (tempFile in tempFiles) {

    # main function for fast reading, switching off warning about final EOL missing
    fd <- file(description = tempFile)
    tmplines <- readLines(con = fd, warn = FALSE)
    close(fd)

    # readLines produces: \"_id\": \"2007-000371-42-FR\"
    ids <- sub(".*_id\":[ ]*\"(.*?)\".*", "\\1", tmplines)

    # ids should always be found
    if (all(ids == "")) stop("No _id(s) detected in converted JSON, cannot continue.")

    # replace documents since keys within documents cannot be updated
    tmpjson <- paste0('{ "_id": {"$in": ["', paste0(ids, collapse = '", "'), '"]}}')
    jsonlite::validate(tmpjson)
    #
    mongo$remove(query = tmpjson, just_one = FALSE)
    tmpinsert <- lapply(tmplines, mongo$insert)

    # increment counters
    tmpids <- c(tmpids, ids)
    tmpinsertall <- tmpinsertall + sum(sapply(tmpinsert, "[[", "nInserted"), na.rm = TRUE)
  }

  # prepare return value
  return(list(n = tmpinsertall, ids = ids))

} # end dbCTRLoadJSONFiles



#' dbQueryAnnotateRecords
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
#'
dbCTRAnnotateQueryRecords <- function(recordnumbers, annotations, annotation.text, annotation.mode,
                                      collection = collection, uri = uri,
                                      password = password, verbose = verbose){

  # debug
  if (verbose) message("* Running dbCTRAnnotateQueryRecords ...")
  if (verbose) message(recordnumbers)
  if (verbose) message(annotations)
  if (verbose) message(annotation.mode)

  # check if dataframe is as expected: columns _id and annotation
  if (nrow(annotations) == 0) {
    annotations <- data.frame("_id" = recordnumbers,
                              "annotation" = "",
                              stringsAsFactors = FALSE)
    names(annotations) <- c("_id", "annotation")
  }
  if (!("annotation" %in% names(annotations))) {
    annotations <- data.frame(annotations,
                              "annotation" = "",
                              stringsAsFactors = FALSE)
    names(annotations) <- c("_id", "annotation")
  }

  # keep only those annotations that are to be modified
  annotations <- annotations[annotations[["_id"]] %in% recordnumbers, ]

  # check if dataframe is as expected: columns _id and annotation
  if (nrow(annotations) == 0) {
    annotations <- data.frame("_id" = recordnumbers,
                              "annotation" = "",
                              stringsAsFactors = FALSE)
    names(annotations) <- c("_id", "annotation")
  }

  # modify the annotations
  annotations$annotation <- switch(annotation.mode,
                                   "replace" = paste0(annotation.text),
                                   "prepend" = paste0(annotation.text, " ", annotations$annotation),
                                               paste0(annotations$annotation, " ", annotation.text)
  )
  annotations$annotation <- paste0('{"$set": {"annotation": "',
                                   trimws(annotations$annotation),
                                   '"}}')

  # debug
  if (verbose) message(annotations)

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = verbose)

  # update the database
  for (i in annotations[["_id"]]) {
    mongo$update(query = paste0('{"_id": {"$eq": "', i, '"}}'),
                 update = annotations[ annotations[["_id"]] == i, 2],
                 upsert = TRUE)
  }

  # close database connection
  mongo$disconnect()

  # inform user
  message("= Annotated retrieved records")

} # end dbQueryAnnotateRecords


#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
#'
dbCTRUpdateQueryHistory <- function(register, queryterm, recordnumber,
                                    collection, uri,
                                    password, verbose){

  # debug
  if (verbose) message("Running dbCTRUpdateQueryHistory ...")

  # retrieve existing history data
  hist <- suppressMessages(
    dbQueryHistory(collection, uri,
                   password, verbose)
  )

  # debug
  if (verbose) print(hist)

  # append current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  hist <- rbind(hist, cbind("query-timestamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                            "query-register"  = register,
                            "query-records"   = recordnumber,
                            "query-term"      = queryterm),
                stringsAsFactors = FALSE)

  # collate information about current query into json object
  json <- jsonlite::toJSON(list("queries" = hist))

  # debug
  if (verbose) message(json)

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = verbose)

  # update database
  mongo$update(query = '{"_id": {"$eq": "meta-info"}}',
               update = paste0('{ "$set" :', json, "}"),
               upsert = TRUE)

  # close database connection
  mongo$disconnect()

  # inform user
  message('* Updated history in meta-info of "', collection, '"')

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
#'
ctrLoadQueryIntoDbCtgov <- function(queryterm, register,
                                    euctrresults, annotation.text, annotation.mode,
                                    details, parallelretrievals, debug,
                                    collection, uri,
                                    password, verbose,
                                    queryupdateterm) {

  ## sanity correction for naked terms
  # test cases
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
  queryterm <- sub("(^|&|[&]?\\w+=\\w+&)(\\w+|[NCT0-9-]+)($|&\\w+=\\w+)", "\\1term=\\2\\3", queryterm)

    ## check availability of relevant helper programs
  if (!suppressWarnings(installFindBinary("php --version")))
    stop("php not found.", call. = FALSE)
  #
  if (!suppressWarnings(installFindBinary("php -r 'simplexml_load_string(\"\");'")))
    stop("php xml not found.", call. = FALSE)

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)

  # CTGOV standard identifiers
  # updated 2017-07 with revised ctgov website links, e.g.
  # https://clinicaltrials.gov/ct2/results/download_studies?rslt=With&cond=Neuroblastoma&age=0&draw=3
  queryUSRoot   <- "https://clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download_studies?"
  queryUSType2  <- "ct2/results?"

  ## deal with special cases
  # ctgov queryterm is just NCT number, prefix with variable name
  if (grepl("^NCT[0-9]{8}$", queryterm)) queryterm <- paste0("term=", queryterm)
  # ctgov queryterm is just text, prefix with variable name
  if (!grepl("=", queryterm)) queryterm <- paste0("term=", queryterm)

  ## inform user and prepare url for downloading
  message("(1/3) Downloading trials from CTGOV as xml.")
  ctgovdownloadcsvurl <- paste0(queryUSRoot, queryUSType1, "&", queryterm, queryupdateterm)
  if (debug) message("DEBUG: ", ctgovdownloadcsvurl)

  # check if host is available
  if ("try-error" %in% class(try(httr::headers(httr::HEAD(url = utils::URLencode(queryUSRoot))), silent = TRUE)))
    stop("Host ", queryUSRoot, " does not respond, cannot continue.", call. = FALSE)

  # check number of trials to be downloaded
  ctgovdfirstpageurl <- paste0(queryUSRoot, queryUSType2, "&", queryterm, queryupdateterm)
  tmp <- httr::content(httr::GET(url = utils::URLencode(ctgovdfirstpageurl)), as = "text")
  tmp <- gsub("\n|\t|\r", " ", tmp)
  tmp <- gsub("<.*?>", " ", tmp)
  tmp <- gsub("  +", " ", tmp)
  tmp <- sub(".* (.*?) Stud(y|ies) found for.*", "\\1", tmp)

  # safeguard against no or unintended large numbers
  tmp <- suppressWarnings(as.integer(tmp))
  if (is.na(tmp) || !length(tmp)) {
    message("No trials or number of trials could not be determined: ", tmp)
    return(invisible(list(n = 0L, ids = "")))
  }
  if (as.integer(tmp) > 5000L) {
    message("These are ", tmp, " (more than 5000) trials, this may be unintended. ",
            "Please split into separate queries.")
    return(invisible(list(n = 0L, ids = "")))
  }

  # inform user
  message("Retrieved overview, ", tmp, " trial(s) are to be downloaded.")

  # prepare a file handle for saving in temporary directory
  f <- paste0(tempDir, "/", "ctgov.zip")

  # inform user
  message("Downloading trials ", appendLF = FALSE)

  # get (download) trials in single zip file f
  tmp <- httr::GET(url = utils::URLencode(ctgovdownloadcsvurl),
                   httr::progress(),
                   httr::write_disk(path = f,
                                    overwrite = TRUE))

  # inform user
  if (file.size(f) == 0)
    stop("No studies downloaded. Please check 'queryterm' or run again with debug = TRUE.", call. = FALSE)

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## compose commands to transform xml into json, into
  # a single allfiles.json in the temporaray directory
  xml2json <- system.file("exec/xml2json.php", package = "ctrdata", mustWork = TRUE)
  xml2json <- paste0("php -f ", xml2json, " ", tempDir)

  # special command handling on windows
  if (.Platform$OS.type == "windows") {
    # xml2json requires cygwin's php. transform paths for cygwin use:
    xml2json <- gsub("\\\\", "/", xml2json)
    xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
    xml2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', xml2json, '"')
  } # if windows

  # run conversion of downloaded xml to json
  message("\n(2/3) Converting to JSON ...")
  if (debug) message("DEBUG: ", xml2json)
  imported <- system(xml2json, intern = TRUE)

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = FALSE)

  # get any annotations for any later update
  if (annotation.text != "") {
    annotations <- mongo$find(query = paste0('{"_id": {"$ne": "meta-info"}}'),
                              fields = '{"_id": 1, "annotation": 1}')
  }

  ## run import
  message("(3/3) Importing JSON into mongoDB ...")
  if (debug) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 mongo = mongo)

  ## absorb nested id_info object into new array otherids of strings
  # obtain full data set on _id and other ids
  cursor <- mongo$iterate(query  = '{"_id": { "$regex": "^NCT[0-9]{8}", "$options": ""} }',
                          fields = '{"id_info.org_study_id": 1, "id_info.secondary_id": 1}'
  )$batch(size = mongo$count())
  # transform every second element in a list item into a vector
  otherids <- sapply(cursor, function(x) paste0(as.vector(unlist(x[2]))))
  # retain _id's for updating
  cursor <- sapply(cursor, function(x) as.vector(unlist(x[1])))
  # iterate over list items
  for (i in seq_len(length(cursor))) {
    # replace double square brackets around array
    tmp <- sub("\\[\\[", "[", sub("\\]\\]", "]", jsonlite::toJSON(list("otherids" = otherids[[i]]))))
    # upsert
    mongo$update(query  = paste0('{"_id":{"$eq":"', cursor[i], '"}}'),
                 update = paste0('{ "$set" :', tmp, "}"),
                 upsert = TRUE)
  }

  ## add index for newly created fired
  mongo$index(add = "otherids")
  message('Added index field "otherids".')

  ## close database connection
  mongo$disconnect()

  ## add annotations
  if ( (annotation.text != "") &
       (length(imported$ids) > 0) ) {

    # dispatch
    dbCTRAnnotateQueryRecords(recordnumbers = imported$ids, annotations = annotations,
                              annotation.text = annotation.text, annotation.mode = annotation.mode,
                              collection = collection, uri = uri,
                              password = password, verbose = verbose)

  }

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s).")

  # clean up temporary directory
  if (!debug) unlink(tempDir, recursive = TRUE)

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
#'
ctrLoadQueryIntoDbEuctr <- function(queryterm, register,
                                    euctrresults, annotation.text, annotation.mode,
                                    details, parallelretrievals, debug,
                                    collection, uri,
                                    password, verbose,
                                    queryupdateterm) {

  ## sanity correction for naked terms
  # test cases
  # queryterm = c("cancer&age=adult",                      # correct
  #               "cancer",                                # correct
  #               "cancer&age=adult&phase=0",              # correct
  #               "cancer&age=adult&phase=1&results=true", # correct
  #               "&age=adult&phase=1&abc=xyz&cancer&results=true", # correct
  #               "age=adult&cancer",                      # correct
  #               "2010-024264-18",                        # correct
  #               "NCT1234567890",                         # correct
  #               "teratoid&country=dk"                    # correct
  #               "term=cancer&age=adult",                 # keep
  #               "age=adult&term=cancer")                 # keep
  queryterm <- sub("(^|&|[&]?\\w+=\\w+&)(\\w+|[ +ORNCT0-9-]+)($|&\\w+=\\w+)", "\\1query=\\2\\3", queryterm)

  # check availability of relevant helper programs
  if (!suppressWarnings(installFindBinary("echo x | sed s/x/y/"))) stop("sed not found.",  call. = FALSE)
  if (!suppressWarnings(installFindBinary("perl -V:osname")))      stop("perl not found.", call. = FALSE)

  # inform user
  message("* Downloading trials from EUCTR:", appendLF = TRUE)

  # create empty temporary directory on localhost for
  # download from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)

  # EUCTR standard identifiers
  queryEuRoot  <- "https://www.clinicaltrialsregister.eu/"
  queryEuType1 <- "ctr-search/search?"
  queryEuType2 <- "ctr-search/rest/download/summary?"
  queryEuType3 <- "ctr-search/rest/download/full?"
  queryEuType4 <- "ctr-search/rest/download/result/zip/xml/"
  queryEuPost  <- "&mode=current_page&format=text&dContent=summary&number=current_page&submit-download=Download"

  # check if host is available
  if ("try-error" %in% class(try(httr::headers(httr::HEAD(url = utils::URLencode(queryEuRoot),
                                                          config = httr::config(ssl_verifypeer = FALSE))), silent = TRUE)))
    stop("Host ", queryEuRoot, " does not respond, cannot continue.", call. = FALSE)

  # get first result page
  q <- utils::URLencode(paste0(queryEuRoot, queryEuType1, queryterm))
  if (debug) message("DEBUG: queryterm is ", q)
  resultsEuPages <- httr::content(httr::GET(url = q,
                                            config = httr::config(ssl_verifypeer = FALSE)), as = "text")

  # get number of trials identified by query
  resultsEuNumTrials <- sub(".*Trials with a EudraCT protocol \\(([0-9,.]*)\\).*", "\\1", resultsEuPages)
  resultsEuNumTrials <- suppressWarnings(as.numeric(gsub("[,.]", "", resultsEuNumTrials)))

  # calculate number of results pages
  resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20)

  # check for plausbility and stop function without erro
  if (is.na(resultsEuNumPages) || is.na(resultsEuNumTrials) || (resultsEuNumTrials == 0)) {
    message("First result page empty - no (new) trials found?")
    return(invisible(list(n = 0, ids = "")))
  }

  # inform user
  message("Retrieved overview, ", resultsEuNumTrials, " trial(s) from ",
          resultsEuNumPages, " page(s) to be downloaded.")

  # calculate batches to get data from all results pages
  resultsNumBatches <- resultsEuNumPages %/% parallelretrievals
  resultsNumModulo  <- resultsEuNumPages %%  parallelretrievals
  message("(1/3) Downloading trials (max. ", parallelretrievals, " page[s] in parallel):")

  # progress indicator function
  cb <- function(req){message(". ", appendLF = FALSE)}

  # iterate over batches of results pages
  for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

    # parallel requests by using startpage:stoppage
    # TODO use queue and re-queueing
    startpage <- (i - 1) * parallelretrievals + 1
    stoppage  <- ifelse(i > resultsNumBatches,
                        startpage + resultsNumModulo,
                        startpage + parallelretrievals) - 1
    message(" p ", startpage, "-", stoppage, " ", appendLF = FALSE)

    # download all text files from pages in current batch into variable

    # prepare download and saving
    pool <- curl::new_pool()
    urls <- unlist(lapply(paste0(queryEuRoot, ifelse(details, queryEuType3, queryEuType2),
                                 queryterm, "&page=", startpage:stoppage, queryEuPost),
                          utils::URLencode))
    fp <- paste0(tempDir, "/euctr-trials-page_",
                 formatC(startpage:stoppage, digits = 0, width = nchar(resultsEuNumPages), flag = 0), ".txt")
    tmp <- lapply(seq_along(urls),
                  function(x) curl::curl_fetch_multi(url = urls[x],
                                                     done = cb,
                                                     pool = pool,
                                                     data = fp[x],
                                                     handle = curl::new_handle(ssl_verifypeer = FALSE)
                                                     ))

    # do download and saving
    tmp <- curl::multi_run(pool = pool, poll = length(urls))

    # check plausibility
    if (class(tmp) == "try-error")
      stop("Download from EUCTR failed; last error: ", class(tmp), call. = FALSE)
    #
    # if (length(tmp) != (stoppage - startpage + 1))
    if (tmp[["success"]] != (stoppage - startpage + 1))
      stop("Download from EUCTR failed; incorrect number of records.", call. = FALSE)

    # clean up large object
    rm(tmp)

  } # for batch

  # compose commands: for external script on all files in temporary directory and for import
  euctr2json <- system.file("exec/euctr2json.sh", package = "ctrdata", mustWork = TRUE)
  euctr2json <- paste(euctr2json, tempDir)

  # special handling in case of windows
  if (.Platform$OS.type == "windows") {
    #
    # euctr2json requires cygwin's perl, sed. transform paths for cygwin use
    euctr2json <- gsub("\\\\", "/", euctr2json)
    euctr2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", euctr2json)
    euctr2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', euctr2json, '"')
    #
  }

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = FALSE)

  # get any annotations for any later update
  if (annotation.text != "") {

    # retrieve annotations
    annotations <- mongo$find(query = paste0('{"_id": {"$ne": "meta-info"}}'),
                              fields = '{"_id": 1, "annotation": 1}')

  }

  # run conversion of text files saved into file system to json file
  message("\n(2/3) Converting to JSON ...")
  if (debug) message("DEBUG: ", euctr2json)
  imported <- system(euctr2json, intern = TRUE)

  # run import into mongo from json files
  message("(3/3) Importing JSON into mongoDB ...")
  if (debug) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 mongo = mongo)

  # close database connection
  mongo$disconnect()

  ## read in the eudract numbers of the trials just retrieved and imported
  eudractnumbersimported <- imported$ids

  ## add annotations
  if ( (annotation.text != "") &
       (length(eudractnumbersimported) > 0) ) {

    # dispatch
    dbCTRAnnotateQueryRecords(recordnumbers = eudractnumbersimported, annotations = annotations,
                              annotation.text = annotation.text, annotation.mode = annotation.mode,
                              collection = collection, uri = uri,
                              password = password, verbose = verbose)
  }

  ## inform user on final import outcome
  message("= Imported or updated ", imported$n, " records on ", resultsEuNumTrials, " trial(s).")

  # debug
  if (debug) message(eudractnumbersimported)


  ## results: load also euctr trials results if requested
  if (euctrresults) {

    # results are available only one-by-one for each trial as just retrieved and imported

    # transform eudract numbers with country info ("2010-024264-18-3RD") into eudract numbers ("2010-024264-18")
    eudractnumbersimported <- unique(substring(text = eudractnumbersimported, first = 1, last = 14))

    # inform user
    message("* Retrieve results if available from EUCTR for ", length(eudractnumbersimported), " trials: ")

    ## parallel download and unzipping into temporary directory

    # "https://www.clinicaltrialsregister.eu/ctr-search/rest/download/result/zip/xml/..."
    # first version:  "2007-000371-42/1"
    # second version: "2007-000371-42/2"
    # latest version: "2007-000371-42"

    # calculate batches to get data from all results pages
    resultsNumBatches <- length(eudractnumbersimported) %/% parallelretrievals
    resultsNumModulo  <- length(eudractnumbersimported) %%  parallelretrievals

    # inform user
    message("(1/4) Downloading results (max. ", parallelretrievals,
            " trials in parallel):", appendLF = FALSE)

    # iterate over batches of results
    for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

      # calculated indices for eudractnumbersimported vector
      startindex <- (i - 1) * parallelretrievals + 1
      stopindex  <- ifelse(i > resultsNumBatches,
                           startindex + resultsNumModulo,
                           startindex + parallelretrievals) - 1

      # inform user
      message("\n t ", startindex, "-", stopindex, " ", appendLF = FALSE)

      # prepare download and save
      pool <- curl::new_pool()
      urls <- unlist(lapply(paste0(queryEuRoot, queryEuType4,
                                   eudractnumbersimported[startindex : stopindex]),
                            utils::URLencode))
      fp <- paste0(tempDir, "/", eudractnumbersimported[startindex : stopindex], ".zip")
      tmp <- lapply(seq_along(urls),
                    function(x) curl::curl_fetch_multi(url = urls[x],
                                                       done = cb,
                                                       pool = pool,
                                                       data = fp[x],
                                                       handle = curl::new_handle(ssl_verifypeer = FALSE)
                                                       ))

      # do download and save
      tmp <- curl::multi_run(pool = pool, poll = length(urls))

      # unzip downloaded file and rename
      tmp <- unlist(lapply(seq_along(urls), function(x) {

        if (file.size(fp[x]) != 0) {

          tmp <- utils::unzip(zipfile = fp[x], exdir = tempDir)

          if (any(grepl("pdf$", tmp)))
            message("PDF ", appendLF = FALSE)

          # TODO could there be more than one XML file?
          if (any(tmp2 <- grepl("xml$", tmp)))
            file.rename(tmp[tmp2][1], paste0(tempDir, "/",
                                             eudractnumbersimported[startindex : stopindex][x], ".xml"))

          message(". ", appendLF = FALSE)
        } else {
          message("x ", appendLF = FALSE)
          # message(" no results for ",
          #         eudractnumbersimported[startindex : stopindex][x],
          #         appendLF = FALSE)
        }

        # clean up
        if (!debug) unlink(fp[x])

      }))

    } # for batch

    ## use system commands to convert
    ## xml to json and to import json

    # compose command
    xml2json <- system.file("exec/xml2json_euctrresults.php", package = "ctrdata", mustWork = TRUE)
    xml2json <- paste0("php -f ", shQuote(xml2json), " ", shQuote(tempDir))

    # special command handling on windows
    if (.Platform$OS.type == "windows") {
      # xml2json_euctrresults requires cygwin's php. transform paths for cygwin use:
      xml2json <- gsub("\\\\", "/", xml2json)
      xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
      xml2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', xml2json, '"')
      #
    } # if windows

    # run conversion of downloaded xml to json
    message("\n(2/4) Converting to JSON ...")
    if (debug) message("DEBUG: ", xml2json)
    importedresults <- system(xml2json, intern = TRUE)


    # get a working mongo connection, select trial record collection
    mongo <- ctrMongo(collection = collection, uri = uri,
                      password = password, verbose = FALSE)

    # iterate over batches of results files
    message("(3/4) Importing JSON into mongoDB ...")
    importedresults <- NULL
    for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

      # calculated indices for eudractnumbersimported vector
      startindex <- (i - 1) * parallelretrievals + 1
      stopindex  <- ifelse(i > resultsNumBatches,
                           startindex + resultsNumModulo,
                           startindex + parallelretrievals) - 1

      batchresults <- sapply(eudractnumbersimported[startindex : stopindex],
                             function(x) {

                               # compose file name and check
                               fileName <- paste0(tempDir, "/", x, ".json")
                               if (file.exists(fileName) && file.size(fileName) > 0){

                                 # read contents
                                 tmp <- readChar(con = fileName, nchars = file.info(fileName)$size, useBytes = TRUE)

                                 # update database with results
                                 tmp <- try({tmp <- mongo$update(query  = paste0('{"a2_eudract_number":{"$eq":"', x, '"}}'),
                                                          update = paste0('{ "$set" :', tmp, "}"),
                                                          upsert = TRUE, multiple = TRUE)
                                             as.numeric(max(tmp$modifiedCount,
                                                            tmp$matchedCount,
                                                            na.rm = TRUE))
                                             }, silent = TRUE)

                                 # inform user on failed trial
                                 if (class(tmp) == "try-error") {
                                   warning(paste0("Import into mongo failed for trial ", x), immediate. = TRUE)
                                   tmp <- 0
                                 }

                               } else {

                                 # file did not exist
                                 tmp <- 0

                               }

                               # return for accumulating information
                               return(tmp)

                             }) # import

      # accumulate
      importedresults <- c(importedresults, batchresults)

    } # for batch

    # iterate over batches of result history from webpage
    message("(4/4) Retrieving any results history and importing into mongoDB ...", appendLF = FALSE)
    for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

      # calculated indices for eudractnumbersimported vector
      startindex <- (i - 1) * parallelretrievals + 1
      stopindex  <- ifelse(i > resultsNumBatches,
                           startindex + resultsNumModulo,
                           startindex + parallelretrievals) - 1

      # inform user
      message("\n h ", startindex, "-", stopindex, " ", appendLF = FALSE)

      # prepare download and save
      pool <- curl::new_pool()
      done <- function(res){retdat <<- c(retdat, list(res))}
      urls <- unlist(lapply(paste0("https://www.clinicaltrialsregister.eu/ctr-search/trial/",
                                   eudractnumbersimported[startindex : stopindex], "/results"),
                            utils::URLencode))
      tmp <- lapply(seq_along(urls),
                    function(x) curl::curl_fetch_multi(url = urls[x],
                                                       done = done,
                                                       pool = pool,
                                                       handle = curl::new_handle(ssl_verifypeer = FALSE)
                                                       ))

      # do download and save into batchresults
      retdat <- NULL
      tmp <- curl::multi_run(pool = pool,
                             poll = length(urls))
      batchresults <- lapply(retdat,
                             function(x) rawToChar(x[["content"]]))

      # curl return sequence is not predictable
      # therefore recalculate the eudract numbers
      eudractnumberscurled <- sapply(retdat, function(x) x[["url"]])
      eudractnumberscurled <- sub(".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*", "\\1", eudractnumberscurled)

      # for date time conversion
      lct <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")

      # extract information about results
      tmpFirstDate <- as.Date(sapply(batchresults, function(x)
        trimws(sub(".+First version publication date</div>.*?<div>(.+?)</div>.*", "\\1",
                   ifelse(grepl("First version publication date", x), x, "")))),
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

      tmpChanges <- sapply(batchresults, function(x)
        trimws(gsub("[ ]+", " ",
               gsub("[\n\r]", "",
               gsub("<[a-z/]+>", "",
               sub(".+Version creation reason.*?<td class=\"valueColumn\">(.+?)</td>.+", "\\1",
                   ifelse(grepl("Version creation reason", x), x, ""))
               ))))
      )

      tmp <- lapply(seq_along(along.with = startindex : stopindex), function(x) {
        upd <- mongo$update(query = paste0('{"a2_eudract_number": {"$eq": "',
                                           eudractnumberscurled[x], '"}}'),
                            update = paste0('{ "$set" : {',
                                            '"firstreceived_results_date" : "',  tmpFirstDate[x], '", ',
                                            '"version_results_history"    : "',  tmpChanges[x],   '"',
                                            "}}"),
                            upsert = TRUE,
                            multiple = TRUE)
        if(debug) message(upd)
      })

      # clean up large object
      rm(batchresults)

    } # for batch

    # close database connection
    mongo$disconnect()

    # sum up successful downloads
    importedresults <- sum(unlist(importedresults))

    ## inform user on final import outcome
    message("\n= Imported or updated results for ", importedresults,
            " records for ", resultsEuNumTrials, " trial(s).")

  } # if euctrresults


  # clean up temporary directory
  if (!debug) unlink(tempDir, recursive = TRUE)

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr
