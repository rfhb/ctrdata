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
#' @param querytoupdate Either the word "last" to re-run the last query that
#'   was loaded into the collection, or the integer number of query to be run
#'   again; see \link{dbQueryHistory}. This parameter takes precedence over
#'   \code{queryterm}.
#' @param euctrresults If \code{TRUE}, also download available results when
#'   retrieving and loading trials from EUCTR. This slows down this function.
#'   (For CTGOV, all available results are retrieved and loaded from
#'   ctrdata version 0.9.10 onwards.)
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
ctrLoadQueryIntoDb <- function(queryterm = "", register = "EUCTR", querytoupdate = 0L,
                               euctrresults = FALSE,
                               details = TRUE, parallelretrievals = 10, debug = FALSE,
                               collection = "ctrdata", db = "users", url = "mongodb://localhost",
                               username = "", password = "", verbose = FALSE) {

  ## parameter checks

  # deduce queryterm and register if a full url is provided
  if (class(queryterm) == "character" &&
      is.atomic(queryterm) &&
      length(queryterm) == 1 &&
      grepl ("^https.+clinicaltrials.+", queryterm)) {
    #
    # remove any appended intrapage anchor from url, e.g. #tableTop
    queryterm <- sub("#.+$", "", queryterm)
    #
    queryterm <- ctrGetQueryUrlFromBrowser(queryterm)
    #
  }

  ## deal with data frame as returned from ctrQueryHistoryInDb() and ctrGetQueryUrlFromBrowser()
  if (is.data.frame(queryterm) &&
      all(substr(names(queryterm), 1, 6) == "query-")) {
    #
    nr <- nrow(queryterm)
    #
    if (nr > 1) warning("Using last row of queryterm parameter.", immediate. = TRUE)
    #
    register  <- queryterm [nr, "query-register"]
    queryterm <- queryterm [nr, "query-term"]
    #
  }

  ## sanity checks
  if ((grepl("[^a-zA-Z0-9=+&%_-]", gsub("\\[", "", gsub("\\]", "", queryterm)))) & (register == ""))
    stop("Parameter 'queryterm' is not an URL showing results of a query or has unexpected characters: ",
         queryterm, ", expected are: a-zA-Z0-9=+&%_-[]. Perhaps additionally specify 'register = '?")
  #
  if ( (queryterm == "") & querytoupdate == 0)
    stop("Parameter 'queryterm' is empty.")
  #
  if (!grepl(register, "CTGOVEUCTR"))
    stop("Parameter 'register' not known: ", register)
  #
  if (class(querytoupdate) != "character" &&
      querytoupdate != trunc(querytoupdate))
    stop("Parameter 'querytoupdate' is not an integer value or 'last'.")
  #
  if (class(querytoupdate) == "character" &&
      querytoupdate != "last")
    stop("Parameter 'querytoupdate' is not an integer value or 'last'.")

  # check program availability
  installMongoFindBinaries(debug = debug)
  if (.Platform$OS.type == "windows") installCygwinWindowsTest()

  # check program version as acceptable json format changed from 2.x to 3.x
  installMongoCheckVersion()

  # remove trailing or leading whitespace
  queryterm <- gsub("^\\s+|\\s+$", "", queryterm)

  # initialise variable that is filled only if an update is to be made
  queryupdateterm <- ""


  ## check if we need to rerun previous query

  # check if parameters are consistent
  if ( (querytoupdate > 0) && (queryterm != "") ) warning("'query term' and 'querytoupdate' specified,",
                                                          " continuing only with new query", immediate. = TRUE)

  # get parameters for running as update
  if ( (querytoupdate > 0) && (queryterm == "") ) {
    #
    rerunparameters <- ctrRerunQuery(querytoupdate = querytoupdate,
                                     debug = debug,
                                     collection = collection, db = db, url = url,
                                     username = username, password = password, verbose = verbose,
                                     queryupdateterm = queryupdateterm)
    # set main parameters
    queryupdateterm <- rerunparameters$queryupdateterm
    queryterm       <- rerunparameters$queryterm
    register        <- rerunparameters$register
  }


  ## main function

  # parameters for core functions
  params <- list(queryterm = queryterm, register = register, querytoupdate = querytoupdate,
                 euctrresults = euctrresults,
                 details = details, parallelretrievals = parallelretrievals, debug = debug,
                 collection = collection, db = db, url = url,
                 username = username, password = password, verbose = verbose,
                 queryupdateterm = queryupdateterm)
  # call core functions
  imported <- switch(as.character(register),
                     "CTGOV" = do.call (ctrLoadQueryIntoDbCtgov, params),
                     "EUCTR" = do.call (ctrLoadQueryIntoDbEuctr, params)
  )

  ## finalise

  # return some useful information or break if not successful
  if (!exists("imported")) stop("Function did not result in any trial information imports.")
  if (debug) message("DEBUG: 'queryterm'=", queryterm,
                     "\n'queryupdateterm'=", queryupdateterm,
                     "\n'imported'=", imported,
                     "\n'register'=", register,
                     "\n'collection'=", collection)

  # add query parameters to database
  dbCTRUpdateQueryHistory(register = register,
                          queryterm = ifelse(queryupdateterm == "", queryterm, queryupdateterm),
                          recordnumber = as.integer(imported),
                          collection = collection, db = db, url = url,
                          username = username, password = password, verbose = verbose)

  # update keys database
  dbFindVariable(forceupdate = TRUE, debug = debug,
                 collection = collection, db = db, url = url,
                 username = username, password = password, verbose = verbose)

  # add metadata
  imported <- addMetaData(imported,
                          collection = collection, db = db, url = url,
                          username = username, password = password)

  ## return
  invisible(list("importedupdated" = imported))

}
# end ctrLoadQueryIntoDb





#' ctrRerunQuery
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom RCurl getCurlHandle getURL
#'
ctrRerunQuery <- function (querytoupdate = querytoupdate,
                           debug = debug,
                           collection = collection, db = db, url = url,
                           username = username, password = password, verbose = verbose,
                           queryupdateterm = queryupdateterm) {

  ## prepare

  # get history
  rerunquery <- dbQueryHistory(collection = collection, db = db, url = url,
                               username = username, password = password, verbose = verbose)

  # check parameters
  if (is.null(rerunquery)) stop("'querytoupdate': no previous queries found in collection, aborting query update.")

  # select last query if specified
  if (querytoupdate == "last") querytoupdate <- nrow(rerunquery)

  # try to select the query to be updated
  if (querytoupdate > nrow(rerunquery)) stop("'querytoupdate': specified number not found, check 'dbQueryHistory()'.")

  # set values retrieved
  rerunquery <- rerunquery[querytoupdate, ]
  queryterm  <- rerunquery$`query-term`
  register   <- rerunquery$`query-register`
  initialday <- substr(rerunquery$`query-timestamp`, start = 1, stop = 10)

  # secondary check parameters
  if (queryterm == "")                stop("Parameter 'queryterm' is empty - cannot update query ", querytoupdate)
  if (!grepl(register, "CTGOVEUCTR")) stop("Parameter 'register' not known - cannot update query ", querytoupdate)


  ## adapt updating procedure to respective register

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
      h <- RCurl::getCurlHandle(.opts = list(ssl.verifypeer = FALSE))
      rssquery <- utils::URLencode(paste0("https://www.clinicaltrialsregister.eu/ctr-search/rest/feed/bydates?query=",
                                          queryterm))
      if (debug) message("DEBUG (rss url): ", rssquery)
      #
      resultsRss <- RCurl::getURL(rssquery, curl = h)
      if (debug) message("DEBUG (rss content): ", resultsRss)
      #
      # extract euctr number(s)
      resultsRssTrials <- gregexpr("eudract_number:[0-9]{4}-[0-9]{6}-[0-9]{2}</link>", resultsRss)[[1]]
      resultsRssTrials <- sapply(resultsRssTrials, FUN = function (x) substr(resultsRss, x + 15, x + 28))
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
  }

  ## return main parameters needed
  return(data.frame("queryupdateterm" = queryupdateterm,
                    "queryterm"       = queryterm,
                    "register"        = register,
                    stringsAsFactors = FALSE))

}




#' progressOut
#'
#' @keywords internal
#
progressOut <- function(down, up) {
  #
  # helper function to show progress while downloading
  #
  if (stats::runif(1) < 0.001) message(".", appendLF = FALSE)
  #
}






#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom jsonlite toJSON
#'
dbCTRUpdateQueryHistory <- function(register, queryterm, recordnumber,
                                    collection = collection, db = db, url = url,
                                    username = username, password = password, verbose = verbose,
                                    mongo = mongo){

  # debug
  if (verbose) message("Running dbCTRUpdateQueryHistory ...")

  # retrieve existing history data
  hist <- suppressMessages(
    dbQueryHistory(collection = collection, db = db, url = url,
                   username = username, password = password, verbose = FALSE)
  )

  # debug
  if (verbose) print(hist)

  # append current search
  hist <- rbind(hist, cbind ("query-timestamp" = format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
                             "query-register"  = register,
                             "query-records"   = recordnumber,
                             "query-term"      = queryterm))

  # collate information about current query into json object
  json <- jsonlite::toJSON(list("queries" = hist))

  # debug
  if (verbose) cat(json)

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = TRUE)[["ctr"]]

  # update database
  mongo$update(query = '{"_id":{"$eq":"meta-info"}}',
               update = paste0('{ "$set" :', json, "}"),
               upsert = TRUE)

  # inform user
  message("Updated history.")
  #
}
# end dbCTRUpdateQueryHistory



#' ctrLoadQueryIntoDbCtgov
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
#' @importFrom RCurl getCurlHandle close curlPerform CFILE
#'
ctrLoadQueryIntoDbCtgov <- function(queryterm, register, querytoupdate,
                                    euctrresults,
                                    details, parallelretrievals, debug,
                                    collection, db, url,
                                    username, password, verbose,
                                    queryupdateterm) {

  ## check availability of relevant helper programs
  if (!suppressWarnings(installFindBinary("php --version")))                          stop("php not found.")
  if (!suppressWarnings(installFindBinary("php -r 'simplexml_load_string(\"\");'")))  stop("php xml not found.")

  ## create empty temporary directory on localhost for
  # downloading from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)

  # CTGOV standard identifiers
  # updated 2017-07 with revised ctgov website links, e.g.
  # https://clinicaltrials.gov/ct2/results/download_studies?rslt=With&cond=Neuroblastoma&age=0&draw=3
  queryUSRoot   <- "https://clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download_studies?"

  ## inform user and prepare url for downloading
  message("Downloading trials from CTGOV as xml ", appendLF = FALSE)
  ctgovdownloadcsvurl <- paste0(queryUSRoot, queryUSType1, "&", queryterm, queryupdateterm)
  if (debug) message ("DEBUG: ", ctgovdownloadcsvurl)

  # prepare a file handle for saving in temporary directory
  f <- paste0(tempDir, "/ctgov.zip")

  # get (download) trials in single zip file
  h    <- RCurl::getCurlHandle(.opts = list(ssl.verifypeer = FALSE)) # avoid certificate failure from outside EU
  fref <- RCurl::CFILE(f, mode = "wb")
  tmp  <- RCurl::curlPerform(url = utils::URLencode(ctgovdownloadcsvurl),
                             writedata = fref@ref,
                             noprogress = FALSE,
                             progressfunction = progressOut,
                             curl = h)

  # close file handle
  RCurl::close(fref)
  message("")

  # inform user
  if (file.size(f) == 0) stop("No studies downloaded. Please check 'queryterm' or run again with debug = TRUE.")

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## compose commands to transform xml into json, into
  # a single allfiles.json in the temporaray directory
  xml2json <- system.file("exec/xml2json.php", package = "ctrdata", mustWork = TRUE)
  xml2json <- paste0("php -f ", xml2json, " ", tempDir)
  json2mongo <- paste0(' --host="', sub("mongodb://(.+)", "\\1", url),
                       '" --db="', db, '" --collection="', collection, '"',
                       ifelse(username != "", paste0(' --username="', username, '"'), ""),
                       ifelse(password != "", paste0(' --password="', password, '"'), ""),
                       ' --upsert --type=json --file="', tempDir, '/allfiles.json"',
                       ifelse(installMongoCheckVersion(), "", " --jsonArray"))

  # special command handling on windows
  if (.Platform$OS.type == "windows") {
    # xml2json requires cygwin's php. transform paths for cygwin use:
    xml2json <- gsub("\\\\", "/", xml2json)
    xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
    xml2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', xml2json, '"')
    #
    json2mongo <- gsub(" --", " /", json2mongo)
    json2mongo <- gsub("=", ":", json2mongo)
    #
  } # if windows

  # run conversion of downloaded xml to json
  message("Converting to JSON ...")
  if (debug) message("DEBUG: ", xml2json)
  imported <- system(xml2json, intern = TRUE)

  ## run import
  message("Importing JSON into mongoDB ...")
  if (debug) message("DEBUG: ", json2mongo)
  imported <- system2(command = installMongoFindBinaries(debug = debug)[2],
                      args = json2mongo,
                      stdout = TRUE, stderr = TRUE)

  ## absorb id_info array into new array otherids
  # "id_info" : {
  #   "org_study_id" : "P9971",
  #   "secondary_id" : [
  #     "COG-P9971",
  #     "CDR0000068102"
  #     ],
  #   "nct_id" : "NCT00006095"}
  # to
  # "otherids" : [
  #   "ADVL0011",
  #   "COG-ADVL0011",
  #   "CDR0000068036"
  #   ]
  # TODO: should this be undone in order
  # to fully maintain ctgov schema?

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = FALSE)[["ctr"]]

  # obtain full data set on _id and other ids
  cursor <- mongo$iterate(query  = '{"_id": {"$regex": "NCT[0-9]{8}"}}',
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
  # add index for newly created fired
  mongo$index(add = "otherids")
  message('Added index field "otherids".')

  # close database connection
  rm(mongo); gc()


  ## find out number of trials imported into database
  if (debug) message("DEBUG: ", imported)
  imported <- as.integer(gsub(".*imported ([0-9]+) document.*", "\\1", imported[length(imported)]))
  if (!is.numeric(imported)) stop("Import has apparently failed, returned ", imported)
  message("Imported or updated ", imported, " trial(s).")

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
#' @importFrom RCurl getCurlHandle getURL
#'
ctrLoadQueryIntoDbEuctr <- function(queryterm, register, querytoupdate,
                                    euctrresults,
                                    details, parallelretrievals, debug,
                                    collection, db, url,
                                    username, password, verbose,
                                    queryupdateterm) {

  # check availability of relevant helper programs
  if (!suppressWarnings(installFindBinary("echo x | sed s/x/y/"))) stop("sed not found.")
  if (!suppressWarnings(installFindBinary("perl -V:osname")))      stop("perl not found.")

  # inform user
  message("Downloading trials from EUCTR:", appendLF = TRUE)

  # create empty temporary directory on localhost for
  # download from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)

  # EUCTR standard identifiers
  queryEuRoot  <- "https://www.clinicaltrialsregister.eu/"
  queryEuType1 <- "ctr-search/search?query="
  queryEuType2 <- "ctr-search/rest/download/summary?query="
  queryEuType3 <- "ctr-search/rest/download/full?query="
  queryEuType4 <- "ctr-search/rest/download/result/zip/xml/"
  queryEuPost  <- "&mode=current_page&format=text&dContent=summary&number=current_page&submit-download=Download"

  # get first result page
  h <- RCurl::getCurlHandle(.opts = list(ssl.verifypeer = FALSE)) # avoid certificate failure from outside EU
  q <- utils::URLencode(paste0(queryEuRoot, queryEuType1, queryterm))
  if (debug) message("DEBUG: queryterm is ", q)
  resultsEuPages <- RCurl::getURL(q, curl = h)

  # get number of trials identified by query
  resultsEuNumTrials <- sub(".*Trials with a EudraCT protocol \\(([0-9,.]*)\\).*", "\\1", resultsEuPages)
  resultsEuNumTrials <- suppressWarnings(as.numeric(gsub("[,.]", "", resultsEuNumTrials)))

  # calculate number of results pages
  resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20)

  # check for plausbility
  if (is.na(resultsEuNumPages) || is.na(resultsEuNumTrials) || resultsEuNumTrials == 0)
    stop("First result page empty - no (new) trials found?")

  # inform user
  message("Retrieved overview, ", resultsEuNumTrials, " trial(s) from ",
          resultsEuNumPages, " page(s) are to be downloaded.")

  # calculate batches to get data from all results pages
  resultsNumBatches <- resultsEuNumPages %/% parallelretrievals
  resultsNumModulo  <- resultsEuNumPages %%  parallelretrievals
  message("Downloading trials (from a maximum of ", parallelretrievals, " page(s) in parallel):")

  # iterate over batches of results pages
  for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

    # parallel requests by using startpage:stoppage
    # TODO use queue and re-queueing
    startpage <- (i - 1) * parallelretrievals + 1
    stoppage  <- ifelse(i > resultsNumBatches,
                        startpage + resultsNumModulo,
                        startpage + parallelretrievals) - 1
    message("(", i, ") ", startpage, "-", stoppage, " ", appendLF = FALSE)

    # download data for current batch into variable
    tmp <- RCurl::getURL(utils::URLencode(paste0(queryEuRoot, ifelse(details, queryEuType3, queryEuType2),
                                                 queryterm, "&page=", startpage:stoppage, queryEuPost)),
                         curl = h, async = TRUE, binary = FALSE, noprogress = FALSE, progressfunction = progressOut)
    message("")

    # check plausibility
    if (debug) message("DEBUG: ", class(tmp))
    if (class(tmp) != "character") stop("Download of records from EUCTR failed; last error: ", class(tmp))

    # save downloaded data from variable into file system
    for (ii in startpage:stoppage)
      write(tmp[[1 + ii - startpage]],
            paste0(tempDir, "/euctr-trials-page_",
                   formatC(ii, digits = 0, width = nchar(resultsEuNumPages), flag = 0), ".txt"))
  } # for batch

  # compose commands: for external script on all files in temporary directory and for import
  euctr2json <- system.file("exec/euctr2json.sh", package = "ctrdata", mustWork = TRUE)
  euctr2json <- paste(euctr2json, tempDir)
  json2mongo <- paste0(' --host="', sub("mongodb://(.+)", "\\1", url),
                       '" --db="', db, '" --collection="', collection, '"',
                       ifelse(username != "", paste0(' --username="', username, '"'), ""),
                       ifelse(password != "", paste0(' --password="', password, '"'), ""),
                       ' --upsert --type=json --file="', tempDir, '/allfiles.json"',
                       ifelse(installMongoCheckVersion(), "", " --jsonArray"))

  # special handling in case of windows
  if (.Platform$OS.type == "windows") {
    #
    # euctr2json requires cygwin's perl, sed. transform paths for cygwin use
    euctr2json <- gsub("\\\\", "/", euctr2json)
    euctr2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", euctr2json)
    euctr2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', euctr2json, '"')
    #
    json2mongo <- gsub(" --", " /", json2mongo)
    json2mongo <- gsub("=", ":", json2mongo)
    #
  }

  # run conversion of text files saved into file system to json file
  message("Converting to JSON ...")
  if (debug) message("DEBUG: ", euctr2json)
  imported <- system(euctr2json, intern = TRUE)

  # run fast import into mongo from single json file
  message("Importing JSON into mongoDB ...")
  if (debug) message("DEBUG: ", json2mongo)
  imported <- system2(command = installMongoFindBinaries(debug = TRUE)[2],
                      args = json2mongo, stdout = TRUE, stderr = TRUE)

  # identify number of trials imported into database
  imported <- as.integer(gsub(".*imported ([0-9]+) document.*", "\\1", imported[length(imported)]))


  # find out if fast import was successful
  if ( (!is.numeric(imported)) || (imported < resultsEuNumTrials) || (debug & !verbose) ) {

    # if not successful, switch to SLOW IMPORT
    warning("Switching to slow import because mongoimport as single JSON file failed.", immediate. = TRUE)

    # compose command
    json2split <- system.file("exec/json2split.sh", package = "ctrdata", mustWork = TRUE)
    json2split <- paste(json2split, tempDir)

    # special handling of command on windows
    if (.Platform$OS.type == "windows") {
      #
      json2split <- gsub("\\\\", "/", json2split)
      json2split <- gsub("([A-Z]):/", "/cygdrive/\\1/", json2split)
      json2split <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', json2split, '"')
      #
    } # if windows

    # run json2split, to split single json file
    # into one json file for each trial record
    message("Splitting into JSON files ...")
    if (debug) message("DEBUG: ", json2split)
    imported  <- system(json2split, intern = TRUE)
    splitjson <- try(as.numeric(imported))
    if (class(splitjson) == "try-error") stop("Splitting single JSON files failed. Aborting ctrLoadQueryIntoDb.")

    # now import single-trial json files one by one, record and print failed trial ids
    message("Importing ", max(splitjson), " individual JSON files ...")
    allimported <- 0
    for (i in 1:splitjson) {
      # TODO: loop / parallelise
      # compose command
      json2split2mongo <- sub("allfiles.json", paste0("allfiles-", i, ".json"), json2mongo)
      # run import command
      imported <- system2(command = installMongoFindBinaries(debug = TRUE)[2],
                          args = json2split2mongo, stdout = TRUE, stderr = TRUE)
      # identify if successful result or not
      imported <- as.integer(gsub("^.*imported ([0-9]+) document[s]{0,1}$", "\\1", imported[length(imported)]))
      # check for failed trial
      if (!is.numeric(imported) || imported == 0) {
        # get failed trial
        trialfirstline <- readLines(gsub("^.+\"(.+?\\.json).+$", "\\1", json2split2mongo), n = 1, warn = FALSE)
        trialfirstline <- gsub("^.+([0-9]{4}-[0-9]{6}-[0-9]{2}).+$", "\\1", trialfirstline)
        # inform user
        warning(paste0("Import into mongoDB failed for trial ", trialfirstline), immediate. = TRUE)
      } else {
        # inform on successful trial
        if (debug) message("DEBUG: ", paste0("allfiles-", i, ".json"), " successfully imported.")
        allimported <- allimported + imported
      }
      # progress indicator
      if (!debug) cat(".")
    }
    # updated main indicator of number of imported trials
    imported <- allimported
  } # if fast import not successful

  ## inform user on final import outcome
  message("Imported or updated ", imported, " records on ", resultsEuNumTrials, " trial(s).")



  ## results: load also euctr trials results if requested
  if(euctrresults) {

    ## results are available only one-by-one for each trial
    #  we need the eudract numbers of the trials that were
    #  just retrieved and imported
    # for debugging:
    # tempDir <- "/Users/ralfherold/Daten/mak/r/emea/ctrdata/private/test"
    eudractnumbersimported <- readLines(paste0(tempDir, '/alleudract.txt'))
    eudractnumbersimported <- rev(sort(unique(eudractnumbersimported)))

    # for debugging:
    # eudractnumbersimported <- c("2004-000518-37", "2007-000371-42", "2004-004386-15", "2007-000371-42")

    # inform user
    message("\nDownloading EUCTR results for ", length(eudractnumbersimported), " trials: ")


    ## parallel download and unzipping into temporary directory

    # conventions
    # prefix: "https://www.clinicaltrialsregister.eu/ctr-search/rest/download/result/zip/xml/"
    # first version:  "2007-000371-42/1"
    # second version: "2007-000371-42/2"
    # last version:   "2007-000371-42"

    # calculate batches to get data from all results pages
    resultsNumBatches <- length(eudractnumbersimported) %/% parallelretrievals
    resultsNumModulo  <- length(eudractnumbersimported) %%  parallelretrievals

    # inform user
    message("Downloading trial results (at a maximum of ", parallelretrievals, " files in parallel):")

    # initialise
    h    <- RCurl::getCurlHandle(.opts = list(ssl.verifypeer = FALSE)) # avoid certificate failure from outside EU

    # iterate over batches of results
    for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

      # calculated indices for eudractnumbersimported vector
      startindex <- (i - 1) * parallelretrievals + 1
      stopindex  <- ifelse(i > resultsNumBatches,
                           startindex + resultsNumModulo,
                           startindex + parallelretrievals) - 1

      tmp <- sapply(eudractnumbersimported[startindex : stopindex],
                    function(x) {

                      # prepare a file handle for saving in temporary directory
                      f <- paste0(tempDir, "/", x , ".zip")
                      fref <- RCurl::CFILE(f, mode = "wb")

                      # get (download) trial results' zip file
                      tmp  <- RCurl::curlPerform(url = utils::URLencode(paste0(queryEuRoot, queryEuType4, x)),
                                                 writedata = fref@ref,
                                                 noprogress = FALSE,
                                                 progressfunction = progressOut,
                                                 curl = h)

                      # close file handle
                      RCurl::close(fref)

                      # unzip downloaded file and rename
                      if (file.size(f) != 0) {

                        tmp <- utils::unzip(f, exdir = tempDir)

                        if(any(grepl("pdf$", tmp)))
                          warning("PDF results ", x,
                                  call. = FALSE, immediate. = TRUE, noBreaks. = FALSE)

                        if(any(tmp2 <- grepl("xml$", tmp)))
                          file.rename(tmp[tmp2][1], paste0(tempDir, "/", x , ".xml"))

                      }

                      # inform user
                      if (file.size(f) == 0) warning("No results found for ", x,
                                                     call. = FALSE, immediate. = TRUE, noBreaks. = FALSE)

                      # clean up
                      if (!debug) unlink(f)

                    }) # download, unzip, save

      # inform user
      message("Batch: ", i, ", ", startindex, " - ", stopindex)

    } # for batch


    ## use system commands to convert
    ## xml to json and to import json

    # compose command
    xml2json <- system.file("exec/xml2json_euctrresults.php", package = "ctrdata", mustWork = TRUE)
    xml2json <- paste0("php -f ", shQuote(xml2json), " ", shQuote(tempDir)) # TODO: use shQuote in all places

    # special command handling on windows
    if (.Platform$OS.type == "windows") {
      # xml2json_euctrresults requires cygwin's php. transform paths for cygwin use:
      xml2json <- gsub("\\\\", "/", xml2json)
      xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
      xml2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', xml2json, '"')
      #
    } # if windows

    # run conversion of downloaded xml to json
    message("Converting to JSON ...")
    if (debug) message("DEBUG: ", xml2json)
    importedresults <- system(xml2json, intern = TRUE)


    # get a working mongo connection, select trial record collection
    mongo <- ctrMongo(collection = collection, db = db, url = url,
                      username = username, password = password, verbose = FALSE)[["ctr"]]

    # iterate over batches of results
    importedresults <- NULL
    for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {

      # calculated indices for eudractnumbersimported vector
      startindex <- (i - 1) * parallelretrievals + 1
      stopindex  <- ifelse(i > resultsNumBatches,
                           startindex + resultsNumModulo,
                           startindex + parallelretrievals) - 1

      tmp <- sapply(eudractnumbersimported[startindex : stopindex],
                    function(x) {

                      # compose file name and check
                      # for debugging:
                      # x <- "2004-000518-37"
                      fileName <- paste0(tempDir, "/", x, ".json")
                      if(file.exists(fileName) && file.size(fileName) > 0){

                        # read contents
                        tmp <- readChar(fileName, file.info(fileName)$size)

                        # update database with results
                        tmp <- mongo$update(query  = paste0('{"a2_eudract_number":{"$eq":"', x, '"}}'),
                                            update = paste0('{ "$set" :', tmp, "}"),
                                            upsert = TRUE, multiple = TRUE)

                        # inform user on failed trial
                        if (!tmp) warning(paste0("Import into mongo failed for trial ", x), immediate. = TRUE)

                      } else {

                        # file did not exist
                        tmp <- FALSE

                      }

                      # return for accumulating information
                      return(tmp)

                    }) # import

      # accumulate
      importedresults <- c(importedresults, tmp)

    } # for batch

    # close database connection
    rm(mongo); gc()

    # sum up successful downloads
    importedresults <- sum(unlist(importedresults))

    ## inform user on final import outcome
    message("Imported or updated results for ", importedresults, " out of ", resultsEuNumTrials, " trial(s).\n")

  } # if euctrresults


  # clean up temporary directory
  if (!debug) unlink(tempDir, recursive = TRUE)

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr
