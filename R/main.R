### ctrdata package
### main functions


#' Retrieve or update information on clinical trials from register and store in
#' database
#'
#' Note that upsert is used to store in database, which means that records may
#' be accumulated. If you want to insert into an empty database, you have to
#' include a mongo connection object to such an empty database.
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
ctrLoadQueryIntoDb <- function(queryterm = "", register = "EUCTR", querytoupdate = 0,
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
    queryterm <- ctrGetQueryUrlFromBrowser(queryterm)
    #
  }

  # deal with data frame as returned from ctrQueryHistoryInDb() and ctrGetQueryUrlFromBrowser()
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

  # sanity checks
  if (grepl("[^a-zA-Z0-9=+&%_-]", gsub("\\[", "", gsub("\\]", "", queryterm))))
    stop("Parameter 'queryterm' is not an URL showing results of a query or has unexpected characters: ",
         queryterm, ", expected are: a-zA-Z0-9=+&%_-[].")
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

  # ctgov core parameters
  params <- list(queryterm = queryterm, register = register, querytoupdate = querytoupdate,
                 details = details, parallelretrievals = parallelretrievals, debug = debug,
                 collection = collection, db = db, url = url,
                 username = username, password = password, verbose = verbose,
                 queryupdateterm = queryupdateterm)
  # ctgov core functions
  imported <- switch(as.character(register),
                     "CTGOV" = do.call (ctrLoadQueryIntoDbCtgov, params),
                     "EUCTR" = do.call (ctrLoadQueryIntoDbEuctr, params)
  )


  ## finalise

  # return some useful information
  if (!exists("imported")) stop("Function did not result in any trial information imports.")

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
#
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
      rssquery <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/rest/feed/bydates?query=", queryterm)
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
  if (stats::runif(1) < 0.001) cat(".")
  #
}






#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#
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
#
ctrLoadQueryIntoDbCtgov <- function(queryterm, register, querytoupdate,
                                    details, parallelretrievals, debug,
                                    collection, db, url,
                                    username, password, verbose,
                                    queryupdateterm) {

  # check availability of relevant helper programs
  if (!suppressWarnings(installFindBinary("php --version")))                          stop("php not found.")
  if (!suppressWarnings(installFindBinary("php -r 'simplexml_load_string(\"\");'")))  stop("php xml not found.")

  # create empty temporary directory on localhost for
  # download from register into temporary directy
  tempDir <- tempfile(pattern = "ctrDATA")
  dir.create(tempDir)

  # CTGOV standard identifiers
  queryUSRoot   <- "https://clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download?"
  queryUSPreXML <- "&down_stds=all&down_typ=study&down_flds=all&down_fmt=xml"
  # next line to include any available result-related information within the XML
  # queryUSPreXML <- "down_stds=all&down_typ=results&down_flds=all&down_fmt=plain"
  queryUSPost   <- "&show_down=Y"

  # example: condition ependymoma, children, interventional study, added or modified from 1 Dec 2014 onwards:
  #
  # https://clinicaltrials.gov/ct2/results?term=&recr=&rslt=&type=Intr&cond=ependymoma&intr=&titles=&
  # outc=&spons=&lead=&id=&state1=&cntry1=&state2=&cntry2=&state3=&cntry3=&locn=&gndr=&age=0&rcv_s=&rcv_e=&
  # lup_s=12%2F01%2F2014&lup_e=
  #
  # "Download Selected Fields", all fields, all studies, comma separated format:
  #
  # https://clinicaltrials.gov/ct2/results/download?down_stds=all&down_typ=fields&down_flds=all&down_fmt=csv ||
  # &type=Intr&cond=ependymoma&age=0&lup_s=12%2F01%2F2014& || show_down=Y
  #
  # "Download All Study Fields as XML", all studies:
  #
  # https://clinicaltrials.gov/ct2/results/download?down_stds=all&down_typ=study&down_flds=all&down_fmt=xml ||
  # &type=Intr&cond=ependymoma&age=0&lup_s=12%2F01%2F2014& || show_down=Y

  # CTGOV field names - use NCT for mongodb index
  fieldsCTGOV  <- c("Rank", "NCT Number", "Title", "Recruitment", "Study Results", "Conditions", "Interventions",
                    "Sponsor/Collaborators", "Gender", "Age Groups", "Phases", "Enrollment", "Funded Bys",
                    "Study Types", "Study Designs", "Other IDs", "First Received", "Start Date", "Completion Date",
                    "Last Updated", "Last Verified", "Results First Received", "Acronym", "Primary Completion Date",
                    "Outcome Measures", "URL")
  fieldsCTGOV <- sub("NCT Number", "_id", fieldsCTGOV)
  write(fieldsCTGOV, paste0(tempDir, "/field_names.txt"))

  # get result file and unzip into folder
  message("Downloading trials from CTGOV as xml ", appendLF = FALSE)
  ctgovdownloadcsvurl <- paste0(queryUSRoot, queryUSType1, queryUSPreXML, "&",
                                queryterm, queryupdateterm, queryUSPost)
  if (debug) message ("DEBUG: ", ctgovdownloadcsvurl)
  #
  f <- paste0(tempDir, "/ctgov.zip")
  #
  h    <- RCurl::getCurlHandle(.opts = list(ssl.verifypeer = FALSE)) # avoid certificate failure from outside EU
  fref <- RCurl::CFILE(f, mode = "wb")
  tmp  <- RCurl::curlPerform(url = ctgovdownloadcsvurl,
                             writedata = fref@ref,
                             noprogress = FALSE,
                             progressfunction = progressOut,
                             curl = h)
  RCurl::close(fref)
  #
  if (file.size(f) == 0) stop("No studies downloaded. Please check 'queryterm' or run again with debug = TRUE.")
  utils::unzip(f, exdir = tempDir)

  # compose commands - transform xml into json, a single allfiles.json in the temporaray directory
  xml2json <- system.file("exec/xml2json.php", package = "ctrdata", mustWork = TRUE)
  xml2json <- paste0("php -f ", xml2json, " ", tempDir)
  json2mongo <- paste0(' --host="', sub("mongodb://(.+)", "\\1", url),
                       '" --db="', db, '" --collection="', collection, '"',
                       ifelse(username != "", paste0(' --username="', username, '"'), ""),
                       ifelse(password != "", paste0(' --password="', password, '"'), ""),
                       ' --upsert --type=json --file="', tempDir, '/allfiles.json"',
                       ifelse(installMongoCheckVersion(), "", " --jsonArray"))

  if (.Platform$OS.type == "windows") {
    # xml2json requires cygwin's php. transform paths for cygwin use:
    xml2json <- gsub("\\\\", "/", xml2json)
    xml2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", xml2json)
    xml2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', xml2json, '"')
    #
    json2mongo <- gsub(" --", " /", json2mongo)
    json2mongo <- gsub("=", ":", json2mongo)
    #
  }
  #
  # run transformation
  message("Converting to JSON ...")
  if (debug) message("DEBUG: ", xml2json)
  imported <- system(xml2json, intern = TRUE, show.output.on.console = FALSE)

  # run import
  message("Importing JSON into mongoDB ...")
  if (debug) message("DEBUG: ", json2mongo)
  imported <- system2(command = installMongoFindBinaries(debug = debug)[2],
                      args = json2mongo,
                      stdout = TRUE, stderr = TRUE)

  # absorb id_info array into new array otherids for later perusal
  #
  # "id_info" : {
  #   "org_study_id" : "P9971",
  #   "secondary_id" : [
  #     "COG-P9971",
  #     "CDR0000068102"
  #     ],
  #   "nct_id" : "NCT00006095"
  #
  # to
  #
  # "otherids" : [
  #   "ADVL0011",
  #   "COG-ADVL0011",
  #   "CDR0000068036"
  #   ]
  #

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = TRUE)[["ctr"]]

  # obtain full data set on _id and other ids
  cursor <- mongo$iterate(query = '{"_id": {"$regex": "NCT[0-9]{8}"}}',
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

  # find out number of trials imported into database
  if (debug) message("DEBUG: ", imported)
  imported <- as.integer(gsub(".*imported ([0-9]+) document.*", "\\1", imported[length(imported)]))
  if (!is.numeric(imported)) stop("Import has apparently failed, returned ", imported)
  message("Imported or updated ", imported, " trial(s).")

  # clean up temporary directory
  if (!debug) unlink(tempDir, recursive = TRUE)

  # add query parameters to database
  dbCTRUpdateQueryHistory(register = register, queryterm = queryterm, recordnumber = imported,
                          collection = collection, db = db, url = url,
                          username = username, password = password, verbose = verbose)

  # update keys database
  dbFindVariable(forceupdate = TRUE, debug = debug,
                 collection = collection, db = db, url = url,
                 username = username, password = password, verbose = verbose)

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtgov









#' ctrLoadQueryIntoDbEuctr
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#
ctrLoadQueryIntoDbEuctr <- function(queryterm, register, querytoupdate,
                                    details, parallelretrievals, debug,
                                    collection, db, url,
                                    username, password, verbose,
                                    queryupdateterm) {

  # check availability of relevant helper programs
  if (!suppressWarnings(installFindBinary("echo x | sed s/x/y/"))) stop("sed not found.")
  if (!suppressWarnings(installFindBinary("perl -V:osname")))      stop("perl not found.")

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
  queryEuPost  <- "&mode=current_page&format=text&dContent=summary&number=current_page&submit-download=Download"

  # get first result page
  h <- RCurl::getCurlHandle(.opts = list(ssl.verifypeer = FALSE)) # avoid certificate failure from outside EU
  q <- paste0(queryEuRoot, queryEuType1, queryterm)
  if (debug) message("DEBUG: queryterm is ", q)
  resultsEuPages <- RCurl::getURL(q, curl = h)
  resultsEuNumTrials <- sub(".*Trials with a EudraCT protocol \\(([0-9,.]*)\\).*", "\\1", resultsEuPages)
  resultsEuNumTrials <- suppressWarnings(as.numeric(gsub("[,.]", "", resultsEuNumTrials)))
  resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20) # alternative: parsing "next" and "last" links
  #
  if (is.na(resultsEuNumPages) || is.na(resultsEuNumTrials) || resultsEuNumTrials == 0)
    stop("First result page empty - no (new) trials found?")
  #
  message("Retrieved overview, ", resultsEuNumTrials, " trial(s) from ",
          resultsEuNumPages, " page(s) are to be downloaded.")

  # get data
  resultsNumBatches <- resultsEuNumPages %/% parallelretrievals
  resultsNumModulo  <- resultsEuNumPages %%  parallelretrievals
  message("Downloading trials (from a maximum of ", parallelretrievals, " page(s) in parallel):")
  #
  for (i in 1:(resultsNumBatches + ifelse(resultsNumModulo > 0, 1, 0))) {
    # parallel requests by using startpage:stoppage
    # TODO use queue and re-queueing
    startpage <- (i - 1) * parallelretrievals + 1
    stoppage  <- ifelse(i > resultsNumBatches, startpage + resultsNumModulo, startpage + parallelretrievals) - 1
    message("(", i, ") ", startpage, "-", stoppage, " ", appendLF = FALSE)
    #
    tmp <- RCurl::getURL(paste0(queryEuRoot, ifelse(details, queryEuType3, queryEuType2), queryterm,
                                "&page=", startpage:stoppage, queryEuPost),
                         curl = h, async = TRUE, binary = FALSE, noprogress = FALSE, progressfunction = progressOut)
    #
    if (debug) message("DEBUG: ", class(tmp))
    if (class(tmp) != "character") stop("Download of records from EUCTR failed; last error: ", class(tmp))
    #
    for (ii in startpage:stoppage)
      write(tmp[[1 + ii - startpage]],
            paste0(tempDir, "/euctr-trials-page_",
                   formatC(ii, digits = 0, width = nchar(resultsEuNumPages), flag = 0), ".txt"))
  }

  # compose commands: for external script on all files in temporary directory and for import
  euctr2json <- system.file("exec/euctr2json.sh", package = "ctrdata", mustWork = TRUE)
  euctr2json <- paste(euctr2json, tempDir)
  json2mongo <- paste0(' --host="', sub("mongodb://(.+)", "\\1", url),
                       '" --db="', db, '" --collection="', collection, '"',
                       ifelse(username != "", paste0(' --username="', username, '"'), ""),
                       ifelse(password != "", paste0(' --password="', password, '"'), ""),
                       ' --upsert --type=json --file="', tempDir, '/allfiles.json"',
                       ifelse(installMongoCheckVersion(), "", " --jsonArray"))
  #
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

  # run conversion
  message("Converting to JSON ...")
  if (debug) message("DEBUG: ", euctr2json)
  imported <- system(euctr2json, intern = TRUE, show.output.on.console = FALSE)

  # run import
  message("Importing JSON into mongoDB ...")
  if (debug) message("DEBUG: ", json2mongo)
  imported <- system2(command = ctrdata:::installMongoFindBinaries(debug = TRUE)[2],
                      args = json2mongo, stdout = TRUE, stderr = TRUE)

  # find out number of trials imported into database
  imported <- as.integer(gsub(".*imported ([0-9]+) document.*", "\\1", imported[length(imported)]))

  # find out if fast import was successful
  if ( (!is.numeric(imported)) || (imported < resultsEuNumTrials) || (debug & !verbose) ) {
    #
    # if not successful, switch to SLOW IMPORT
    warning("Switching to slow import because mongoimport as single JSON file failed.", immediate. = TRUE)
    #
    # json2split
    json2split <- system.file("exec/json2split.sh", package = "ctrdata", mustWork = TRUE)
    json2split <- paste(json2split, tempDir)
    #
    if (.Platform$OS.type == "windows") {
      #
      json2split <- gsub("\\\\", "/", json2split)
      json2split <- gsub("([A-Z]):/", "/cygdrive/\\1/", json2split)
      json2split <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', json2split, '"')
      #
    }
    #
    message("Splitting into JSON files ...")
    if (debug) message("DEBUG: ", json2split)
    imported  <- system(json2split, intern = TRUE, show.output.on.console = FALSE)
    splitjson <- try(as.numeric(imported))
    if (class(splitjson) == "try-error") stop("Splitting single JSON files failed. Aborting ctrLoadQueryIntoDb.")

    # mongoimport: loop / parallelise? record and print failed trial ids
    message("Importing ", max(splitjson), " individual JSON files ...")
    allimported <- 0
    for (i in 1:splitjson) {
      json2split2mongo <- sub("allfiles.json", paste0("allfiles-", i, ".json"), json2mongo)
      #
      if (debug & i == 1) message("DEBUG: ", json2split2mongo)
      if (debug)          message("DEBUG: ", i)
      #
      imported <- system2(command = ctrdata:::installMongoFindBinaries(debug = TRUE)[2],
                          args = json2split2mongo, stdout = TRUE, stderr = TRUE)
      #
      imported <- as.integer(gsub("^.*imported ([0-9]+) document[s]{0,1}$", "\\1", imported[length(imported)]))
      #
      if (!is.numeric(imported) || imported == 0) {
        # get failed trial
        trialfirstline <- readLines(gsub("^.+\"(.+?\\.json).+$", "\\1", json2split2mongo), n = 1, warn = FALSE)
        trialfirstline <- gsub("^.+([0-9]{4}-[0-9]{6}-[0-9]{2}).+$", "\\1", trialfirstline)
        # inform user
        warning(paste0("Import into mongoDB failed for trial ", trialfirstline), immediate. = TRUE)
        cat()
      } else {
        if (debug) message("DEBUG: ", paste0("allfiles-", i, ".json"), " successfully imported.")
        allimported <- allimported + imported
      }
      if (!debug) cat(".")
    }
    imported <- allimported
  }

  # inform user on final import outcome
  message("Imported or updated ", imported, " records on ", resultsEuNumTrials, " trial(s).")

  # clean up temporary directory
  if (!debug) unlink(tempDir, recursive = TRUE)

  # add query parameters to database
  if (debug) message("DEBUG: 'queryterm'=", queryterm, ", 'queryupdateterm'=", queryupdateterm)
  dbCTRUpdateQueryHistory(register = register,
                          queryterm = ifelse(queryupdateterm == "", queryterm, queryupdateterm),
                          recordnumber = imported,
                          collection = collection, db = db, url = url,
                          username = username, password = password, verbose = verbose)

  # update keys database
  dbFindVariable(forceupdate = TRUE, debug = debug,
                 collection = collection, db = db, url = url,
                 username = username, password = password, verbose = verbose)

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbEuctr
