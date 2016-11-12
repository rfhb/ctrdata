### ctrdata package
### utility functions


## variable definitions
#
# EUCTR definitions
agegroupsEUCTR <- c("Preterm newborn infants", "Newborns", "Infants and toddlers", "Children", "Adolescents",
                    "Under 18", "Adults", "Elderly")
variablesEUCTR <- c("EudraCT Number", "Sponsor Protocol Number", "Sponsor Name", "Full Title", "Start Date",
                    "Medical condition", "Disease", "Population Age", "Gender", "Trial protocol", "Link")
countriesEUCTR <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT",
                    "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB")
#
# non-exported function(s)
# is.queryterm <- function(x) {
#   # check for valid characters. to avoid problems with the range in the grepl
#   # test, replace square brackets in the string to be checked
#   if (grepl('[^a-zA-Z0-9=+&%_-]', gsub('\\[', '', gsub('\\]', '', x))))
#     stop('Unexpected characters in "', x, '", expected are: a-zA-Z0-9=+&%_-[].')
#   return(NULL)
# }


#' This is a function to select and use
#' a connection to a Mongo DB server.
#'
#' @param mongo A mongo data base object, currently using mongolite. Default
#' is an empty string, which would make this function to result in a default
#' Mongo DB connection, currently using mongolite with collection ctrdata in
#' database users on server localhost, unless there is a monoglite data base
#' object name "mongo" defined in the global environment.
#'
#' @return A mongo data base object, currently using mongolite
#'
#' @keywords internal
#'
ctrMongo <- function(mongo = "") {

  # if parameter is empty, test and use global environment
  if(class(mongo) == "character" && mongo == "" && ("mongo" %in% objects(envir = .GlobalEnv))) {

    # inform user
    message("Using object \"mongo\" defined in global environment!")

    mongo <- get("mongo", envir = .GlobalEnv)

  }

  # check parameter
  if(class(mongo) == "character" && mongo != "") stop("Cannot use \"", mongo, "\", check parameter mongo.")

  # use acceptable parameter
  if(identical(class(mongo), c("mongo", "jeroen", "environment"))) {

    # We already have a database connection

    # inform user
    message("Using Mongo DB database.collection: \"", mongo$info()$stats$ns, "\".")

    # mongolite will automatically reconnect
    return(mongo)

  } else {

    # set up default database connection
    host <- "127.0.0.1:27017"
    username <- ""
    password <- ""
    collection <- "ctrdata"
    db <- "users"
    # not used: options = ssl_options()

    # url: mongodb://[username:password@]host1[:port1]
    mongourl <- paste0("mongodb://",
                       ifelse(username != "", username, ""),
                       ifelse(password != "", paste0(":", password), ""),
                       ifelse(username != "", "@", ""),
                       host)

    value <- mongolite::mongo(collection = collection, db = db, url = mongourl, verbose = FALSE)

    # inform user
    message("Using default Mongo DB (collection \"", collection, "\" in database \"", db, "\" on \"", host, "\").")

    return(value)
  }
}
# end ctrMongo


#' Open advanced search pages of register(s) or execute search in default web
#' browser.
#'
#' @param register Register(s) to open. Either "EUCTR" or "CTGOV" or a vector of
#'   both. Default is to open both registers' advanced search pages. To open the
#'   browser with a previous search, register (or queryterm) can be the output
#'   of ctrGetQueryUrlFromBrowser() or can be one row from
#'   dbQueryHistory().
#' @param copyright (Optional) If set to \code{TRUE}, opens copyright pages of
#'   register(s).
#' @param queryterm (Optional) Show results of search for \code{queryterm} in
#'   browser. To open the browser with a previous search, (register or)
#'   queryterm can be the output of ctrGetQueryUrlFromBrowser() or can be one
#'   row from ctrQueryHistoryInDb().
#' @param ... Any additional parameter to use with browseURL, which is called by
#'   this function.
#'
#' @export
#'
#' @return Is always true, invisibly.
#'
#' @examples
#'
#' \dontrun{
#'
#' ctrOpenSearchPagesInBrowser(register = "EUCTR", queryterm = "cancer&age=under-18")
#' ctrOpenSearchPagesInBrowser(queryterm = ctrQueryHistoryInDb() [1,])
#' ctrOpenSearchPagesInBrowser(copyright = TRUE)
#'
#' }
#'
ctrOpenSearchPagesInBrowser <- function(register = c("EUCTR", "CTGOV"), copyright = FALSE, queryterm = "", ...) {
  #
  # check arguments
  if (!exists("register") || (register == '' & queryterm == '')) stop("No usable argument found.")
  #
  # deal with data frame as returned from ctrQueryHistoryInDb()
  if (is.data.frame(queryterm)) query <- queryterm
  if (is.data.frame(register))  query <- register
  #
  if (exists("query")) {
    tmp <- try ({
      if(nrow(query) > 1) warning("Parameter included data frame with more than one row, only using first row.", immediate. = TRUE)
      queryterm <- query [1, "query-term"]
      register  <- query [1, "query-register"]
      rm("query")
    }, silent = TRUE)
  }
  #
  # deal with values returned from ctrGetQueryUrlFromBrowser()
  if (is.list(queryterm)) query <- queryterm
  if (is.list(register))  query <- register
  #
  if (exists("query")) {
    tmp <- try ({
      queryterm <- query$queryterm
      register  <- query$register
      rm("query")
    }, silent = TRUE)
    #
  }
  #
  if(queryterm == '') {
    if (copyright == TRUE) {
      if ("CTGOV" %in% register) utils::browseURL("https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use", ...)
      if ("EUCTR" %in% register) utils::browseURL("http://www.ema.europa.eu/ema/index.jsp?curl=pages/regulation/general/general_content_000178.jsp&mid=", ...)
    } else {
      if ("CTGOV" %in% register) utils::browseURL("https://clinicaltrials.gov/ct2/search/advanced", ...)
      if ("EUCTR" %in% register) utils::browseURL("https://www.clinicaltrialsregister.eu/ctr-search/search", ...)
    }
  }
  #
  # try to deduce queryterm and register from a url that is provided as anonymous first parameter
  if(queryterm == '' && grepl ("^https.+clinicaltrials.+", register)) queryterm <- ctrdata::ctrGetQueryUrlFromBrowser(content = register)
  #
  # graciously deduce queryterm and register if a url is unexpectedly provided as queryterm
  if(is.character(queryterm) && grepl ("^https.+clinicaltrials.+", queryterm)) queryterm <- ctrdata::ctrGetQueryUrlFromBrowser(content = queryterm)
  #
  if (queryterm != "") {
    message("Opening in browser previous search: ", queryterm, ", in register: ", register)
    if ("CTGOV" %in% register) utils::browseURL(paste0("https://clinicaltrials.gov/ct2/results?", queryterm), ...)
    if ("EUCTR" %in% register) utils::browseURL(paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=", queryterm), ...)
  }
  #
  invisible(TRUE)
}
# end ctrOpenSearchPagesInBrowser


#' Import from clipboard the URL of a search in one of the registers
#'
#' @param content URL from browser address bar. Defaults to clipboard contents.
#' @return A string of query parameters that can be used to retrieve data from
#'   the register.
#'
#' @import clipr
#'
#' @export
#'
#' @return A list with a query term and the register name that can directly be
#'   used in \link{ctrLoadQueryIntoDb}
#'
#' @examples
#'
#' \dontrun{
#' ctrLoadQueryIntoDb (ctrGetQueryUrlFromBrowser())
#' }
#'
ctrGetQueryUrlFromBrowser <- function(content = clipr::read_clip()) {
  #
  if (length(content) != 1L) {
    stop(paste("Clipboard content is not a clinical trial register search URL. Returning NULL."))
    return(NULL)
  }
  #
  if (grepl("https://www.clinicaltrialsregister.eu/ctr-search/", content)) {
    #
    queryterm <- sub("https://www.clinicaltrialsregister.eu/ctr-search/search[?]query=(.*)", "\\1", content)
    message("Found search query from EUCTR.")
    return(list(queryterm = queryterm, register = "EUCTR"))
  }
  #
  if (grepl("https://clinicaltrials.gov/ct2/results", content)) {
    #
    queryterm <- sub("https://clinicaltrials.gov/ct2/results[?](.*)", "\\1", content)
    queryterm <- sub("(.*)&Search=Search", "\\1", queryterm)
    queryterm <- gsub("[a-z_0-9]+=&", "", queryterm)
    queryterm <- sub("&[a-z_0-9]+=$", "", queryterm)
    message("Found search query from CTGOV.")
    return(list(queryterm = queryterm, register = "CTGOV"))
  }
  #
  stop(paste("Content is not a clinical trial register search URL. Returning NULL."))
  return(NULL)
}
# end ctrGetQueryUrlFromBrowser


#' Show the history of queries that were loaded into a database
#'
#' @inheritParams ctrMongo
#'
#' @return A data frame with variables: query-timestamp, query-egister,
#'  query-records (note: this is the number of records loaded when last executing
#'  ctrLoadQueryIntoDb(), not the total record number) and query-term
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dbQueryHistory()
#' }
#'
dbQueryHistory <- function(mongo = "") {

  # get a working mongo connection
  mongo <- ctrMongo(mongo = mongo)

  # Example history:
  # {
  #   "_id" : "meta-info",
  #   "queries": [
  #   {
  #     "query-timestamp" : "2016-10-18-17-09-20",
  #     "query-register" : "EUCTR",
  #     "query-records" : "6",
  #     "query-term" : "2010-024264-18"
  #   },
  #   {
  #     "query-timestamp" : "2016-10-18-17-08-40",
  #     "query-register" : "CTGOV",
  #     "query-records" : "1",
  #     "query-term" : "term=2010-024264-18"
  #   }
  #  ]
  # }

  # Get record from mongo db using batch because find would
  # try to return a dataframe and this would ignore the array
  tmp <- mongo$iterate(query = '{"_id":{"$eq":"meta-info"}}', fields = '{"queries": 1, "_id": 0}')$batch()

  # Select only relevant element from record
  tmp <- tmp[[1]]$queries

  # Check if meeting expectations
  if(!is.list(tmp) || (length(tmp) < 1)) stop("No history found in expected format. Please check database connection.")

  # Change into data frame with appropriate column names
  tmp <- sapply(tmp, function(x) do.call(rbind, x))
  tmp <- t(tmp)
  tmp <- data.frame(tmp, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
  names(tmp) <- c("query-timestamp", "query-register", "query-records", "query-term")

  # Inform user
  message("Number of queries in history of \"", mongo$info()$stats$ns, "\": ", nrow(tmp))

  return(tmp)

}
# end ctrQueryHistoryInDb


#' Find names of keys (fields) in the database
#'
#' Given part of the name of a field of interest to the user, this function
#' returns the full field names as found in the database. It is not necessary to
#' add wild cards to the name of the field of interest.
#'
#' For fields in EUCTR (protocol-related information), see also the register's
#' documentation: \url{https://eudract.ema.europa.eu/protocol.html}.
#'
#' For fields in CTGOV (protocol-related information), see also the register's
#' definitions: \url{https://prsinfo.clinicaltrials.gov/definitions.html}
#'
#' Note that generating a list of keys with variety.js as used in this function
#' may not work with certain mongo databases, for example when the host or port
#' is different per database, such as found with a free mongolab plan.
#'
#' @param namepart A plain string (not a regular expression) to be searched for
#'   among all field names (keys) in the database.
#' @param allmatches If \code{TRUE}, returns all keys if more than one is found
#'   (default is \code{FALSE}).
#' @param forceupdate If \code{TRUE}, refreshes collection of keys (default is
#'   \code{FALSE}).
#' @param debug If \code{TRUE}, prints additional information (default is
#'   \code{FALSE}).
#'
#' @inheritParams ctrMongo
#'
#' @return Vector of first keys (fields) found (or of all keys, see above)
#'
#' @import rmongodb curl
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  dbFindVariable ("date")
#' }
#'
dbFindVariable <- function(namepart = "", allmatches = FALSE, forceupdate = FALSE,
                           mongo = "", debug = FALSE) {

  # sanity checks
  if (!is.atomic(namepart)) stop("Name part should be atomic.")
  if (length(namepart) > 1) stop("Name part should have only one element.")
  if (namepart == "" & !forceupdate) stop("Empty name part string.")

  # check program availability
  if (.Platform$OS.type == "windows") {
    installMongoFindBinaries()
    if (is.na(get("mongoBinaryLocation", envir = .privateEnv))) stop("Not running dbFindVariable because mongo binary was not found.")
  }

  # get a working mongo connection
  mongo <- ctrMongo(mongo = mongo)

  # check if database with variety results exists or should be forced to be updated
  if (forceupdate || length(rmongodb::mongo.get.database.collections(mongo, db = "varietyResults")) == 0L ||
      length(grepl(paste0(ns, "Keys"), rmongodb::mongo.get.database.collections(mongo, db = "varietyResults"))) == 0L) {
    #
    if (!grepl("127.0.0.1", attr(mongo, "host")))
      warning("variety.js may fail with certain remote servers (for example when the host or port ",
              "is different per database, such as with a free mongolab plan).", immediate. = TRUE)
    #
    # check if extension is available (system.file under MS Windows does not end with slash) ...
    varietylocalurl <- paste0(system.file("exec", package = "ctrdata"), "/variety.js")
    # if variety.js is not found, download it
    if (!file.exists(varietylocalurl)) {
      message("Downloading variety.js and installing into package exec folder ...")
      varietysourceurl <- "https://raw.githubusercontent.com/variety/variety/master/variety.js"
      curl::curl_download(varietysourceurl, varietylocalurl)
    }
    # compose actual command to call mongo with variety.js
    varietymongo <- paste0('mongo "', attr(mongo, "host"), '/', attr(mongo, "db"), '"',
                           ifelse(attr(mongo, "username") != "", paste0(" --username ", attr(mongo, "username")), ""),
                           ifelse(attr(mongo, "password") != "", paste0(" --password ", attr(mongo, "password")), ""),
                           " --eval \"var collection = '", ns, "', persistResults=true\" ", varietylocalurl)
    #
    if (.Platform$OS.type == "windows") {
      varietymongo <- paste0(get("mongoBinaryLocation", envir = .privateEnv), varietymongo)
    }
    #
    message("Calling mongo with variety.js and adding keys to database ...")
    if (debug) message(varietymongo)
    tmp <- system(varietymongo, intern = TRUE)
    #
  }

  if (namepart != "") {
    #
    # mongo get fieldnames
    # TODO avoid data.frame = TRUE
    tmp <- rmongodb::mongo.find.all(mongo, paste0("varietyResults", ".", ns, "Keys"), fields = list("key" = 1L), data.frame = TRUE)
    fieldnames <- tmp[,1]
    #
    # actually now find fieldnames
    fieldname <- fieldnames[grepl(tolower(namepart), tolower(fieldnames))]
    if (!allmatches) {
      if ((tmp <- length(fieldname)) > 1) message(paste0("Returning first of ", tmp, " keys found."))
      fieldname <- fieldname[1]
    }
    # return the first match / all matches
    return(fieldname)
    #
  }

}
# end dbFindVariable


#' This function checks for duplicate records of clinical trialss in the
#' database based on the clinical trial identifier, and it returns a list of ids
#' of unique trials.
#'
#' If records for a clinical trial are found from more than one register, the
#' record from EUCTR is returned. The function currently relies on CTGOV
#' recording other identifiers such as the EudraCT number in the field "Other
#' IDs".
#'
#' @inheritParams ctrLoadQueryIntoDb
#' @inheritParams dfFindUniqueEuctrRecord
#'
#' @param preferregister The abbreviation of the preferred register, in case
#' a trial is in more than one register (string, either "EUCTR" or "CTGOV").
#' If set to an empty string (""), keeps the keys for the same trial in both
#' registers in the returned vector.
#'
#' @param verbose If set to \code{TRUE}, prints out information about numbers
#' of records found at subsequent steps when searching for duplicates
#'
#' @return A vector with strings of keys (_id in the database) that are
#'   non-duplicate trials.
#'
#' @export
#'
#' @import rmongodb
#'
#' @examples
#'
#' \dontrun{
#' dbFindIdsUniqueTrials()
#' }
#'
dbFindIdsUniqueTrials <- function(mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"), ns = "ctrdata",
                                  prefermemberstate = "GB", include3rdcountrytrials = TRUE, preferregister = "EUCTR", verbose = TRUE) {

  # objective: create a list of mongo database record identifiers (_id)
  # that represent unique records of clinical trials, based on user's
  # preferences for selecting the preferred from any multiple records

  # 1. get euctr records
  listofEUCTRids <- try(
    suppressWarnings(dbGetVariablesIntoDf(fields = c("a2_eudract_number", "a52_us_nct_clinicaltrialsgov_registry_number", "a3_full_title_of_the_trial"), mongo = mongo, ns = ns)),
    silent = TRUE
  )
  if(class(listofEUCTRids) == "try-error") listofEUCTRids <- NULL
  if(!is.null(listofEUCTRids)) listofEUCTRids <- listofEUCTRids[grepl("[0-9]{4}-[0-9]{6}-[0-9]{2}-[3A-Z]{2,3}", listofEUCTRids[["_id"]]), ]

  # 2. find unique, preferred country version
  if(!is.null(listofEUCTRids)) listofEUCTRids <- ctrdata:::dfFindUniqueEuctrRecord(df = listofEUCTRids,
                                                                                   prefermemberstate = prefermemberstate,
                                                                                   include3rdcountrytrials = include3rdcountrytrials)

  # 3. get ctrgov records
  listofCTGOVids <- try(
    suppressWarnings(dbGetVariablesIntoDf(fields = c("otherids", "official_title"), mongo = mongo, ns = ns)),
    silent = TRUE
  )
  if(class(listofCTGOVids) == "try-error") listofCTGOVids <- NULL
  if(!is.null(listofCTGOVids)) listofCTGOVids <- listofCTGOVids[grepl("NCT[0-9]{8}", listofCTGOVids[["_id"]]), ]

  # 4. retain unique ctrgov records
  if(!is.null(listofCTGOVids)) {
    dupes <- listofCTGOVids[["_id"]] %in% listofCTGOVids[["otherids"]]
    if (sum(dupes) > 0) listofCTGOVids <- listofCTGOVids[!dupes, ]
  }

  # 5. find records (_id's) that are in both in euctr and ctgov
  # 6. select records from preferred register
  if(preferregister == "EUCTR") {
    #
    # a.2 - ctgov in euctr
    dupes.a.2 <- listofCTGOVids[["otherids"]] %in% listofEUCTRids[["_id"]]
    if(verbose) message("Searching duplicates: Found ", sum(dupes.a.2), " CTGOV otherids in EUCTR _id's")
    #
    # b.2 - ctgov in euctr
    dupes.b.2 <- listofCTGOVids[["_id"]] %in% listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]
    if(verbose) message("Searching duplicates: Found ", sum(dupes.b.2), " CTGOV _id's in EUCTR a52_us_nct_clinicaltrialsgov_registry_number")

    retids <- c(listofEUCTRids[["_id"]], listofCTGOVids[["_id"]] [!dupes.a.2 & !dupes.b.2])
    #
  }
  if(preferregister == "CTGOV") {
    #
    # a.1 - euctr in ctgov
    dupes.a.1 <- listofEUCTRids[["_id"]] %in% listofCTGOVids[["otherids"]]
    if(verbose) message("Searching duplicates: Found ", sum(dupes.a.1), " EUCTR _id's in CTGOV otherids")

    # b.1 - euctr in ctgov
    dupes.b.1 <- listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]] %in% listofCTGOVids[["_id"]]
    if(verbose) message("Searching duplicates: Found ", sum(dupes.b.1), " EUCTR a52_us_nct_clinicaltrialsgov_registry_number in CTOGV _id's")

    retids <- c(listofCTGOVids[["_id"]], listofEUCTRids[["_id"]] [!dupes.a.1 & !dupes.b.1])
    #
  }

  # # search for duplicates using study titles
  # # first experiments showed that titles of the same study
  # # may be very different between the registers, hence this
  # # approach does not seem sufficiently sensitive and specific
  # if(FALSE) {
  #   #
  #   if ("stringdist" %in% installed.packages()[,"Package"] &&
  #       "tm" %in% installed.packages()[,"Package"]) {
  #     #
  #     # library(tm)
  #     # myCorpus <- Corpus(VectorSource(df$text))
  #     # myCorpus <- tm_map(myCorpus, tolower)
  #     # myCorpus <- tm_map(myCorpus, removePunctuation)
  #     # myCorpus <- tm_map(myCorpus, removeNumbers)
  #
  #     # first argument goes into the rows of the result value
  #     tmp <- stringdist::stringdistmatrix(tolower(listofEUCTRids[["a3_full_title_of_the_trial"]]),
  #                                         tolower(listofCTGOVids[["official_title"]]))
  #     #
  #     # retrieve well matching titles
  #     tmp <- which(tmp < 45, arr.ind = TRUE)
  #     #
  #     # list well matching titles
  #     cbind(listofEUCTRids[["a3_full_title_of_the_trial"]][tmp[,1]],
  #           listofCTGOVids[["official_title"]][tmp[,2]])
  #     #
  #   }
  # }

  # prepare output
  #
  # avoid returning list() if none found
  if(length(retids) == 0) retids <- character()
  #
  # inform user
  message(paste0("Returning keys (_id's) of ", length(retids), " records in the database."))
  #
  return(retids)

}
# end dbFindIdsUniqueTrials


#' Create a data frame from records in the database that have specified fields
#'
#' With this convenience function, fields in the mongo database are retrieved
#' into an R dataframe. As mongo json fields within the record of a trial
#' can be hierarchical and structured, the function returns a concatenation
#' of values if there is more than one value or if the field is (in) an array,
#' such as follows: value 1 / value 2 / ... (see example)
#'
#' For more sophisticated data retrieval from the database, see vignette examples
#' and other packages to query mongodb such as mongolite.
#'
#' @param fields Vector of strings, with names of the sought fields.
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return A data frame with columns corresponding to the sought fields. Note
#'   that a column for the record _id will always be included. The maximum
#'   number of rows of the returned data frame is equal to or less than the
#'   number of records in the data base.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' dbGetVariablesIntoDf("b1_sponsor.b31_and_b32_status_of_the_sponsor")[1,]
#' #                   _id  b1_sponsor.b31_and_b32_status_of_the_sponsor
#' #  1  2004-000015-25-GB                   Non-commercial / Commercial
#'
#' dbGetVariablesIntoDf("keyword")[1:2,]
#' #            _id                                           keyword
#' #  1 NCT00129259  T1D / type 1 diabetes / type 1 diabetes mellitus
#'
#' }
#'
dbGetVariablesIntoDf <- function(fields = "", mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"),
                                 ns = "ctrdata", debug = FALSE) {
  #
  if (!is.vector(fields) | class(fields) != "character") stop("Input should be a vector of strings of field names.")
  if (any(fields == "", na.rm = TRUE)) stop(paste("'fields' contains empty elements; please provide a vector of strings of field names.",
                                                  "Function dbFindVariable() can be used to find field names."))
  #
  # total number of records in collection. for information of user at end of function.
  countall <- rmongodb::mongo.count(mongo, paste0(attr(mongo, "db"), ".", ns),
                                    query  = rmongodb::mongo.bson.from.JSON('{"_id":{"$ne":"meta-info"}}'))
  #
  # initialise output
  result <- NULL
  #
  for (item in fields) {
    #
    query <- paste0('{"_id": {"$ne": "meta-info"}, "', item, '": {"$gt": ""}}')
    if (debug) message("DEBUG: variable / field: ", item)
    #
    tmp <- try({
      dfi <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), '.', ns), data.frame = FALSE,
                                      query  = rmongodb::mongo.bson.from.JSON(query),
                                      fields = rmongodb::mongo.bson.from.JSON(paste0('{"_id": 1, "', item, '": 1}')))
      if (debug) message("DEBUG: variable / field ", item, " has length ", length(dfi))
      #
      # attempt custom function to condense into a data frame instead of using data.frame = TRUE
      dfi <- as.data.frame(cbind(sapply(dfi, function(x) as.vector(unlist(x[1]))),
                                 sapply(dfi, function(x) paste0(as.vector(unlist(x[2])), collapse = " / "))),
                           stringsAsFactors = FALSE)
      names(dfi) <- c("_id", item)
      #
    }, silent = FALSE)
    # }
    #
    if ((class(tmp) != "try-error") && (nrow(dfi) > 0)) {

      if (is.null(result)) {
        result <- dfi
      } else {
        result <- merge(result, dfi, by = '_id', all = TRUE)
      }

    } else {# try-error occured
      stop(paste0("For variable / field: ", item, " no data could be extracted, please check the contents of the database."))
    }
  } # end for item in fields

  # finalise output
  if (is.null(result)) stop('No records found which had values for the specified fields.')
  # some results were obtained
  diff <- countall - nrow(result)
  if (diff > 0) warning(paste0(diff, " of ", countall, " records dropped which did not have values for any of the specified fields."))
  #
  return(result)
}
# dbGetVariablesIntoDf


#' Merge related variables into a single variable, and optionally map values to
#' a new set of values.
#'
#' @param df A data frame in which there are two variables (columns) to be
#'   merged into one.
#' @param varnames A vector with names of the two variables to be merged.
#' @param levelslist A list with one slice each for a new value to be used for a
#'   vector of old values.
#'
#' @return A vector of strings
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' statusvalues <- list("ongoing" = c("Recruiting", "Active", "Ongoing",
#'                                    "Active, not recruiting", "Enrolling by invitation"),
#'                      "completed" = c("Completed", "Prematurely Ended", "Terminated"),
#'                      "other" = c("Withdrawn", "Suspended",
#'                                  "No longer available", "Not yet recruiting"))
#' dfMergeTwoVariablesRelevel(result, c("Recruitment", "x5_trial_status"), statusvalues)
#' }
#'
dfMergeTwoVariablesRelevel <- function(df = NULL, varnames = "", levelslist = NULL) {
  #
  if (class(df) != "data.frame")   stop("Need a data frame as input.")
  if (length(varnames)  != 2)      stop("Please provide exactly two variable names.")

  # find variables in data frame and merge
  tmp <- match(varnames, names(df))
  df <- df[, tmp]
  df[,1] <- ifelse(is.na(tt <- df[ ,1]), "", tt)
  df[,2] <- ifelse(is.na(tt <- df[ ,2]), "", tt)
  tmp <- paste0(df[,1], df[,2])

  if (!is.null(levelslist)) {

    # check
    if (class(levelslist) != "list") stop("Need lists for parameter levelslist.")

    # helper function to collapse factor levels into the first mentioned level
    refactor <- function(x, collapselevels, levelgroupname){
      levels(x) [match(collapselevels, levels(x))] <- levelgroupname
      return(x)
    }

    # convert result to factor as this is needed for helper function
    tmp <- as.factor(tmp)

    # apply helperfunction to elements of the list
    for (i in 1:length(levelslist)) {
      tmp <- refactor(tmp, unlist(levelslist[i]), attr(levelslist[i], "names"))
    }

    # convert factor back into string vector
    tmp <- as.character(tmp)

  }

  if(length(tt <- unique(tmp)) > 10)
    message("Unique values returned (limited to ten): ", paste(tt[1:10], collapse = ", "))
  else
    message("Unique values returned: ", paste(tt, collapse = ", "))

  return(tmp)
}
# end dfMergeTwoVariablesRelevel


#' Select a single trial record when there are records for different EU Member
#' States for this trial.
#'
#' The EUCTR provides one record per trial per EU Member State in which the
#' trial is conducted. For all trials conducted in more than one Member State,
#' this function returns only one record per trial. A preferred Member State can
#' be specified by the user, and a record of the trial in the preferred Member
#' State will be returned if available. If not, an english record ("GB") or
#' lacking this, any other available record will be returned.
#'
#' Note: To depuplicate trials from different registers (EUCTR and CTGOV),
#' please first use function \code{\link{dbFindIdsUniqueTrials}}.
#'
#' @param df A data frame created from the database that includes the columns
#'   "_id" and "a2_eudract_number", for example created with function
#'   dbGetVariablesIntoDf(c("_id", "a2_eudract_number")).
#' @param prefermemberstate Code of single EU Member State for which records should
#'   returned if available. (If not available, a record for GB or lacking this
#'   any other record for the trial will be returned.) For a list of codes of EU
#'   Member States, please see vector \code{countriesEUCTR}.
#' @param include3rdcountrytrials A logical value if trials should be retained
#'   that are conducted in third countries, that is outside the European Union.
#'   These can be recognised by EUCTR identifiers ending in -3RD, such as 2010-022945-52-3RD.
#'
#' @return A data frame as subset of \code{df} corresponding to the sought
#'   records.
#'
#' @keywords internal
#
dfFindUniqueEuctrRecord <- function(df = NULL, prefermemberstate = "GB", include3rdcountrytrials = TRUE) {
  #
  if (class(df) != "data.frame") stop("Parameter df is not a data frame.")
  if (is.null(df [['_id']]) || is.null(df$a2_eudract_number)) stop('Data frame does not include "_id" and "a2_eudract_number" columns.')
  if (nrow(df) == 0) stop("Data frame does not contain records (0 rows).")
  if (!(prefermemberstate %in% countriesEUCTR)) stop("Value specified for prefermemberstate does not match one of the recognised codes: ", paste (sort (countriesEUCTR), collapse = ", "))

  # count number of records by eudract number
  tbl <- table(df [['_id']], df$a2_eudract_number)
  tbl <- as.matrix(tbl)
  # nms has names of all records
  nms <- dimnames(tbl)[[1]]

  # nrs has eudract numbers for which is there more than 1 record
  nrs <- colSums(tbl)
  nrs <- nrs[nrs > 1]
  nrs <- names(nrs)

  # nst is a list of nrs trials of a logical vector along nms
  # that indicates if the indexed record belongs to the trial
  nst <- lapply(nrs, function(x) substr(nms, 1, 14) %in% x)

  # helper function to find the Member State version
  removeMSversions <- function(indexofrecords){
    # given a vector of records (nnnn-nnnnnnn-nn-MS) of a single trial, this
    # returns all those _ids of records that do not correspond to the preferred
    # Member State record, based on the user's choices and defaults.
    # Function uses prefermemberstate, nms from the caller environment
    #
    recordnames <- nms[indexofrecords]
    #
    # fnd should be only a single string, may need to be checked
    if (sum(fnd <- grepl(prefermemberstate, recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    #
    if (sum(fnd <- grepl("GB", recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    #
    return(recordnames[-1])
  }

  # finds per trial the desired record; uses prefermemberstate and nms
  result <- lapply(nst, function(x) removeMSversions(x))
  result <- unlist(result)

  # eleminate the unwanted EUCTR records
  df <- df [!(df [['_id']] %in% result), ]
  # also eliminate the meta-info record
  df <- df [!(df [['_id']] == "meta-info"), ]

  # as a last step, handle 3rd country trials e.g. 2010-022945-52-3RD
  if (!include3rdcountrytrials) df <- df [!grepl("-3RD", df[["_id"]]), ]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result))) message(tmp, ' EUCTR records dropped that were not the preferred of multiple records for the trial.')

  return(df)
  #
}
# end dfFindUniqueEuctrRecord


#' Convenience function to install a cygwin environment under MS Windows,
#' including perl and php.
#'
#' Alternatively and in case of difficulties, download and run the cygwin
#' setup yourself as follows: \code{cygwinsetup.exe --no-admin --quiet-mode
#' --verbose --upgrade-also --root c:/cygwin --site
#' http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages
#' perl,php-jsonc,php-simplexml}
#'
#' @export
#'
#' @param force Set to \code{TRUE} to force updating and overwriting an existing
#'   installation in \code{c:\\cygwin}
#' @param proxy Specify any proxy to be used for downloading via http, e.g.
#'   "host_or_ip:port". \code{installCygwinWindowsDoInstall()} may detect and use
#'   the proxy configuration uset in MS Windows to use an automatic proxy
#'   configuration script. Authenticated proxies are not supported at this time.
#'
installCygwinWindowsDoInstall <- function(force = FALSE, proxy = ""){
  #
  if (.Platform$OS.type != "windows")    stop("This function is only for MS Windows operating systems.")
  if (!force & dir.exists("c:\\cygwin")) stop("cygwin is already installed. To overwrite, call this function with force = TRUE.")
  #
  # create directory within R sessions temporary directory
  tmpfile <- paste0(tempdir(), '/cygwin_inst')
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")
  #
  # download.file uses the proxy configured in the system
  if (grepl("64-bit", utils::sessionInfo()$platform)) utils::download.file(url = "http://cygwin.org/setup-x86_64.exe", destfile = dstfile, quiet = TRUE, mode = "wb")
  if (grepl("32-bit", utils::sessionInfo()$platform)) utils::download.file(url = "http://cygwin.org/setup-x86.exe",    destfile = dstfile, quiet = TRUE, mode = "wb")
  #
  # check
  if (!file.exists(dstfile))         stop("Download failed. Please install manually.")
  if (file.size(dstfile) < 5*10 ^ 5) stop("Download seems to have failed (file too small). Please install manually.")
  #
  if (proxy != "") {
    # manual setting overrides all
    proxy <- paste0('--proxy ', proxy)
    message("Setting cygwin proxy install argument to: ", proxy, ", based on provided parameter.")
  } else {
    # detect proxy to be used, automatically or manually configured?
    # find and use proxy settings for actually running the cygwin setup
    # - Windows 7 see https://msdn.microsoft.com/en-us/library/cc980059.aspx
    # - Windows 10 see
    tmp <- utils::readRegistry('Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings', hive = "HCU")
    #
    if (!is.null(tmp)) {
      #
      if (length (tmp$AutoConfigURL) > 0) {
        # retrieve settings
        proxypacfile <- paste0(tmpfile, '/pacfile.txt')
        utils::download.file(tmp$AutoConfigURL, proxypacfile)
        # for testing: proxypacfile <- "private/proxypacfile"
        # find out and select last mentioned proxy line
        proxypac <- readLines(proxypacfile)
        proxypac <- proxypac[grepl('PROXY', proxypac)]
        proxypac <- proxypac[length(proxypac)]
        proxy <- sub('.*PROXY ([0-9]+.[0-9]+.[0-9]+.[0-9]+:[0-9]+).*', '\\1', proxypac)
        if (proxy == '') stop('A proxy could not be identified from the automatic configuration script used by the system.',
                              ' Please set manually: installCygwinWindowsDoInstall (proxy = "host_or_ip:port"')
        proxy <- paste0('--proxy ', proxy)
        message("Automatically setting cygwin proxy install argument to: ", proxy, ", based on AutoConfigProxy in registry.")
        #
      } else {
        if (!is.null(tmp$ProxyServer)) {
          proxy <- paste0('--proxy ', tmp$ProxyServer)
          message("Automatically setting cygwin proxy install argument to: ", proxy, ", based on ProxyServer in registry.")
          #
        } else {
          message("Proxy not set, could not use ProxyServer or AutoConfigProxy in registry.")
          #
        }
      }
    }
  }
  #
  # compose installation command
  installcmd <- "--no-admin --quiet-mode --verbose --upgrade-also --root c:/cygwin --site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages perl,php-jsonc,php-simplexml"
  #
  # execute cygwin setup command
  system(paste0(dstfile, " ", installcmd, " --local-package-dir ", tmpfile, ' ', proxy))
  #
  # test cygwin installation
  ctrdata:::installCygwinWindowsTest()
  #
}
# end installCygwinWindowsDoInstall


#' Convenience function to test for working cygwin installation
#'
#' @return Information if cygwin can be used, \code{TRUE} or \code{FALSE}
#'
#' @keywords internal
#
installCygwinWindowsTest <- function() {
  #
  if (.Platform$OS.type != "windows") stop("This function is only for MS Windows operating systems.")
  #
  tmpcygwin <- system("cmd.exe /c c:\\cygwin\\bin\\env", intern = TRUE)
  #
  if (length(tmpcygwin) > 5) {
    message("cygwin base install seems to be working correctly.")
    invisible(TRUE)
  } else {
    warning("cygwin does not seem to be installed correctly.")
    invisible(FALSE)
  }
}
# installCygwinWindowsTest


#' Function to detect the location of mongo database binaries (mongo,
#' mongoimport) and to save the location into a private environment.
#' Call this function with a correct parameter if the automatic
#' detection fails.
#'
#' @param mongoDirWin Only used under MS Windows: folder that contains mongo
#'   binaries, defaults to "c:\\mongo\\bin\\" as used on
#'   \url{http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows#interactive-installation}
#'
#' @return Either an empty string if \code{mongoimport} was found on the path
#'   or, under MS Windows, a string representing the path to the folder of the
#'   mongo binaries
#'
#' @keywords internal
#
installMongoFindBinaries <- function(mongoDirWin = "c:\\mongo\\bin\\") {
  #
  # debug: mongoBinaryLocation <- "/usr/bin/"
  tmp <- ifelse(.Platform$OS.type != "windows", "mongoimport", "mongoimport.exe")
  #
  if (exists("mongoBinaryLocation", envir = .privateEnv) && !is.na(get("mongoBinaryLocation", envir = .privateEnv))
      && file.exists(paste0(get("mongoBinaryLocation", envir = .privateEnv), tmp))) {
    #
    message("mongoimport / mongo is in ", get("mongoBinaryLocation", envir = .privateEnv))
    invisible(get("mongoBinaryLocation", envir = .privateEnv))
    #
  } else {
    #
    # check folder specified in parameter
    mongoDirWin <- gsub("[\\]*$", "\\\\", mongoDirWin)
    #
    if ((.Platform$OS.type == "windows") && (file.exists(paste0(mongoDirWin, 'mongoimport.exe')))) {
      #
      assign("mongoBinaryLocation", mongoDirWin, envir = .privateEnv)
      message("mongoimport / mongo is in ", mongoDirWin)
      invisible(get("mongoBinaryLocation", envir = .privateEnv))
      #
    } else {
      #
      # not found: reset any information and start searching
      assign("mongoBinaryLocation", NA, envir = .privateEnv)
      #
      # first test for binary in the path
      tmp <- try(if (.Platform$OS.type != "windows") {
        system('mongoimport --version', intern = TRUE)
      } else {
        system('mongoimport.exe --version', intern = TRUE)
      }, silent = TRUE)
      #
      if (class(tmp) != "try-error") {
        #
        # found it in the path, save empty location string in package environment
        message("mongoimport / mongo found in the path.")
        assign("mongoBinaryLocation", "", envir = .privateEnv)
        invisible("")
        #
      } else {
        #
        warning("mongoimport / mongo was not found in the path.", immediate. = TRUE)
        #
        if (.Platform$OS.type != "windows") stop("Cannot continue. Search function is only for MS Windows operating systems.")
        #
        # second search for folder into which mongo was installed
        location <- utils::readRegistry('SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Installer\\Folders', hive = "HLM")
        location <- names(location)
        location <- location[grepl("Mongo", location)]
        location <- location[grepl("bin", location)]
        #
        tmp <- file.exists(paste0(location, 'mongoimport.exe'))
        #
        if (!tmp) stop("Cannot continue. mongoimport not found recorded in the registry, ", location, ".")
        #
        # found it, save in package environment
        location <- shQuote(location)
        assign("mongoBinaryLocation", location, envir = .privateEnv)
        message("mongoimport / mongo found in ", location)
        invisible(location)
        #
      }
    }
  }
}
# end installMongoFindBinaries


#' Check the version of the build of the mongo server to be used
#'
#' In addition to the returned value, the function will generate a warning
#' message if applicable.
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return A logical value indicating if the mongodb version is acceptable for
#'   use with this package.
#'
#' @keywords internal
#
installMongoCheckVersion <- function(mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users")) {
  #
  result <- rmongodb::mongo.command(mongo, attr(mongo, "db"), list("buildInfo" = 1L))
  result <- rmongodb::mongo.bson.to.Robject(result)
  #
  # for testing
  #result$version <- "2.6.3"
  #
  if (grepl("^3", result$version)) {
    #
    return(TRUE)
    #
  } else {
    #
    warning("mongodb not version 3. Earlier versions have limitations that may break function ctrLoadQueryIntoDb() in package ctrdata.\n Please upgrade, see http://docs.mongodb.org/manual/installation/. \n Trying to continue. Support for versions other than 3 may be discontinued.", immediate. = TRUE)
    return(FALSE)
  }
  #
}
# end installMongoCheckVersion


#' Check availability of binaries installed in operating system
#'
#' @param commandtest Command to be used for testing the availability of the binary, e.g. "php -v". Note
#' internal quotes need to be escaped, e.g. \code{installFindBinary('php -r \"simplexml_load_string(\'\');\"')}.
#' See R/onload.R for tested binaries.
#'
#' @param debug Set to \code{TRUE} to see printed return value of \code{commandtest}
#'
#' @return A logical if executing commandtest returned an error or not
#'
#' @keywords internal
#
installFindBinary <- function(commandtest = NULL, debug = FALSE) {
  #
  if (is.null(commandtest)) stop ("Empty argument: commandtest")
  #
  if (.Platform$OS.type == "windows") commandtest <- paste0("cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c ", shQuote(commandtest))
  #
  if(debug) print(commandtest)
  #
  commandresult <- try(
    system(commandtest, intern = TRUE),
    silent = TRUE
  )
  #
  commandreturn <- ifelse (class(commandresult) == "try-error" ||
                             grepl("error", tolower(paste(commandresult, collapse = " "))), FALSE, TRUE)
  #
  if(!commandreturn) warning(commandtest, " not found.")
  #
  if(debug) print(commandresult)
  #
  return(commandreturn)
  #
}
# end installFindBinary

