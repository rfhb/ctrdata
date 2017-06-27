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
                    "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB",
                    "3RD")



#' This is a function to set up connections to a Mongo DB server, one for
#' the actual trial records and a second for the keys as discovered by
#' variety in the trial records.
#'
#' @param collection Name of collection (default is "ctrdata")
#'
#' @param db Name of database (default is "users")
#'
#' @param url Address of the mongodb server in mongo connection string URI format
#'  \url{http://docs.mongodb.org/manual/reference/connection-string/} (default is
#'  mongodb://localhost)
#'
#' @param username In case access requires credentials.
#' @param password In case access requires credentials.
#' @param verbose Print information.
#'
#' @return A mongo data base object, currently using mongolite
#'
#' @keywords internal
#'
ctrMongo <- function(collection = "ctrdata", db = "users", url = "mongodb://localhost",
                     username = "", password = "", verbose = FALSE) {

  # not used so far: options = ssl_options()

  # url: mongodb://[username:password@]host1[:port1]
  host     <- sub("mongodb://(.+)", "\\1", url)
  mongourl <- paste0("mongodb://",
                     ifelse(username != "", username, ""),
                     ifelse(password != "", paste0(":", password), ""),
                     ifelse(username != "", "@", ""),
                     host)

  # for variety, create / access related collection
  collectionKeys <- paste0(collection, "Keys")

  valueCtrDb     <- mongolite::mongo(collection = collection,     db = db, url = mongourl, verbose = verbose)
  valueCtrKeysDb <- mongolite::mongo(collection = collectionKeys, db = db, url = mongourl, verbose = verbose)

  # inform user
  if (verbose) message("Using Mongo DB (collections \"", collection,
                       "\" and \"", collectionKeys,
                       "\" in database \"", db,
                       "\" on \"", host, "\").")

  return(list("ctr" = valueCtrDb, "keys" = valueCtrKeysDb))
}
# end ctrMongo


#' Open advanced search pages of register(s) or execute search in default web
#' browser.
#'
#' @param input Show results of search for \code{queryterm} in
#'   browser. To open the browser with a previous search, (register or)
#'   queryterm can be the output of \link{ctrGetQueryUrlFromBrowser} or can be one
#'   row from \link{dbQueryHistory}.
#' @param register Register(s) to open. Either "EUCTR" or "CTGOV" or a vector of
#'   both. Default is to open both registers' advanced search pages. To open the
#'   browser with a previous search, the output of ctrGetQueryUrlFromBrowser()
#'   or one row from dbQueryHistory() can be used.
#' @param copyright (Optional) If set to \code{TRUE}, opens copyright pages of
#'   register(s).
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
#' ctrOpenSearchPagesInBrowser("https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer")
#'
#' ctrOpenSearchPagesInBrowser(ctrGetQueryUrlFromBrowser())
#'
#' ctrOpenSearchPagesInBrowser(dbQueryHistory())
#'
#' }
#'
ctrOpenSearchPagesInBrowser <- function(input = "", register = "", copyright = FALSE, ...) {
  #
  # check combination of arguments to select action
  #
  if (class(input) == "character" && is.atomic(input) && input == "") {
    #
    # open empty search pages
    if ("EUCTR" == register) utils::browseURL("https://www.clinicaltrialsregister.eu/ctr-search/search", ...)
    if ("CTGOV" == register) utils::browseURL("https://clinicaltrials.gov/ct2/search/advanced", ...)
    #
    # if requested also show copyright pages
    if (copyright) {
      if ("EUCTR" == register) utils::browseURL("https://www.clinicaltrialsregister.eu/disclaimer.html", ...)
      if ("CTGOV" == register) utils::browseURL("https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use", ...)
    }
  } else {
    #
    # check input argument and determine action
    #
    # - is a url
    if (class(input) == "character" &&
        is.atomic(input) &&
        length(input) == 1 &&
        grepl ("^https.+clinicaltrials.+", input)) {
      #
      input <- ctrGetQueryUrlFromBrowser(input)
      #
    }
    #
    # - data frame as returned from ctrQueryHistoryInDb() and ctrGetQueryUrlFromBrowser()
    if (is.data.frame(input) && all(substr(names(input), 1, 6) == "query-")) {
      #
      nr <- nrow(input)
      #
      if (nr > 1) warning("Using last row of input.", immediate. = TRUE)
      #
      register  <- input [nr, "query-register"]
      queryterm <- input [nr, "query-term"]
      #
    }
    #
    if (queryterm != "" && register != "") {
      #
      message("Opening in browser previous search: ", queryterm, ", in register: ", register)
      #
      utils::browseURL(paste0(switch(as.character(register),
                                     "CTGOV" = "https://clinicaltrials.gov/ct2/results?",
                                     "EUCTR" = "https://www.clinicaltrialsregister.eu/ctr-search/search?query="),
                              queryterm), ...)
      #
    }
    #
    invisible(TRUE)
  }
}
# end ctrOpenSearchPagesInBrowser


#' Import from clipboard the URL of a search in one of the registers
#'
#' @param content URL from browser address bar. Defaults to clipboard contents.
#' @return A string of query parameters that can be used to retrieve data from
#'   the register.
#'
#' @export
#'
#' @return A data frame with a query term and the register name that can directly be
#'   used in \link{ctrLoadQueryIntoDb} and in \link{ctrOpenSearchPagesInBrowser}
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
  # EUCTR
  if (grepl("https://www.clinicaltrialsregister.eu/ctr-search/", content)) {
    #
    queryterm <- sub("https://www.clinicaltrialsregister.eu/ctr-search/search[?]query=(.*)", "\\1", content)
    message("Found search query from EUCTR.")
    #
    df <- data.frame(cbind(queryterm, "EUCTR"), stringsAsFactors = FALSE)
    names(df) <- c("query-term", "query-register")
    #
    return(df)
  }
  #
  # CTGOV, e.g.
  # https://clinicaltrials.gov/ct2/results?term=2010-024264-18&Search=Search
  if (grepl("https://clinicaltrials.gov/ct2/results", content)) {
    #
    queryterm <- sub("https://clinicaltrials.gov/ct2/results[?](.*)", "\\1", content)
    queryterm <- sub("(.*)&Search[a-zA-Z]*=(Search|Find)[a-zA-Z+]*",  "\\1", queryterm)
    queryterm <- gsub("[a-z_0-9]+=&", "", queryterm)
    queryterm <- sub("&[a-z_0-9]+=$", "", queryterm)
    message("Found search query from CTGOV.")
    #
    df <- data.frame(cbind(queryterm, "CTGOV"), stringsAsFactors = FALSE)
    names(df) <- c("query-term", "query-register")
    #
    return(df)
  }
  #
  warning("Content is not a clinical trial register search URL. Returning NULL.", immediate. = TRUE)
  return(NULL)
}
# end ctrGetQueryUrlFromBrowser


#' Show the history of queries that were loaded into a database
#'
#' @inheritParams ctrMongo
#'
#' @return A data frame with variables: query-timestamp, query-egister,
#'  query-records (note: this is the number of records loaded when last executing
#'  ctrLoadQueryIntoDb(), not the total record number) and query-term.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dbQueryHistory()
#' }
#'
dbQueryHistory <- function(collection = "ctrdata", db = "users", url = "mongodb://localhost",
                           username = "", password = "", verbose = FALSE) {

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = verbose)[["ctr"]]

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
  if (!is.list(tmp) || (length(tmp) < 1)) {
    #
    warning("No history found in expected format.", immediate. = TRUE)
    tmp <- data.frame(NULL)
    #
  } else {
    # Change into data frame with appropriate column names
    tmp <- sapply(tmp, function(x) do.call(rbind, x))
    tmp <- t(tmp)
    tmp <- data.frame(tmp, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
    names(tmp) <- c("query-timestamp", "query-register", "query-records", "query-term")
    # Inform user
    message("Number of queries in history of \"", mongo$info()$stats$ns, "\": ", nrow(tmp))
  }

  # total number of records in collection to inform user
  countall <- mongo$count(query = '{"_id":{"$ne":"meta-info"}}')
  message("Total of ", countall, " records in collection.")

  # close database connection
  rm(mongo); gc()

  # return
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
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  dbFindVariable ("date")
#' }
#'
dbFindVariable <- function(namepart = "", allmatches = FALSE, forceupdate = FALSE, debug = FALSE,
                           collection = "ctrdata", db = "users", url = "mongodb://localhost",
                           username = "", password = "", verbose = FALSE) {

  # sanity checks
  if (!is.atomic(namepart)) stop("Name part should be atomic.")
  if (length(namepart) > 1) stop("Name part should have only one element.")
  if (namepart == "" & !forceupdate) stop("Empty name part string.")

  # get a working mongo connection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = verbose)

  # check if database with variety results exists or should be forced to be updated
  if (forceupdate || mongo[["keys"]]$count() == 0L) {
    #
    # check program availability
    installMongoFindBinaries()
    #
    # if (!grepl("127.0.0.1", attr(mongo, "host")))
    #   warning("variety.js may fail with certain remote servers (for example when the host or port ",
    #           "is different per database, such as with a free mongolab plan).", immediate. = TRUE)
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
    # mongo collection_to_analyse --quiet --eval "var collection = 'users',
    #  persistResults=true, resultsDatabase='db.example.com/variety' variety.js
    varietymongo <- paste0(' "', sub("mongodb://(.+)", "\\1", url), "/", db, '"',
                           ifelse(username != "", paste0(' --username ="', username, '"'), ""),
                           ifelse(password != "", paste0(' --password ="', password, '"'), ""),
                           " --eval \"var collection='", collection, "', persistResults=true, ",
                           "resultsDatabase='", paste0(sub("mongodb://(.+)", "\\1", url), "/", db, "'\" "),
                           varietylocalurl)
    #
    if (.Platform$OS.type == "windows") {
      #
      varietymongo <- paste0(shQuote(installMongoFindBinaries()[1]), varietymongo)
      varietymongo <- gsub(" --([up])", " /\1", varietymongo)
      varietymongo <- gsub(" =", ":", varietymongo)
      #
    } else {
      #
      varietymongo <- paste0(shQuote(installMongoFindBinaries()[1]), varietymongo)
      #
    }
    #
    message("Calling mongo with variety.js and adding keys to database ...")
    if (debug) message(varietymongo)
    tmp <- system(varietymongo, intern = TRUE)
    if (debug) message(tmp)
    #
  }

  # now do the actual search and find for key name parts
  if (namepart != "") {
    #
    # mongo get fieldnames into vector (no other solution found)
    fieldnames <- mongo[["keys"]]$find(fields = '{"key": 1}')
    fieldnames <- fieldnames[seq_len(nrow(fieldnames)), ]
    fieldnames <- as.vector(fieldnames[["key"]])
    #
    # actually now find fieldnames
    fieldname <- fieldnames[grepl(tolower(namepart), tolower(fieldnames))]
    if (!allmatches) {
      if ( (tmp <- length(fieldname)) > 1) message(paste0("Returning first of ", tmp, " keys found."))
      fieldname <- fieldname[1]
    }
    #
    # add metadata
    fieldname <- addMetaData(fieldname,
                             collection = collection, db = db, url = url,
                             username = username, password = password)
    #
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
#' @param preferregister The abbreviation of the preferred register, in case
#' a trial is in more than one register (string, either "EUCTR" or "CTGOV").
#' If set to an empty string (""), keeps the keys for the same trial in both
#' registers in the returned vector.
#'
#' @inheritParams dfFindUniqueEuctrRecord
#'
#' @param verbose If set to \code{TRUE}, prints out information about numbers
#' of records found at subsequent steps when searching for duplicates
#'
#' @inheritParams ctrMongo
#'
#' @return A vector with strings of keys ("_id" in the database) that
#'   represent non-duplicate trials.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dbFindIdsUniqueTrials()
#' }
#'
dbFindIdsUniqueTrials <- function(preferregister = "EUCTR", prefermemberstate = "GB", include3rdcountrytrials = TRUE,
                                  collection = "ctrdata", db = "users", url = "mongodb://localhost",
                                  username = "", password = "", verbose = TRUE) {

  # parameter checks
  if (!grepl(preferregister, "CTGOVEUCTR")) stop("Register not known: ", preferregister)

  # objective: create a list of mongo database record identifiers (_id)
  # that represent unique records of clinical trials, based on user's
  # preferences for selecting the preferred from any multiple records

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = verbose)[["ctr"]]
  #
  # total number of records in collection to inform user
  countall <- mongo$count(query = '{"_id":{"$ne":"meta-info"}}')
  if (verbose)message("Total of ", countall, " records in collection.")

  # 1. get euctr records
  listofEUCTRids <- try(suppressMessages(suppressWarnings(
    dbGetVariablesIntoDf(fields = c("a2_eudract_number",
                                    "a41_sponsors_protocol_code_number",
                                    "a51_isrctn_international_standard_randomised_controlled_trial_number",
                                    "a52_us_nct_clinicaltrialsgov_registry_number"),
                         debug = FALSE,
                         collection = collection, db = db, url = url,
                         username = username, password = password, verbose = FALSE,
                         stopifnodata = FALSE)
    )),
    silent = TRUE
  )
  if (class(listofEUCTRids) == "try-error") listofEUCTRids <- NULL
  if ( is.null(listofEUCTRids)) message("No EUCTR records found.")
  if (!is.null(listofEUCTRids)) listofEUCTRids <-
    listofEUCTRids[grepl("[0-9]{4}-[0-9]{6}-[0-9]{2}-[3A-Z]{2,3}", listofEUCTRids[["_id"]]), ]

  # 2. find unique, preferred country version of euctr
  if (!is.null(listofEUCTRids)) listofEUCTRids <-
    dfFindUniqueEuctrRecord(df = listofEUCTRids,
                            prefermemberstate = prefermemberstate,
                            include3rdcountrytrials = include3rdcountrytrials)

  # 3. get ctrgov records
  listofCTGOVids <- mongo$iterate(
    query = '{"_id": {"$regex": "NCT[0-9]{8}"}}',
    fields = '{"id_info.org_study_id": 1, "id_info.secondary_id": 1, "id_info.nct_alias": 1}'
    )$batch(size = mongo$count())

  if (is.null(listofCTGOVids)) message("No CTGOV records found.")
  # close database connection
  rm(mongo); gc()

  # 4. retain unique ctrgov records
  if (!is.null(listofCTGOVids)) {
    #
    # search for dupes for each entry of _id, eliminate enty's own _id (nct_id) by grepl
    dupes <- sapply(listofCTGOVids, function(x) x[["_id"]] %in% unlist(x[["id_info"]]))
    if (sum(dupes) > 0) listofCTGOVids <- listofCTGOVids[!dupes, ]
    if (verbose) message("Searching duplicates: Found ", sum(dupes),
                        " CTGOV _id in CTGOV otherids (secondary_id, nct_alias, org_study_id)")
  }

  # 5. find records (_id's) that are in both in euctr and ctgov
  if (!is.null(listofEUCTRids) & !is.null(listofCTGOVids)) {
    #
    # 6. select records from preferred register
    if (preferregister == "EUCTR") {
      #
      # b2 - ctgov in euctr (_id corresponds to index 1)
      dupes_b2 <- sapply(listofCTGOVids, "[[", 1) %in% listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_b2),
                           " CTGOV _id in EUCTR a52_us_nct_clinicaltrialsgov_registry_number")
      #
      # a2 - ctgov in euctr a2_...
      dupes_a2 <- sapply(lapply(listofCTGOVids, function(x) sub(".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*",
                                                                "\\1", unlist(x[["id_info"]]))),
                         function(x) any(x %in% listofEUCTRids[["a2_eudract_number"]]))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_a2),
                           " CTGOV otherids (secondary_id, nct_alias, org_study_id) in EUCTR a2_eudract_number")
      #
      # c.2 - ctgov in euctr a52_... (id_info corresponds to index 2)
      dupes_c2 <- sapply(lapply(listofCTGOVids, "[[", 2),
                         function(x) any(unlist(x) %in%
                         listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_c2),
                           " CTGOV otherids (secondary_id, nct_alias, org_study_id) in",
                           " EUCTR a52_us_nct_clinicaltrialsgov_registry_number")
      #
      # d.2 - ctgov in euctr a51_... (id_info corresponds to index 2)
      dupes_d2 <- sapply(lapply(listofCTGOVids, "[[", 2),
                         function(x) any(unlist(x) %in%
                         listofEUCTRids[["a51_isrctn_international_standard_randomised_controlled_trial_number"]]))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_d2),
                           " CTGOV otherids (secondary_id, nct_alias, org_study_id) in ",
                           " EUCTR a51_isrctn_international_standard_randomised_controlled_trial_number")
      #
      retids <- c(listofEUCTRids[["_id"]], sapply(listofCTGOVids, "[[", 1) [
        !dupes_a2 & !dupes_b2 & !dupes_c2 & !dupes_d2])
      #
    }
    #
    if (preferregister == "CTGOV") {
      #
      # a.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes_a1 <- listofEUCTRids[["a2_eudract_number"]] %in% sub(".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*",
                                                                "\\1", unlist(sapply(listofCTGOVids, "[[", 2)))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_a1),
                           " EUCTR _id in CTGOV otherids (secondary_id, nct_alias, org_study_id)")
      #
      # b.1 - euctr in ctgov (_id corresponds to index 1)
      dupes_b1 <- listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]] %in%
                  sapply(listofCTGOVids, "[[", 1)
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_b1),
                           " EUCTR a52_us_nct_clinicaltrialsgov_registry_number in CTOGV _id")
      #
      # c.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes_c1 <- listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]] %in%
                  unlist(sapply(listofCTGOVids, "[[", 2))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_c1),
                           " EUCTR a52_us_nct_clinicaltrialsgov_registry_number in",
                           " CTOGV otherids (secondary_id, nct_alias, org_study_id)")
      #
      # d.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes_d1 <- listofEUCTRids[["a51_isrctn_international_standard_randomised_controlled_trial_number"]] %in%
                  unlist(sapply(listofCTGOVids, "[[", 2))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes_d1),
                           " EUCTR a51_isrctn_international_standard_randomised_controlled_trial_number ",
                           " in CTOGV otherids (secondary_id, nct_alias, org_study_id)")
      #
      retids <- c(sapply(listofCTGOVids, "[[", 1), listofEUCTRids[["_id"]] [
        !dupes_a1 & !dupes_b1 & !dupes_c1 & !dupes_d1])
      #
    }
  } else {
    #
    retids <- c(listofEUCTRids[["_id"]], unlist(sapply(listofCTGOVids, "[[", 1)))
    #
  }
  #
  # prepare output
  #
  # avoid returning list() if none found
  if (length(retids) == 0) retids <- character()
  #

  # add metadata
  retids <- addMetaData(retids,
                        collection = collection, db = db, url = url,
                        username = username, password = password)

  # inform user
  message(paste0("Returning keys (_id) of ", length(retids),
                 " records out of total of ", countall,
                 " records in collection \"", collection, "\"."))
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
#'
#' @param stopifnodata Stops with an error (\code{TRUE}, default) or with a warning
#'    (\code{TRUE}) if sought variable is not available in any of the records
#'    in the database.
#'
#' @param debug Printing additional information if set to \code{TRUE}; default
#'   is \code{FALSE}.
#'
#' @inheritParams ctrMongo
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
dbGetVariablesIntoDf <- function(fields = "", debug = FALSE,
                                 collection = "ctrdata", db = "users", url = "mongodb://localhost",
                                 username = "", password = "", verbose = FALSE,
                                 stopifnodata = TRUE) {

  # check parameters
  if (!is.vector(fields) | class(fields) != "character") stop("Input should be a vector of strings of field names.")
  #
  if (any(fields == "", na.rm = TRUE)) stop("'fields' contains empty elements; ",
                                            " please provide a vector of strings of field names.",
                                            " Function dbFindVariable() can be used to find field names.")

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = verbose)[["ctr"]]

  # total number of records in collection, used for max batch size and at function end
  countall <- mongo$count(query = '{"_id":{"$ne":"meta-info"}}')

  # initialise output
  result <- NULL

  # iterate over fields so that we can use a custom function to merge results,
  # given that mongodb clients have different approaches and complex returnn
  for (item in fields) {
    #
    query <- paste0('{"_id": {"$ne": "meta-info"}, "', item, '": {"$gt": ""}}')
    if (debug) message("DEBUG: variable / field: ", item)
    #
    tmp <- try({
      #
      dfi <- mongo$iterate(query = query, fields = paste0('{"_id": 1, "', item, '": 1}'))$batch(size = countall)
      #
      if (debug) message("DEBUG: variable / field ", item, " has length ", length(dfi))
      #
      # attempt custom function to condense into a data frame instead of using data.frame = TRUE
      dfi <- as.data.frame(cbind(sapply(dfi, function(x) as.vector(unlist(x[1]))),
                                 sapply(dfi, function(x) paste0(as.vector(unlist(x[2])), collapse = " / "))
      ), stringsAsFactors = FALSE)
      # name result set
      names(dfi) <- c("_id", item)
      #
    }, silent = FALSE)
    #
    if ( (class(tmp) != "try-error") && (nrow(dfi) > 0) ) {
      # no error
      if (is.null(result)) {
        result <- dfi
      } else {
        result <- merge(result, dfi, by = "_id", all = TRUE)
      }
      #
    } else {
      # try-error occured
      if (stopifnodata)
        stop("For variable / field: ", item,
             " no data could be extracted, please check the contents of the database.")
      else
        warning("For variable / field: ", item,
                " no data could be extracted, please check the contents of the database.")
    }
  } # end for item in fields

  # close database connection
  rm(mongo); gc()

  # finalise output
  if (is.null(result)) stop("No records found which had values for the specified fields.")

  # some results were obtained

  # add metadata
  result <- addMetaData(result,
                        collection = collection, db = db, url = url,
                        username = username, password = password)

  # notify user
  diff <- countall - nrow(result)
  if (diff > 0) warning(diff, " of ", countall,
                        " records dropped which did not have values for any of the specified fields.")

  # return
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
  df[ , 1] <- ifelse(is.na(tt <- df[ , 1]), "", tt)
  df[ , 2] <- ifelse(is.na(tt <- df[ , 2]), "", tt)
  tmp <- paste0(df[ , 1], df[ , 2])

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
    for (i in seq_len(length(levelslist))) {
      tmp <- refactor(tmp, unlist(levelslist[i]), attr(levelslist[i], "names"))
    }

    # convert factor back into string vector
    tmp <- as.character(tmp)

  }

  if (length(tt <- unique(tmp)) > 10)
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

  # check parameters
  if (class(df) != "data.frame") stop("Parameter df is not a data frame.")
  #
  if (is.null(df [["_id"]]) || is.null(df$a2_eudract_number)) stop('Data frame does not include "_id"',
                                                                   ' and "a2_eudract_number" columns.')
  #
  if (nrow(df) == 0) stop("Data frame does not contain records (0 rows).")
  #
  if (!(prefermemberstate %in% countriesEUCTR)) stop("Value specified for prefermemberstate does not match",
                                                     " one of the recognised codes: ",
                                                     paste (sort (countriesEUCTR), collapse = ", "))

  # notify it mismatching parameters
  if (prefermemberstate == "3RD" & !include3rdcountrytrials) {
    warning("Preferred EUCTR version set to 3RD country trials, but include3rdcountrytrials was FALSE,",
            " setting to TRUE.", call. = FALSE, noBreaks. = FALSE)
    include3rdcountrytrials <- TRUE
  }

  # as a first step, handle 3rd country trials e.g. 2010-022945-52-3RD
  # if retained, these trials would count as record for a trial
  if (!include3rdcountrytrials) df <- df[!grepl("-3RD", df[["_id"]]), ]

  # count number of records by eudract number
  tbl <- table(df[["_id"]], df$a2_eudract_number)
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
    # debug: recordnames <- nms[nst[[1]]]
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
    # default is to list all but first record
    # the listed records are the duplicates
    # 3RD country trials would be listed first
    # hence selected, which is not desirable
    # unless chosen as prefermemberstate
    return(rev(sort(recordnames))[-1])
  }

  # finds per trial the desired record; uses prefermemberstate and nms
  result <- lapply(nst, function(x) removeMSversions(x))
  result <- unlist(result)

  # eleminate the unwanted EUCTR records
  df <- df [!(df [["_id"]] %in% result), ]
  # also eliminate the meta-info record
  df <- df [!(df [["_id"]] == "meta-info"), ]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result)))
    message("Searching multiple country records: Found ", tmp,
            " EUCTR _id that were not the preferred member state record(s) for the trial.")

  # return
  return(df)
  #
}
# end dfFindUniqueEuctrRecord


#' Title
#'
#' @param  x object to be annotated
#' @inheritParams ctrMongo
#'
#' @keywords internal
#'
addMetaData <- function(x, url, db, collection, username, password) {

  # add metadata
  attr(x, "ctrdata-using-mongodb-url")        <- url
  attr(x, "ctrdata-using-mongodb-db")         <- db
  attr(x, "ctrdata-using-mongodb-collection") <- collection
  attr(x, "ctrdata-using-mongodb-username")   <- username
  attr(x, "ctrdata-created-timestamp")        <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(x, "ctrdata-from-dbqueryhistory")      <- dbQueryHistory(collection = collection, db = db, url = url,
                                                                username = username, password = password,
                                                                verbose = FALSE)

  # return annotated object
  return(x)
}



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
  if (!force & dir.exists("c:\\cygwin")) stop("cygwin is already installed. To overwrite, use force = TRUE.")
  #
  # create directory within R sessions temporary directory
  tmpfile <- paste0(tempdir(), "/cygwin_inst")
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")
  #
  # download.file uses the proxy configured in the system
  if (grepl("64-bit", utils::sessionInfo()$platform))
    utils::download.file(url = "http://cygwin.org/setup-x86_64.exe", destfile = dstfile, quiet = TRUE, mode = "wb")
  if (grepl("32-bit", utils::sessionInfo()$platform))
    utils::download.file(url = "http://cygwin.org/setup-x86.exe",    destfile = dstfile, quiet = TRUE, mode = "wb")
  #
  # check
  if (!file.exists(dstfile))             stop("Download failed. Please install manually.")
  if (file.size(dstfile) < (5 * 10 ^ 5)) stop("Download failed (file too small). Please install manually.")
  #
  if (proxy != "") {
    # manual setting overrides all
    proxy <- paste0(" --proxy ", proxy)
    message("Setting cygwin proxy install argument to: ", proxy, ", based on provided parameter.")
  } else {
    # detect proxy to be used, automatically or manually configured?
    # find and use proxy settings for actually running the cygwin setup
    # - Windows 7 see https://msdn.microsoft.com/en-us/library/cc980059.aspx
    # - Windows 10 see
    tmp <- try(utils::readRegistry("Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings",
                                   hive = "HCU"), silent = TRUE)
    #
    if (class(tmp) != "try-error" && !is.null(tmp)) {
      #
      if (length (tmp$AutoConfigURL) > 0) {
        # retrieve settings
        proxypacfile <- paste0(tmpfile, "/pacfile.txt")
        utils::download.file(tmp$AutoConfigURL, proxypacfile)
        # for testing: proxypacfile <- "private/proxypacfile"
        # find out and select last mentioned proxy line
        proxypac <- readLines(proxypacfile)
        proxypac <- proxypac[grepl("PROXY", proxypac)]
        proxypac <- proxypac[length(proxypac)]
        proxy <- sub(".*PROXY ([0-9]+.[0-9]+.[0-9]+.[0-9]+:[0-9]+).*", "\\1", proxypac)
        if (proxy == "") stop("A proxy could not be identified using the system\'s automatic configuration script.",
                              ' Please set manually: installCygwinWindowsDoInstall (proxy = "host_or_ip:port"')
        proxy <- paste0(" --proxy ", proxy)
        message("Automatically setting cygwin proxy to: ", proxy, ", based on AutoConfigProxy in registry.")
        #
      } else {
        if (!is.null(tmp$ProxyServer)) {
          proxy <- paste0(" --proxy ", tmp$ProxyServer)
          message("Automatically setting cygwin proxy to: ", proxy, ", based on ProxyServer in registry.")
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
  installcmd <- paste0("--no-admin --quiet-mode --verbose --upgrade-also --root c:/cygwin ",
                       "--site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ ",
                       "--packages perl,php-jsonc,php-simplexml")
  #
  # execute cygwin setup command
  system(paste0(dstfile, " ", installcmd, " --local-package-dir ", tmpfile, " ", proxy))
  #
  # test cygwin installation
  installCygwinWindowsTest()
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
    message("cygwin does not seem to be installed correctly.")
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
#'   binaries, defaults to "c:\\mongodb\\bin\\". See
#'   \url{http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows#interactive-installation}
#' @param debug Printing additional information if set to \code{TRUE}; default
#'   is \code{FALSE}.
#'
#' @return A vector of length 2 with the paths for the \code{mongo} and
#'   \code{mongoimport} binaries on the user's system
#'
#' @keywords internal
#
installMongoFindBinaries <- function(mongoDirWin = "c:\\mongodb\\bin\\", debug = FALSE) {
  #
  environ <- .privateEnv
  #
  binaries <- paste0(c("mongo", "mongoimport"), ifelse(.Platform$OS.type != "windows", "", ".exe"))
  #
  if (exists("mongoBinaryLocation", envir = environ)
      && !is.na(get("mongoBinaryLocation", envir = environ))
      && file.exists(paste0(get("mongoBinaryLocation", envir = environ), binaries[1]))) {
    #
    if (debug) message("mongoimport / mongo is in ", get("mongoBinaryLocation", envir = environ))
    #
  } else {
    #
    # check folder specified in parameter
    mongoDirWin <- gsub("[\\]*$", "\\\\", mongoDirWin)
    #
    if (.Platform$OS.type == "windows"
        && file.exists(paste0(mongoDirWin, binaries[1]))) {
      #
      assign("mongoBinaryLocation", mongoDirWin, envir = environ)
      if (debug) message("mongoimport / mongo is in ", mongoDirWin)
      #
    } else {
      #
      # not found: reset any information and start searching
      assign("mongoBinaryLocation", NA, envir = environ)
      #
      # first test for binary in the path
      tmp <- try(if (.Platform$OS.type != "windows") {
        system("mongoimport --version", intern = TRUE)
      } else {
        system("mongoimport.exe --version", intern = TRUE)
      }, silent = TRUE)
      #
      if (class(tmp) != "try-error") {
        #
        # found it in the path, save empty location string in package environment
        if (debug) message("mongoimport / mongo found in the path.")
        assign("mongoBinaryLocation", "", envir = environ)
        #
      } else {
        #
        if (debug) message("mongoimport / mongo not found in path.")
        #
        if (.Platform$OS.type != "windows") stop("Cannot continue. Search function is only for MS Windows.")
        #
        # second search for folder into which mongo was installed
        location <- try(utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Installer\\Folders",
                                            hive = "HLM"), silent = TRUE)
        #
        if (class(location) == "try-error") stop("Cannot continue. mongo not found recorded in the registry.")
        #
        location <- names(location)
        location <- location[grepl("Mongo", location)]
        location <- location[grepl("bin", location)]
        #
        tmp <- file.exists(paste0(location, "mongoimport.exe"))
        #
        if (!tmp) stop("Cannot continue. mongoimport not found recorded in the registry, ", location, ".")
        #
        # found it, save in package environment
        assign("mongoBinaryLocation", location, envir = environ)
        if (debug) message("mongoimport / mongo found in ", location)
        #
      }
    }
  }
  #
  return(paste0(get("mongoBinaryLocation", envir = environ), binaries))
  #
}
# end installMongoFindBinaries


#' Check the version of the build of the mongo server to be used
#'
#' In addition to the returned value, the function will generate a warning
#' message if applicable.
#'
#' @inheritParams ctrMongo
#'
#' @return A logical value indicating if the mongodb version is acceptable for
#'   use with this package.
#'
#' @keywords internal
#
installMongoCheckVersion <- function(collection = "ctrdata", db = "users", url = "mongodb://localhost",
                                     username = "", password = "", verbose = FALSE) {
  #
  # get a working mongo connection, select trial record collection
  mongo <- suppressMessages(ctrMongo(collection = collection, db = db, url = url,
                                     username = username, password = password, verbose = verbose))[["ctr"]]
  # get mongo server infos
  result <- mongo$info()$server
  #
  if (grepl("^3", result$version)) {
    #
    return(TRUE)
    #
  } else {
    #
    warning("mongodb not version 3. Earlier versions have limitations that may break function ctrLoadQueryIntoDb()",
            " in package ctrdata.\nPlease upgrade, see http://docs.mongodb.org/manual/installation/. \n",
            "Trying to continue. Support for versions other than 3 may be discontinued.", immediate. = TRUE)
    return(FALSE)
  }
  #
  # close database connection
  rm(mongo); gc()
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
  if (.Platform$OS.type == "windows") commandtest <-
      paste0("cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c ", shQuote(commandtest))
  #
  if (debug) print(commandtest)
  #
  commandresult <- try(
    system(commandtest, intern = TRUE),
    silent = TRUE
  )
  #
  commandreturn <- ifelse (class(commandresult) == "try-error" ||
                             grepl("error", tolower(paste(commandresult, collapse = " "))), FALSE, TRUE)
  #
  if (!commandreturn) warning(commandtest, " not found.")
  #
  if (debug) print(commandresult)
  #
  return(commandreturn)
  #
}
# end installFindBinary
