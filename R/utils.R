### ctrdata package
### utility functions

## variable definitions
#
# EUCTR definitions
countriesEUCTR <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
                    "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
                    "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB",
                    "IS", "LI", "NO",
                    "3RD")


#' Set up connections to a Mongo DB server database
#'
#' @param collection Name of collection (default is "ctrdata")
#'
#' @param uri Default is mongodb://localhost/users/.
#'  Address of database in mongodb server, based on mongo connection string
#'  format: mongodb://[username@]host1[:port1][,...hostN[:portN]]]/database/
#'  Do NOT include password, this will only be used from the parameter.
#'  See \url{http://docs.mongodb.org/manual/reference/connection-string/}
#'
#' @param password In case access requires credentials.
#'  Note this defaults to the environment variable "ctrdatamongopassword".
#'  (by means of \code{Sys.getenv("ctrdatamongopassword")}), to
#'  support scripting without revealing secrets.
#'
#' @param verbose Print information.
#'
#' @return A mongo data base object, currently using mongolite
#'
#' @keywords internal
#'
#' @importFrom mongolite mongo
#'
ctrMongo <- function(collection = "ctrdata",
                     uri = "mongodb://localhost/users",
                     password = Sys.getenv("ctrdatamongopassword"),
                     verbose = FALSE) {

  # mongo versions tested
  # - local 3.4.18 (macOS)
  # - travis 3.4.20
  # - appveyor ? (https://www.appveyor.com/docs/services-databases/#mongodb)

  # references
  # https://docs.mongodb.com/manual/reference/connection-string/
  # https://docs.mongodb.com/manual/reference/program/mongoimport/

  # The +srv indicates to the client that the hostname that follows corresponds to a DNS SRV record.
  # Use of the +srv connection string modifier automatically sets the ssl option to true for the connection.
  # Override this behavior by explicitly setting the ssl option to false with ssl=false in the query string.

  # For a standalone that enforces access control:
  # mongodb://myDBReader:D1fficultP%40ssw0rd@mongodb0.example.com:27017/admin

  # For a sharded cluster that enforces access control, include user credentials:
  # mongodb://myDBReader:D1fficultP%40ssw0rd@mongos0.example.com:27017,mongos1.example.com:27017,mongos2.example.com:27017/admin

  # For a replica set, specify the hostname(s) of the mongod instance(s) as listed in the replica set configuration.
  # For a replica set, include the replicaSet option.
  # mongodb://myDBReader:D1fficultP%40ssw0rd@mongodb0.example.com:27017,mongodb1.example.com:27017,mongodb2.example.com:27017/admin?replicaSet=myRepl

  # --uri <connectionString>
  #   New in version 3.4.6.
  # Specify a resolvable URI connection string for the mongod to which to connect.

  # /usr/local/opt/mongodb/bin/mongoimport
  # --host "Cluster0-shard-0/cluster0-shard-00-00-b9wpw.mongodb.net:27017,cluster0-shard-00-01-b9wpw.mongodb.net:27017,cluster0-shard-00-02-b9wpw.mongodb.net:27017"
  # --ssl --username "admin" --password "admin" --authenticationDatabase admin --db "dbtemp" --collection "dbcoll"
  # --type "json" --file "private/2007-001012-23.json"

  # /usr/local/opt/mongodb/bin/mongoimport --host "cluster0-shard-00-00-b9wpw.mongodb.net:27017"
  # --ssl --username "admin" --password "admin" --authenticationDatabase admin --db "dbtemp" --collection "dbcoll"
  # --type "json" --file "private/2007-001012-23.json"

  # Example NDJSON
  # {"some":"thing"}
  # {"foo":17,"bar":false,"quux":true}
  # {"may":{"include":"nested","objects":["and","arrays"]}}

  ## check parameters
  # remove unwanted characters
  uri <- gsub("[^a-zA-Z0-9%?=@/:+_.-]", "", uri)
  uri <- gsub(":@", "@", uri)

  ## password if any to uri
  # encode password
  password <- utils::URLencode(password)
  # insert into uri if uri has a username
  if ((password != "") && grepl("//.+@", uri))
    uri <- sub("(.+)@(.+)", paste0("\\1", ":", password, "@\\2"), uri)

  # check and set proxy if needed to access internet
  # TODO
  # setProxy()

  # connect to mongo server
  valueCtrDb <- mongolite::mongo(collection = collection,
                                 url = uri,
                                 verbose = verbose)

  # inform user
  if (verbose) message("Using MongoDB (collections \"", collection,
                       "\" in database \"", uri, "\").")

  return(invisible(valueCtrDb))
}
# end ctrMongo


#' Open advanced search pages of register(s) or execute search in browser
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
    # if no register is specified, open both
    if (all(register == "", na.rm = TRUE)) register <- c("EUCTR", "CTGOV")
    #
    # open empty search pages
    if ("EUCTR" %in% register)
      try({utils::browseURL("https://www.clinicaltrialsregister.eu/ctr-search/search", ...)}, silent = TRUE)
    #
    if ("CTGOV" %in% register)
      try({utils::browseURL("https://clinicaltrials.gov/ct2/search/advanced", ...)}, silent = TRUE)
    #
    # if requested also show copyright pages
    if (copyright) {
      #
      if ("EUCTR" %in% register)
        try({utils::browseURL("https://www.clinicaltrialsregister.eu/disclaimer.html", ...)}, silent = TRUE)
      #
      if ("CTGOV" %in% register)
        try({utils::browseURL("https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use", ...)}, silent = TRUE)
      #
    }
  } else {
    #
    # check input argument and determine action
    #
    # - is a url
    if (class(input) == "character" &&
        is.atomic(input) &&
        length(input) == 1 &&
        grepl("^https.+clinicaltrials.+", input)) {
      #
      input <- ctrGetQueryUrlFromBrowser(content = input)
      #
    }
    #
    # - data frame as returned from ctrQueryHistoryInDb() and ctrGetQueryUrlFromBrowser()
    if (is.data.frame(input) && all(substr(names(input), 1, 6) == "query-")) {
      #
      nr <- nrow(input)
      #
      if (nr > 1) warning("Using last row of input.", call. = FALSE, immediate. = TRUE)
      #
      register  <- input[nr, "query-register"]
      queryterm <- input[nr, "query-term"]
      #
    }
    #
    # - if input is not a complete url, but register is specified
    if (class(input) == "character" &&
        is.atomic(input) &&
        length(input) == 1 &&
        register != "") {
      #
      queryterm <- input
      #
    }
    #
    if (exists("queryterm") && queryterm != "" && register != "") {
      #
      message("Opening browser for search: \n\n", queryterm, "\n\nin register: ", register)
      #
      # sanity correction for naked terms
      if (register == "EUCTR") queryterm <-
          sub("(^|&|[&]?\\w+=\\w+&)(\\w+|[ +ORNCT0-9-]+)($|&\\w+=\\w+)",
              "\\1query=\\2\\3",
              queryterm)
      if (register == "CTGOV") queryterm <-
          sub("(^|&|[&]?\\w+=\\w+&)(\\w+|[NCT0-9-]+)($|&\\w+=\\w+)",
              "\\1term=\\2\\3",
              queryterm)
      #
      # protect against os where this does not work
      try({utils::browseURL(url = paste0(
        #
        switch(as.character(register),
               "CTGOV" = ifelse(grepl("^xprt=", queryterm),
                                "https://clinicaltrials.gov/ct2/results/refine?show_xprt=Y&",
                                "https://clinicaltrials.gov/ct2/results?"),
               "EUCTR" = "https://www.clinicaltrialsregister.eu/ctr-search/search?"),
        queryterm),
        encodeIfNeeded = TRUE, ...)
      }, silent = TRUE)
    }
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
#' @importFrom clipr read_clip
#'
ctrGetQueryUrlFromBrowser <- function(content = "") {
  #
  # if content parameter not specified, get and check clipboard contents
  if (length(content) == 1L && nchar(content) == 0L) content <- clipr::read_clip()
  #
  if (length(content) != 1L) {
    stop ("ctrGetQueryUrlFromBrowser(): no clinical trial register search URL found ",
         "in parameter 'content' or in clipboard.", call. = FALSE)
    return(NULL)
  }
  #
  # EUCTR
  if (grepl("https://www.clinicaltrialsregister.eu/ctr-search/", content)) {
    #
    queryterm <- sub("https://www.clinicaltrialsregister.eu/ctr-search/search[?](.*)",      "\\1", content)
    queryterm <- sub("https://www.clinicaltrialsregister.eu/ctr-search/trial/([-0-9]+)/.*", "\\1", queryterm)
    #
    # sanity correction for naked terms
    if (!grepl("&\\w+=\\w+|query=\\w", queryterm)) queryterm <- paste0("query=", queryterm)
    #
    # check if url was for results of single trial
    if (grepl(".*/results$", content)) queryterm <- paste0(queryterm, "&resultsstatus=trials-with-results")
    #
    message("* Found search query from EUCTR.")
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
    message("* Found search query from CTGOV.")
    #
    df <- data.frame(cbind(queryterm, "CTGOV"), stringsAsFactors = FALSE)
    names(df) <- c("query-term", "query-register")
    #
    return(df)
  }
  #
  warning("ctrGetQueryUrlFromBrowser(): no clinical trial register search URL found ",
          "in parameter 'content' or in clipboard.", call. = FALSE, immediate. = TRUE)
  #
  return(invisible(NULL))
}
# end ctrGetQueryUrlFromBrowser



#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name, a trade or product name, or one or more company codes.
#'
#' At this time, this function uses the register ClinicalTrials.Gov to
#' detect which substances were also searched for.
#'
#' @param activesubstance An active substance, in an atomic character vector
#'
#' @return A character vector of the active substance (input parameter) and
#'  synonyms, if any were found
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_table
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#' }
#'
ctrFindActiveSubstanceSynonyms <- function(activesubstance = ""){

  # check parameters
  if ( (length(activesubstance) != 1) ||
       !is.character(activesubstance) ||
       (nchar(activesubstance) == 0) )
    stop("ctrFindActiveSubstanceSynonyms(): activesubstance should be a single string.", call. = FALSE)

  # initialise output variable
  as <- activesubstance

  # check and set proxy if needed to access internet
  setProxy()

  # getting synonyms
  ctgovdfirstpageurl <- paste0("https://clinicaltrials.gov/ct2/results/details?term=", activesubstance)
  tmp <- xml2::read_html(x = utils::URLencode(ctgovdfirstpageurl))
  tmp <- rvest::html_node(tmp, xpath = '//*[@id="searchdetail"]//table[1]')
  tmp <- rvest::html_table(tmp, fill = TRUE)
  asx <- tmp[, 1]
  asx <- asx[!grepl(paste0("(more|synonyms|terms|", as, "|",
                           paste0(unlist(strsplit(as, " ")), collapse = "|"),
                           ")"), asx, ignore.case = TRUE)]

  # prepare and return output
  as <- c(as, asx)
  as <- unique(as)
  return(as)
}
# end ctrFindActiveSubstanceSynonyms



#' Show the history of queries that were loaded into a database collection
#'
#' @inheritParams ctrMongo
#'
#' @return A data frame with columns: query-timestamp, query-egister,
#'  query-records (note: this is the number of records loaded when last executing
#'  ctrLoadQueryIntoDb(), not the total record number) and query-term,
#'  and with one row for each ctrLoadQueryIntoDb() loading trial records
#'  in this collection.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dbQueryHistory()
#' }
#'
dbQueryHistory <- function(collection = "ctrdata", uri = "mongodb://localhost/users",
                           password = Sys.getenv("ctrdatamongopassword"), verbose = FALSE) {

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = verbose)

  # Get record from mongo db using batch because find would
  # try to return a dataframe and this would ignore the array
  tmp <- mongo$iterate(query = '{"_id":{"$eq":"meta-info"}}', fields = '{"queries": 1, "_id": 0}')$batch()

  # Select only relevant element from record
  tmp <- tmp[[1]]$queries

  # Check if meeting expectations
  if (!is.list(tmp) || (length(tmp) < 1)) {
    #
    message("No history found in expected format.")
    tmp <- data.frame(NULL)
    #
  } else {
    # Change into data frame with appropriate column names
    tmp <- sapply(tmp, function(x) do.call(rbind, x))
    tmp <- t(tmp)
    tmp <- data.frame(tmp, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
    if (ncol(tmp) != 4) warning(tmp, call. = FALSE, immediate. = TRUE)
    names(tmp) <- c("query-timestamp", "query-register", "query-records", "query-term")
    # Inform user
    if (verbose) message("Number of queries in history of \"", collection, "\": ", nrow(tmp))
  }

  # total number of records in collection to inform user
  countall <- mongo$count(query = '{"_id":{"$ne":"meta-info"}}')
  if (verbose) message("Number of records in collection \"", collection, "\": ", countall)

  # close database connection
  mongo$disconnect()

  # return
  return(tmp)

}
# end ctrQueryHistoryInDb


#' Find names of fields in the database collection
#'
#' Given part of the name of a field of interest to the user, this function
#' returns the full field names as found in the database. It is not necessary to
#' add wild cards to the name of the field of interest.
#'
#' For fields in EUCTR (protocol- and results-related information), see also the
#' register's documentation: \url{https://eudract.ema.europa.eu/}.
#'
#' For fields in CTGOV (protocol-related information), see also the register's
#' definitions: \url{https://prsinfo.clinicaltrials.gov/definitions.html}
#'
#' Note that generating a list of fields with this function may take some time,
#' since a mapreduce function is run on the server. If the user is not
#' not authorised to run such a function on the (local or remote) server,
#' random documents are samples to generate a list of fields.
#'
#' @param namepart A plain string (not a regular expression) to be searched for
#'   among all field names (keys) in the database.
#'
#' @param allmatches If \code{TRUE} (default), returns all keys if more than one is found,
#'   returns only first if \code{FALSE}.
#'
#' @param debug If \code{TRUE}, prints additional information (default \code{FALSE}).
#'
#' @inheritParams ctrMongo
#'
#' @return Vector of field(s) found
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  dbFindFields("date")
#' }
#'
dbFindFields <- function(namepart = "", allmatches = TRUE, debug = FALSE,
                         collection = "ctrdata", uri = "mongodb://localhost/users",
                         password = Sys.getenv("ctrdatamongopassword"), verbose = FALSE) {

  ## sanity checks
  if (!is.atomic(namepart)) stop("Name part should be atomic.", call. = FALSE)
  if (length(namepart) > 1) stop("Name part should have only one element.", call. = FALSE)
  if (namepart == "") stop("Empty name part string.", call. = FALSE)

  # ## check if cache for list of keys in collection exists,
  # # otherwise create new environment as session cache
  if (!exists(".dbffenv")) {
    .dbffenv <- new.env(parent = emptyenv())
  }

  ## check if cache environment has entry for this collection,
  if (exists(x = paste0(uri, "/", collection),
             envir = .dbffenv)) {

    # if true, get keys list from cache
    keyslist <- get(x = paste0(uri, "/", collection),
                    envir = .dbffenv)

    # informing user
    message("Using cache of fields.")

  } else {

    # get keys list from database

    # get a working mongo connection
    mongo <- ctrMongo(collection = collection, uri = uri,
                      password = password, verbose = verbose)

    # informing user
    message("Finding fields on server (this may take some time)")

    # try mapreduce to get all keys
    keyslist <- try({mongo$mapreduce(
      map = "function() {
        obj = this;
        return searchInObj(obj, '');
        function searchInObj(obj, pth){
           for(var key in obj){
              if(typeof obj[key] == 'object' && obj[key] !== null){
                 if(pth != '') {pth = pth + '.'}
                    searchInObj(obj[key], pth + key);
              }else{
                 key = pth + '.' + key;
                 key = key.replace(/[.][0-9]+[.]/g, '.');
                 key = key.replace(/[.][0-9]+$/, '');
                 key = key.replace(/[.][.]+/g, '.');
                 key = key.replace(/^[.]/, '');
                 emit(key, 1);
        }}}}",
      reduce = "function(id, counts) {return Array.sum(counts)}"
      # extract and keep only "_id" = first column, with keys
    )[["_id"]]}, silent = TRUE)

    # mapreduce may not work or not be permitted such
    # as on some free mongo servers, thus revert to guessing
    if (class(keyslist) == "try-error") {

      warning("Mongo server returned: ", as.character(keyslist),
              "Using alternative method (extracting keys from sample documents, may be incomplete).",
              call. = FALSE)

      # get 2 random documents, one for each register EUCTR and CTGOV,
      # if in collection, and retrieve keys from documents

      keyslist <- c("",
                    names(mongo$find(query = '{"_id": { "$regex": "^NCT[0-9]{8}", "$options": ""} }',
                                     limit = 1L)),
                    names(mongo$find(
                      query = '{"_id": { "$regex": "^[0-9]{4}-[0-9]{6}", "$options": ""} }',
                      limit = 1L))
                    )

      keyslist <- unique(keyslist)
      if (length(keyslist) > 1) keyslist <- keyslist[keyslist != ""]

      # inform user if unexpected result
      if (all(keyslist == "")) warning("No keys could be extracted, please check collection ",
                                       collection, call. = FALSE)

    }

    # close database connection
    mongo$disconnect()

    ## store keyslist to environment (cache)
    assign(x = paste0(uri, "/", collection),
           value = keyslist,
           envir = .dbffenv)

  } # end get cached list or generate new list

  # inform user if unexpected situation
  if ((length(keyslist) == 0) || all(keyslist == "")) {
    warning("No keys could be extracted, please check collection ",
            collection, call. = FALSE)
  }

  ## now do the actual search and find for key name parts
  if (namepart != "") {

    # actually now find keys / field names
    fields <- keyslist[grepl(tolower(namepart), tolower(keyslist))]

    # all or only first field name?
    if (!allmatches) {
      if ( (tmp <- length(fields)) > 1) message("Returning first of ", tmp, " fields found.")
      fields <- fields[1]
    }

    # return the first match / all matches
    return(fields)

  }

}
# end dbFindFields


#' Deduplicate records to provide unique clinical trial identifiers
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
                                  collection = "ctrdata", uri = "mongodb://localhost/users",
                                  password = Sys.getenv("ctrdatamongopassword"), verbose = TRUE) {

  # parameter checks
  if (!grepl(preferregister, "CTGOVEUCTR")) stop("Register not known: ", preferregister, call. = FALSE)

  # objective: create a list of mongo database record identifiers (_id)
  # that represent unique records of clinical trials, based on user's
  # preferences for selecting the preferred from any multiple records

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = verbose)
  #
  # total number of records in collection to inform user
  countall <- mongo$count(query = '{"_id":{"$ne":"meta-info"}}')
  if (verbose) message("* Total of ", countall, " records in collection.")

  # 1. get euctr records
  listofEUCTRids <- try(suppressMessages(suppressWarnings(
    dbGetFieldsIntoDf(fields = c("a2_eudract_number",
                                 "a41_sponsors_protocol_code_number",
                                 "a51_isrctn_international_standard_randomised_controlled_trial_number",
                                 "a52_us_nct_clinicaltrialsgov_registry_number"),
                      debug = FALSE,
                      collection = collection, uri = uri,
                      password = password, verbose = FALSE,
                      stopifnodata = FALSE)
    )),
    silent = TRUE
  )
  attribsids <- attributes(listofEUCTRids)
  if (class(listofEUCTRids) == "try-error") listofEUCTRids <- NULL
  if (all(is.na(listofEUCTRids[, -1])))     listofEUCTRids <- NULL
  if (is.null(listofEUCTRids)) message("No EUCTR records found.")

  # extract eudract number
  if (!is.null(listofEUCTRids)) listofEUCTRids <-
    listofEUCTRids[grepl("[0-9]{4}-[0-9]{6}-[0-9]{2}-?[3A-Z]{0,3}", listofEUCTRids[["_id"]]), ]

  # 2. find unique, preferred country version of euctr
  if (!is.null(listofEUCTRids)) listofEUCTRids <-
    dfFindUniqueEuctrRecord(df = listofEUCTRids,
                            prefermemberstate = prefermemberstate,
                            include3rdcountrytrials = include3rdcountrytrials)

  # 3. get ctrgov records
  listofCTGOVids <- mongo$iterate(
    query =  '{"_id": { "$regex": "^NCT[0-9]{8}", "$options": ""} }',
    fields = '{"id_info.org_study_id": 1,
               "id_info.secondary_id": 1,
               "id_info.nct_alias": 1}'
    )$batch(size = mongo$count())

  # inform user
  if (is.null(listofCTGOVids)) message("No CTGOV records found.")

  # close database connection
  mongo$disconnect()

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
      dupes.b2 <- sapply(listofCTGOVids, "[[", 1) %in%
                  listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.b2),
                           " CTGOV _id in EUCTR a52_us_nct_clinicaltrialsgov_registry_number")
      #
      # a2 - ctgov in euctr a2_...
      dupes.a2 <- sapply(lapply(listofCTGOVids, function(x) sub(".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*",
                                                                "\\1", unlist(x[["id_info"]]))),
                         function(x) any(x %in% listofEUCTRids[["a2_eudract_number"]]))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.a2),
                           " CTGOV otherids (secondary_id, nct_alias, org_study_id) in EUCTR a2_eudract_number")
      #
      # c.2 - ctgov in euctr a52_... (id_info corresponds to index 2)
      dupes.c2 <- sapply(lapply(listofCTGOVids, "[[", 2),
                         function(x) any(unlist(x) %in%
                         listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.c2),
                           " CTGOV otherids (secondary_id, nct_alias, org_study_id) in",
                           " EUCTR a52_us_nct_clinicaltrialsgov_registry_number")
      #
      # d.2 - ctgov in euctr a51_... (id_info corresponds to index 2)
      dupes.d2 <- sapply(lapply(listofCTGOVids, "[[", 2),
                         function(x) any(unlist(x) %in%
                         listofEUCTRids[["a51_isrctn_international_standard_randomised_controlled_trial_number"]]))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.d2),
                           " CTGOV otherids (secondary_id, nct_alias, org_study_id) in",
                           " EUCTR a51_isrctn_international_standard_randomised_controlled_trial_number")
      #
      # finalise results set
      listofEUCTRids <- listofEUCTRids[["_id"]]
      listofCTGOVids <- sapply(listofCTGOVids, "[[", 1) [ !dupes.a2 & !dupes.b2 & !dupes.c2 & !dupes.d2 ]
      #
      message("Concatenating ",
              length(listofEUCTRids), " records from EUCTR and ",
              length(listofCTGOVids), " records from CTGOV.")
      #
      retids <- c(listofEUCTRids, listofCTGOVids)
      #
    }
    #
    if (preferregister == "CTGOV") {
      #
      # a.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.a1 <- listofEUCTRids[["a2_eudract_number"]] %in% sub(".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*",
                                                                "\\1", unlist(sapply(listofCTGOVids, "[[", 2)))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.a1),
                           " EUCTR _id in CTGOV otherids (secondary_id, nct_alias, org_study_id)")
      #
      # b.1 - euctr in ctgov (_id corresponds to index 1)
      dupes.b1 <- listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]] %in%
                  sapply(listofCTGOVids, "[[", 1)
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.b1),
                           " EUCTR a52_us_nct_clinicaltrialsgov_registry_number in CTGOV _id")
      #
      # c.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.c1 <- listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]] %in%
                  unlist(sapply(listofCTGOVids, "[[", 2))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.c1),
                           " EUCTR a52_us_nct_clinicaltrialsgov_registry_number in",
                           " CTOGV otherids (secondary_id, nct_alias, org_study_id)")
      #
      # d.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.d1 <- listofEUCTRids[["a51_isrctn_international_standard_randomised_controlled_trial_number"]] %in%
                  unlist(sapply(listofCTGOVids, "[[", 2))
      #
      if (verbose) message("Searching duplicates: Found ", sum(dupes.d1),
                           " EUCTR a51_isrctn_international_standard_randomised_controlled_trial_number",
                           " in CTOGV otherids (secondary_id, nct_alias, org_study_id)")
      #
      # finalise results set
      listofCTGOVids <- sapply(listofCTGOVids, "[[", 1)
      listofEUCTRids <- listofEUCTRids[["_id"]] [ !dupes.a1 & !dupes.b1 & !dupes.c1 & !dupes.d1 ]
      #
      message("Concatenating ",
              length(listofCTGOVids), " records from CTGOV and ",
              length(listofEUCTRids), " records from EUCTR.")
      #
      retids <- c(listofCTGOVids, listofEUCTRids)
      #
    }
  } else {
    #
    retids <- c(listofEUCTRids[["_id"]], unlist(sapply(listofCTGOVids, "[[", 1)))
    #
  }

  # prepare output
  attributes(retids) <- attribsids[grepl("^ctrdata-", names(attribsids))]

  # avoid returning list() if none found
  if (length(retids) == 0) retids <- character()

  # inform user
  message("= Returning keys (_id) of ", length(retids),
          " records out of total of ", countall,
          " records in collection \"", collection, "\".")
  #
  return(retids)

}
# end dbFindIdsUniqueTrials


#' Create data frame by extracting specified fields from database collection
#'
#' With this convenience function, fields in the mongo database are retrieved
#' into an R dataframe. As mongo json fields within the record of a trial
#' can be hierarchical and structured, the function flattens the data and
#' returns a concatenation
#' of values if there is more than one value or if the field is (in) an array,
#' such as follows: value 1 / value 2 / ... (see example)
#'
#' For more sophisticated data retrieval from the database, see vignette examples
#' and other packages to query mongodb such as mongolite.
#'
#' @param fields Vector of one or more strings, with names of the sought fields.
#'    See function \link{dbFindFields} for how to find names of fields.
#'
#' @param stopifnodata Stops with an error (\code{TRUE}, default) or with a warning
#'    (\code{TRUE}) if sought field is empty or not available in any of the records
#'    in the database collection.
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
#' dbGetFieldsIntoDf("b1_sponsor.b31_and_b32_status_of_the_sponsor")[1,]
#' #                   _id  b1_sponsor.b31_and_b32_status_of_the_sponsor
#' #  1  2004-000015-25-GB                   Non-commercial / Commercial
#'
#' dbGetFieldsIntoDf("keyword")[1,]
#' #            _id                                           keyword
#' #  1 NCT00129259  T1D / type 1 diabetes / type 1 diabetes mellitus
#'
#' }
#'
dbGetFieldsIntoDf <- function(fields = "", debug = FALSE,
                              collection = "ctrdata", uri = "mongodb://localhost/users",
                              password = Sys.getenv("ctrdatamongopassword"), verbose = FALSE,
                              stopifnodata = TRUE) {

  # check parameters
  if (!is.vector(fields) | class(fields) != "character")
    stop("Input should be a vector of strings of field names.", call. = FALSE)
  #
  # remove _id if inadventertently mentioned in fields
  fields <- fields["_id" != fields]
  #
  # check if valid fields
  if (any(fields == "", na.rm = TRUE) | (length(fields) == 0))
    stop("'fields' contains empty elements; ",
         " please provide a vector of strings of field names.",
         " Function dbFindFields() can be used to find field names.",
         call. = FALSE)

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, uri = uri,
                    password = password, verbose = verbose)

  # total number of records in collection, used for max batch size and at function end
  countall <- mongo$count(query = '{"_id":{"$ne":"meta-info"}}')

  # provide list of ids
  idsall <- mongo$find(query = '{"_id":{"$ne":"meta-info"}}', fields = '{"_id" : 1}')

  # initialise output
  result <- NULL

  # iterate over fields so that we can use a custom function to merge results,
  # given that mongodb clients have different approaches and complex returns
  for (item in fields) {
    #
    query <- paste0('{"_id": {"$ne": "meta-info"}}')
    if (debug) message("DEBUG: field: ", item)
    #
    tmp <- try({
      #
      dfi <- mongo$iterate(query = query, fields = paste0('{"_id": 1, "', item, '": 1}'))$batch(size = countall)
      #
      if (debug) message("DEBUG: field ", item, " has length ", length(dfi))
      #
      # attempt custom function to condense into a data frame instead of using data.frame = TRUE
      dfi <- as.data.frame(cbind(sapply(dfi, function(x) as.vector(unlist(x[1]))),
                                 sapply(dfi, function(x) paste0(as.vector(unlist(x[2])), collapse = " / "))
      ), stringsAsFactors = FALSE)
      #
      # name result set
      names(dfi) <- c("_id", item)
      #
    },
    silent = FALSE)
    #
    if ( !((class(tmp) != "try-error") & any(nchar(dfi[, 2]) != 0) ) ) {
      # try-error occured or no data retrieved
      if (stopifnodata) {
        stop(paste0("For field: ", item, " no data could be extracted from the database collection.",
                    "Use dbGetFieldsIntoDf(stopifnodata = FALSE) to continue extracting other fields."),
             call. = FALSE)
      } else {
        warning(paste0("For field: ", item, " no data could be extracted from the database collection."),
                call. = FALSE,
                immediate. = FALSE)
        # create empty data set
        dfi <- as.data.frame(cbind(idsall, rep(NA, times = nrow(idsall))), stringsAsFactors = FALSE)
        # name result set
        names(dfi) <- c("_id", item)
      }
    }
    # not stopped, no error and some content, thus append to result
    if (is.null(result)) {
      result <- typeField(dfi)
    } else {
      # type fields where defined and possible, then
      # merge the new dfi (a data frame of _id, name of item)
      # with data frame of previously retrieved results
      result <- merge(result, typeField(dfi), by = "_id", all = TRUE)
    }
    #
  } # end for item in fields

  # close database connection
  mongo$disconnect()

  # finalise output
  if (is.null(result)) stop("No records found which had values for the specified fields.", call. = FALSE)

  # some results were obtained

  # add metadata
  result <- addMetaData(result,
                        collection = collection, uri = uri,
                        password = password)

  # notify user
  diff <- countall - nrow(result)
  if (diff > 0) warning(diff, " of ", countall,
                        " records dropped which did not have values for any of the specified fields.",
                        call. = FALSE, immediate. = FALSE)

  # return
  return(result)
}
# dbGetFieldsIntoDf


#' Merge two variables into one, optionally map values to new levels
#'
#' @param df A \link{data.frame} in which there are two variables (columns) to be
#'   merged into one.
#' @param colnames A vector of length two with names of the two columns that hold
#'   the variables to be merged. See \link{colnames} for how to obtain the names
#'   of columns of a data frame.
#' @param levelslist A list with one slice each for a new value to be used for a
#'   vector of old values (optional).
#' @param ... for deprecated varnames parameter (will be removed)
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
#'
#' dfMergeTwoVariablesRelevel(result, c("Recruitment", "x5_trial_status"), statusvalues)
#' }
#'
dfMergeTwoVariablesRelevel <- function(df = NULL, colnames = "", levelslist = NULL, ...) {

  # check parameters

  # 2019-01-06 migrate from previously used parameter "varnames"
  tmp <- match.call()
  tmp <- tmp["varnames"]
  tmp <- as.list(tmp)[[1]]
  if (length(tmp) == 3 && colnames == "") {
    colnames <- unlist(as.list(tmp[-1]))
    warning("Parameter varnames is deprecated, use colnames instead.", call. = FALSE)
  }

  # other checks
  if (class(df) != "data.frame") stop("Need a data frame as input.", call. = FALSE)
  if (length(colnames)  != 2)    stop("Please provide exactly two column names.", call. = FALSE)

  # find variables in data frame and merge
  tmp <- match(colnames, names(df))
  df <- df[, tmp]
  df[, 1] <- ifelse(is.na(tt <- df[, 1]), "", tt)
  df[, 2] <- ifelse(is.na(tt <- df[, 2]), "", tt)
  tmp <- paste0(df[, 1], df[, 2])

  if (!is.null(levelslist)) {

    # check
    if (class(levelslist) != "list") stop("Need lists for parameter levelslist.", call. = FALSE)

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


#' Select single trial record from records of different EU Member States
#'
#' The EUCTR provides one record per trial per EU Member State in which the
#' trial is conducted. For all trials conducted in more than one Member State,
#' this function returns only one record per trial.
#'
#' Note: To deduplicate trials from different registers (EUCTR and CTGOV),
#' please first use function \code{\link{dbFindIdsUniqueTrials}}.
#'
#' @param df A data frame created from the database that includes the columns
#'   "_id" and "a2_eudract_number", for example created with function
#'   dbGetFieldsIntoDf(c("_id", "a2_eudract_number")).
#' @param prefermemberstate Code of single EU Member State for which records should
#'   returned. If not available, a record for GB or lacking this, any other record
#'   for the trial will be returned. For a list of codes of EU
#'   Member States, please see vector \code{countriesEUCTR}. Alternatively, "3RD"
#'   will lead to return the Third Country record of a trial, if available.
#' @param include3rdcountrytrials A logical value if trials should be retained
#'   that are conducted *exclusively* in third countries, that is outside the European Union.
#'
#' @return A data frame as subset of \code{df} corresponding to the sought
#'   records.
#'
#' @keywords internal
#
dfFindUniqueEuctrRecord <- function(df = NULL, prefermemberstate = "GB", include3rdcountrytrials = TRUE) {

  # check parameters
  if (class(df) != "data.frame") stop("Parameter df is not a data frame.", call. = FALSE)
  #
  if (is.null(df[["_id"]]) || is.null(df["a2_eudract_number"])) stop('Data frame does not include "_id"',
                                                                     ' and "a2_eudract_number" columns.',
                                                                     call. = FALSE)
  #
  if (nrow(df) == 0) stop("Data frame does not contain records (0 rows).", call. = FALSE)
  #
  if (!(prefermemberstate %in% countriesEUCTR)) stop("Value specified for prefermemberstate does not match",
                                                     " one of the recognised codes: ",
                                                     paste(sort(countriesEUCTR), collapse = ", "),
                                                     call. = FALSE)

  # notify it mismatching parameters
  if (prefermemberstate == "3RD" & !include3rdcountrytrials) {
    warning("Preferred EUCTR version set to 3RD country trials, but include3rdcountrytrials was FALSE,",
            " setting to TRUE.", call. = FALSE, noBreaks. = FALSE, immediate. = FALSE)
    include3rdcountrytrials <- TRUE
  }

  # as a first step, handle 3rd country trials e.g. 2010-022945-52-3RD
  # if retained, these trials would count as record for a trial
  if (!include3rdcountrytrials) df <- df[!grepl("-3RD", df[["_id"]]), ]

  # count number of records by eudract number
  tbl <- table(df[["_id"]], df[["a2_eudract_number"]])
  tbl <- as.matrix(tbl)
  # nms has names of all records
  nms <- dimnames(tbl)[[1]]

  # nrs has eudract numbers for which is there more than 1 record
  nrs <- colSums(tbl)
  nrs <- nrs[nrs > 1]
  nrs <- names(nrs)

  # nst is a list of nrs trials of a logical vector along nms
  # that indicates if the indexed record belongs to the trial
  nms2 <- substr(nms, 1, 14)
  nst <- lapply(nrs, function(x) nms2 %in% x)

  # helper function to find the Member State version
  removeMSversions <- function(indexofrecords){
    # given a vector of records (nnnn-nnnnnnn-nn-MS) of a single trial, this
    # returns all those _ids of records that do not correspond to the preferred
    # Member State record, based on the user's choices and defaults.
    # Function uses prefermemberstate, nms from the caller environment
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
  df <- df[!(df [["_id"]] %in% result), ]
  # also eliminate the meta-info record
  df <- df[!(df [["_id"]] == "meta-info"), ]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result)))
    message("Searching multiple country records: Found ", tmp,
            " EUCTR _id that were not the preferred member state record(s) for the trial.")

  # return
  return(df)
  #
}
# end dfFindUniqueEuctrRecord


#' Change type of field based on name of field
#'
#' @param dfi a data frame of columns _id, fieldname
#'
#' @keywords internal
#'
typeField <- function(dfi){

  # check
  if (ncol(dfi) != 2) stop("Expect data frame with two columns, _id and a field.", call. = FALSE)

  # clean up anyway in input
  # - if NA as string, change to empty string
  if (all(class(dfi[, 2]) == "character")) dfi[ dfi[, 2] == "NA", 2] <- ""
  # - if empty string, change to NA
  # if (all(class(dfi[, 2]) == "character")) dfi[ dfi[, 2] == "", 2] <- NA
  # - give Month Year also a Day to work with as.Date
  dfi[, 2] <- sub("^([a-zA-Z]+) ([0-9]{4})$", "\\1 15, \\2", dfi[, 2])

  # for date time conversion
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")

    # selective typing
  tmp <- try({switch(
    EXPR = names(dfi)[2],
    #
    #
    # dates
    #
    # - intern
    "record_last_import" = strptime(dfi[, 2], format = "%Y-%m-%d %H:%M:%S"),
    # - EUCTR
    "n_date_of_ethics_committee_opinion"                                     = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    "n_date_of_competent_authority_decision"                                 = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    "p_date_of_the_global_end_of_the_trial"                                  = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database" = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    "x7_start_date"                                                          = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    "firstreceived_results_date"                                             = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    # - CTGOV
    "start_date"              = as.Date(dfi[, 2], format = "%b %d, %Y"),
    "primary_completion_date" = as.Date(dfi[, 2], format = "%b %d, %Y"),
    "completion_date"         = as.Date(dfi[, 2], format = "%b %d, %Y"),
    "firstreceived_date"      = as.Date(dfi[, 2], format = "%b %d, %Y"),
    "resultsfirst_posted"     = as.Date(dfi[, 2], format = "%b %d, %Y"),
    "lastupdate_posted"       = as.Date(dfi[, 2], format = "%b %d, %Y"),
    "lastchanged_date"        = as.Date(dfi[, 2], format = "%b %d, %Y"),
    #
    #
    # factors
    #
    # - EUCTR Yes / No / Information not present in EudraCT
    "e13_condition_being_studied_is_a_rare_disease" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    "e61_diagnosis"         = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e62_prophylaxis"       = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e63_therapy"           = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e64_safety"            = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e65_efficacy"          = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e66_pharmacokinetic"   = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e67_pharmacodynamic"   = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e68_bioequivalence"    = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e69_dose_response"     = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e610_pharmacogenetic"  = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e611_pharmacogenomic"  = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e612_pharmacoeconomic" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e613_others"           = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    "e71_human_pharmacology_phase_i"         = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e711_first_administration_to_humans"    = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e712_bioequivalence_study"              = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e713_other"                             = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e72_therapeutic_exploratory_phase_ii"   = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e73_therapeutic_confirmatory_phase_iii" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e74_therapeutic_use_phase_iv"           = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    "e81_controlled"      = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e811_randomised"     = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e812_open"           = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e813_single_blind"   = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e814_double_blind"   = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e815_parallel_group" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e816_cross_over"     = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e817_other"          = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    "e83_the_trial_involves_single_site_in_the_member_state_concerned"    = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e84_the_trial_involves_multiple_sites_in_the_member_state_concerned" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e85_the_trial_involves_multiple_member_states"                       = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e861_trial_being_conducted_both_within_and_outside_the_eea"          = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e862_trial_being_conducted_completely_outside_of_the_eea"            = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "e87_trial_has_a_data_monitoring_committee"                           = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    "f11_trial_has_subjects_under_18"            = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f111_in_utero"                              = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f113_newborns_027_days"                     = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f114_infants_and_toddlers_28_days23_months" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f115_children_211years"                     = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f116_adolescents_1217_years"                = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f12_adults_1864_years"                      = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f13_elderly_65_years"                       = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f21_female"                                 = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f22_male"                                   = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f31_healthy_volunteers"                     = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f32_patients"                               = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f33_specific_vulnerable_populations"        = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f331_women_of_childbearing_potential_not_using_contraception_" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f332_women_of_childbearing_potential_using_contraception"      = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f333_pregnant_women"      = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f334_nursing_women"       = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f335_emergency_situation" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "f336_subjects_incapable_of_giving_consent_personally" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    # - CTGOV
    "has_expanded_access"            = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "oversight_info.has_dmc"         = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    "eligibility.healthy_volunteers" = sapply(dfi[, 2], FUN = function(x) switch(x, "Yes" = TRUE, "No" = FALSE, NA)),
    #
    #
    # numbers
    #
    # - EUCTR
    "e824_number_of_treatment_arms_in_the_trial" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e891_in_the_member_state_concerned_years"   = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e891_in_the_member_state_concerned_months"  = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e891_in_the_member_state_concerned_days"    = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e892_in_all_countries_concerned_by_the_trial_years"  = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e892_in_all_countries_concerned_by_the_trial_months" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e892_in_all_countries_concerned_by_the_trial_days"   = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "e841_number_of_sites_anticipated_in_member_state_concerned" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f11_number_of_subjects_for_this_age_range"   = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f1111_number_of_subjects_for_this_age_range" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f1121_number_of_subjects_for_this_age_range" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f1131_number_of_subjects_for_this_age_range" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f1141_number_of_subjects_for_this_age_range" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f1151_number_of_subjects_for_this_age_range" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f1161_number_of_subjects_for_this_age_range" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f121_number_of_subjects_for_this_age_range"  = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f131_number_of_subjects_for_this_age_range"  = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f41_in_the_member_state"          = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f421_in_the_eea"                  = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "f422_in_the_whole_clinical_trial" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    #
    # - CTGOV
    "number_of_arms" = sapply(dfi[, 2], FUN = function(x) as.integer(x = x)),
    "enrollment"     = sapply(dfi[, 2], FUN = function(x) as.integer(x = x))
    #
    # TODO: results-related variables
    #
  )
  },
  silent = TRUE)

  # reset date time
  Sys.setlocale("LC_TIME", lct)

  # prepare output
  if (!("try-error" %in% class(tmp)) &&
      !is.null(unlist(tmp))) {

    # need to construct new data frame,
    # since replacing columns with
    # posixct did not work
    dfn <- names(dfi)
    dfi <- data.frame(dfi[["_id"]],
                      tmp,
                      stringsAsFactors = FALSE)
    names(dfi) <- dfn

  }

  # return
  return(dfi)

} # end typeField


#' Annotate ctrdata function return values
#'
#' @param x object to be annotated
#'
#' @inheritParams ctrMongo
#'
#' @keywords internal
#'
addMetaData <- function(x, uri, collection, password) {

  # add metadata
  attr(x, "ctrdata-using-mongodb-uri")        <- uri
  attr(x, "ctrdata-using-mongodb-collection") <- collection
  attr(x, "ctrdata-created-timestamp")        <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(x, "ctrdata-from-dbqueryhistory")      <- dbQueryHistory(collection = collection, uri = uri,
                                                                password = password,
                                                                verbose = FALSE)

  # return annotated object
  return(x)

} # end addMetaData



#' Function to set proxy
#'
#' @keywords internal
#'
setProxy <- function() {

  # only act if environment
  # variable is not already set
  if (Sys.getenv("https_proxy") == "") {

    # works under windows only
    p <- curl::ie_proxy_info()$Proxy

    if (!is.null(p)) {

      # used by httr and curl
      Sys.setenv(https_proxy = p)

    }
  }
} # end setproxy



#' Convenience function to install a cygwin environment under MS Windows,
#' including perl and php
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
  if (.Platform$OS.type != "windows")
    stop("This function is only for MS Windows operating systems.", call. = FALSE)
  #
  if (!force & dir.exists("c:\\cygwin"))
    stop("cygwin is already installed. To overwrite, use force = TRUE.", call. = FALSE)
  #
  # define installation command
  installcmd <- paste0("--no-admin --quiet-mode --verbose --upgrade-also --root c:/cygwin ",
                       "--site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ ",
                       "--packages perl,php-jsonc,php-simplexml")
  #
  # create directory within R sessions temporary directory
  tmpfile <- paste0(tempdir(), "/cygwin_inst")
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")
  #
  # generate download url
  tmpurl <- ifelse(grepl("x64", utils::win.version()),
                   "setup-x86_64.exe",
                   "setup-x86.exe")
  #
  tmpurl <- paste0("https://cygwin.org/", tmpurl)
  #
  # inform user
  message("Attempting cygwin download using ", tmpurl, " ...")
  #
  # check and set proxy if needed to access internet
  setProxy()
  #
  # download.file uses the proxy configured in the system
  tmpdl <- try({utils::download.file(url = tmpurl,
                                     destfile = dstfile,
                                     quiet = FALSE,
                                     mode = "wb")
    }, silent = TRUE)
  #
  # check
  if (!file.exists(dstfile) ||
      file.size(dstfile) < (5 * 10 ^ 5) ||
      ("try-error" %in% class(tmpdl)))
    stop("Failed, please download manually and install with this command:\n",
         tmpurl, " ", installcmd,
         call. = FALSE)
  #
  if (proxy != "") {
    # manual setting overriding
    proxy <- paste0(" --proxy ", proxy)
    message("Setting cygwin proxy install argument to: ", proxy, ", based on provided parameter.")
  } else {
    # detect proxy
    proxy <- curl::ie_proxy_info()$Proxy
    if (!is.null(proxy)) {
      message("Setting cygwin proxy install argument to: ", proxy, ", based on system settings.")
      proxy <- paste0(" --proxy ", proxy)
    }
 }
  #
  # execute cygwin setup command
  system(paste0(dstfile, " ", installcmd, " --local-package-dir ", tmpfile, " ", proxy))
  #
  # test cygwin installation
  installCygwinWindowsTest(verbose = TRUE)
  #
}
# end installCygwinWindowsDoInstall


#' Convenience function to test for working cygwin installation
#'
#' @param verbose If \code{TRUE}, prints confirmatory message (default \code{FALSE})
#'
#' @return Information if cygwin can be used, \code{TRUE} or \code{FALSE},
#'  or NULL is not under MS Windows
#'
#' @keywords internal
#
installCygwinWindowsTest <- function(verbose = FALSE) {
  #
  if (.Platform$OS.type != "windows") {
    message("Function installCygwinWindowsTest() is only for MS Windows operating systems.")
    return(invisible(NULL))
  }
  #
  tmpcygwin <- try({
    suppressWarnings(
      system("cmd.exe /c c:\\cygwin\\bin\\env",
             intern = TRUE,
             ignore.stderr = TRUE
      ))},
    silent = TRUE)
  #
  if ((class(tmpcygwin) != "try-error") &
      (length(tmpcygwin) > 5)) {
    if (verbose) message("cygwin seems to work correctly.")
    return(invisible(TRUE))
  } else {
    message("cygwin is not available for this package, ctrLoadQueryIntoDb() will not work.\n",
            "Consider calling ctrdata::installCygwinWindowsDoInstall() from within R.")
    return(invisible(FALSE))
  }
}
# end installCygwinWindowsTest


#' Check availability of binaries installed locally
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
  if (is.null(commandtest)) stop("Empty argument: commandtest", call. = FALSE)
  #
  if (.Platform$OS.type == "windows") commandtest <-
      paste0("cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c ", shQuote(commandtest))
  #
  if (debug) print(commandtest)
  #
  commandresult <- try(
    system(commandtest,
           intern = TRUE,
           ignore.stdout = TRUE,
           ignore.stderr = TRUE),
    silent = TRUE
  )
  #
  commandreturn <- ifelse(class(commandresult) == "try-error" ||
                           grepl("error", tolower(paste(commandresult, collapse = " "))), FALSE, TRUE)
  #
  if (!commandreturn) warning(commandtest, " not found.", call. = FALSE, immediate. = FALSE)
  #
  if (debug) print(commandresult)
  #
  return(commandreturn)
  #
}
# end installFindBinary
