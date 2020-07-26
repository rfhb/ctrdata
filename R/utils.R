### ctrdata package
### utility functions

## variable definitions
#
# EUCTR definitions
countriesEUCTR <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB", "IS", "LI",
  "NO", "3RD")


#' Check and prepare nodbi connection object for ctrdata
#'
#' @param con A \link[nodbi]{src} connection object, as obtained with
#'  nodbi::\link[nodbi]{src_mongo}() or nodbi::\link[nodbi]{src_sqlite}()
#'
#' @keywords internal
#'
#' @importFrom nodbi src_sqlite
#' @importFrom utils capture.output
#'
#' @return Connection object as list, with collection
#'  element under root
#'
ctrDb <- function(
  con = nodbi::src_sqlite(
    collection = "ctrdata_auto_generated")) {

  ## sqlite
  if ("src_sqlite" %in% class(con)) {

    if (is.null(con$collection)) {
      stop("In src_sqlite(), a parameter 'collection' needs to specify ",
           "the name of a table, such as src_sqlite(collection = 'test'), ",
           "for package ctrdata to work with other nosql databases.",
           call. = FALSE)
    }

    # check if disconnected
    if (!RSQLite::dbIsValid(con$con)) {
      # try to reconnect
      warning("Database connection was closed, trying to reopen...",
              call. = FALSE, immediate. = TRUE)
      con <- nodbi::src_sqlite(dbname = con$dbname,
                               collection = con$collection)
    }

    # add database as element under root
    con <- c(con,
             "db" = con$dbname,
             "ctrDb" = TRUE)

    # print warning from nodbi::src_sqlite()
    if (grepl(":memory:", con$dbname)) {
      warning("Database not persisting,\ncopy to persistant database like ",
              "this:\n\nRSQLite::sqliteCopyDatabase(",
              "\n  from = <your in-memory-database-object>$con,",
              "\n  to = RSQLite::dbConnect(RSQLite::SQLite(),",
              "\n                          dbname = 'local_file.db'))\n",
              call. = FALSE,
              noBreaks. = FALSE,
              immediate. = TRUE)
    }

    ## return
    return(structure(con,
                     class = c("src_sqlite", "docdb_src")))
  }

  ## mongo
  if ("src_mongo" %in% class(con)) {

    # rights may be insufficient to call info(),
    # hence this workaround that should always
    # work and be stable to retrieve name of
    # collection in the mongo connection
    coll <- utils::capture.output(con$con)[1]
    coll <- sub("^.*'(.*)'.*$", "\\1", coll)

    # add collection as element under root
    con <- c(con,
             "collection" = coll,
             "ctrDb" = TRUE)

    ## return
    return(structure(con,
                     class = c("src_mongo", "docdb_src")))
  }

  ## unprepared for other nodbi adapters so far
  stop("Please specify in parameter 'con' a database connection. ",
       "crdata supports so far only src_mongo() and src_sqlite().")

}


#' Open advanced search pages of register(s) or execute search in browser
#'
#' @param input Show results of search for \code{queryterm} in
#'   browser. To open the browser with a previous search, (register or)
#'   queryterm can be the output of \link{ctrGetQueryUrlFromBrowser} or can
#'   be one row from \link{dbQueryHistory}.
#'
#' @param register Register(s) to open. Either "EUCTR" or "CTGOV" or a vector of
#'   both. Default is to open both registers' advanced search pages. To open the
#'   browser with a previous search, the output of ctrGetQueryUrlFromBrowser()
#'   or one row from dbQueryHistory() can be used.
#'
#' @param copyright (Optional) If set to \code{TRUE}, opens copyright pages of
#'   register(s).
#'
#' @param ... Any additional parameter to use with browseURL, which is called by
#'   this function.
#'
#' @export
#'
#' @return Is always true, invisibly.
#'
#' @examples
#' \dontrun{
#' ctrOpenSearchPagesInBrowser(
#'  "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer")
#'
#' ctrOpenSearchPagesInBrowser(
#'  ctrGetQueryUrlFromBrowser())
#'
#' # open the last query that was
#' # loaded into the database
#' db <- nodbi::src_sqlite(
#'   collection = "previously_created"
#' )
#' ctrOpenSearchPagesInBrowser(
#'   dbQueryHistory(con = db))
#' }
#'
ctrOpenSearchPagesInBrowser <- function(
  input = "",
  register = "",
  copyright = FALSE,
  ...) {

    # check combination of arguments to select action
  #
  if (class(input) == "character" && is.atomic(input) && input == "") {
    #
    # if no register is specified, open both
    if (all(register == "", na.rm = TRUE)) register <- c("EUCTR", "CTGOV")
    #
    # open empty search pages
    if ("EUCTR" %in% register)
      try({
        utils::browseURL(
          "https://www.clinicaltrialsregister.eu/ctr-search/search",
          ...)}, silent = TRUE)
    #
    if ("CTGOV" %in% register)
      try({
        utils::browseURL(
          "https://clinicaltrials.gov/ct2/search/advanced",
          ...)}, silent = TRUE)
    #
    # if requested also show copyright pages
    if (copyright) {
      #
      if ("EUCTR" %in% register)
        try({
          utils::browseURL(
            "https://www.clinicaltrialsregister.eu/disclaimer.html",
            ...)}, silent = TRUE)
      #
      if ("CTGOV" %in% register)
        try({
          utils::browseURL(
            "https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use",
            ...)}, silent = TRUE)
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
      input <- ctrGetQueryUrlFromBrowser(url = input)
      #
    }
    #
    # - data frame as returned from ctrQueryHistoryInDb()
    #   and ctrGetQueryUrlFromBrowser()
    if (is.data.frame(input) &&
        all(substr(names(input), 1, 6) == "query-")) {
      #
      nr <- nrow(input)
      #
      if (nr > 1) warning("Using last row of input.",
                          call. = FALSE, immediate. = TRUE)
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
    if (exists("queryterm") &&
        queryterm != "" &&
        register != "") {
      #
      message("Opening browser for search: \n\n", queryterm,
              "\n\nin register: ", register)
      #
      # sanity correction for naked terms
      if (register == "EUCTR") {
        queryterm <-
          sub("(^|&|[&]?\\w+=\\w+&)([ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
              "\\1query=\\2\\3",
              queryterm)
      }
      if (register == "CTGOV") {
        queryterm <-
          sub("(^|&|[&]?\\w+=\\w+&)(\\w+|[NCT0-9-]+)($|&\\w+=\\w+)",
              "\\1term=\\2\\3",
              queryterm)
      }
      #
      # protect against os where this does not work
      try({utils::browseURL(url = paste0(
        #
        switch(as.character(register),
               "CTGOV" = ifelse(
                 grepl("^xprt=", queryterm),
                 "https://clinicaltrials.gov/ct2/results/refine?show_xprt=Y&",
                 "https://clinicaltrials.gov/ct2/results?"),
               "EUCTR" =
                 "https://www.clinicaltrialsregister.eu/ctr-search/search?"),
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
#' @param url URL such as from the browser address bar.
#' If not specified, clipboard contents will be checked for
#' a suitable URL. Can also contain a query term such as from
#' \link{dbQueryHistory}()["query-term"]
#'
#' @param register Optional name of register (i.e., "EUCTR" or
#' "CTGOV") in case url is a query term
#'
#' @return A string of query parameters that can be used to retrieve data
#' from the register.
#'
#' @export
#'
#' @return A data frame with a query term and the register name that can
#' directly be used in \link{ctrLoadQueryIntoDb} and in
#' \link{ctrOpenSearchPagesInBrowser}
#'
#' @examples
#'
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "my_collection"
#' )
#' ctrLoadQueryIntoDb(
#'   ctrGetQueryUrlFromBrowser(),
#'   con = db
#' )
#' }
#'
#' @importFrom clipr read_clip
#'
ctrGetQueryUrlFromBrowser <- function(
  url = "",
  register = "") {
  #
  # check parameters expectations
  if (!is.atomic(url) || !is.atomic(register) ||
      is.null(url) || is.null(register) ||
      is.na(url) || is.na(register) ||
      !inherits(url, "character") || !inherits(register, "character") ||
      length(url) != 1L || length(register) != 1L) {
    stop("ctrGetQueryUrlFromBrowser(): 'url' and / or 'register' ",
         "is not a single character string.",
         call. = FALSE)
  }
  #
  # if no parameter specified,
  # check clipboard contents
  if (nchar(url) == 0L) {
    url <- suppressWarnings(
      clipr::read_clip(
        allow_non_interactive = TRUE)
    )
    if (is.null(url) || (length(url) != 1L) || (nchar(url) == 0L)) {
      stop("ctrGetQueryUrlFromBrowser(): no clinical trial register ",
           "search URL found in parameter 'url' or in clipboard.",
           call. = FALSE)
    }
    message("* Using clipboard content as register query URL: ", url)
  }
  #
  # EUCTR
  if (grepl("https://www.clinicaltrialsregister.eu/ctr-search/", url) ||
      (!grepl("https://", url) && register == "EUCTR")) {
    #
    # check
    if (grepl("https://", url) &
        !grepl("www.clinicaltrialsregister.eu/", url)) {
      warning("ctrGetQueryUrlFromBrowser(): 'url' inconsistent with EUCTR.",
              call. = FALSE, immediate. = TRUE)
      return(invisible(NULL))
    }
    #
    queryterm <-
      sub("https://www.clinicaltrialsregister.eu/ctr-search/search[?](.*)",
          "\\1", url)
    #
    queryterm <-
      sub("https://www.clinicaltrialsregister.eu/ctr-search/trial/([-0-9]+)/.*",
          "\\1", queryterm)
    #
    # remove any intrapage anchor, e.g. #tableTop
    queryterm <- sub("#.+$", "", queryterm)
    #
    # sanity correction for naked terms
    # test cases:
    # queryterm = c(
    #   "cancer&age=adult",                      # add query=
    #   "cancer",                                # add query=
    #   "cancer+AND breast&age=adult&phase=0",   # add query=
    #   "cancer&age=adult&phase=0",              # add query=
    #   "cancer&age=adult&phase=1&results=true", # add query=
    #   "&age=adult&phase=1&abc=xyz&cancer&results=true", # insert query=
    #   "age=adult&cancer",                      # insert query=
    #   "2010-024264-18",                        # add query=
    #   "NCT1234567890",                         # add query=
    #   "teratoid&country=dk",                   # add query=
    #   "term=cancer&age=adult",                 # keep
    #   "age=adult&term=cancer")                 # keep
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)([ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
      "\\1query=\\2\\3",
      queryterm)
    #
    # check if url was for results of single trial
    if (grepl(".*/results$", url)) {
      queryterm <- paste0(queryterm, "&resultsstatus=trials-with-results")
    }
    #
    message("* Found search query from EUCTR: ", queryterm)
    #
    df <- data.frame(cbind(queryterm, "EUCTR"), stringsAsFactors = FALSE)
    names(df) <- c("query-term", "query-register")
    #
    return(df)
  }
  #
  # CTGOV, e.g.
  # https://clinicaltrials.gov/ct2/results?term=2010-024264-18&Search=Search
  if (grepl("https://clinicaltrials.gov/ct2/results", url) ||
      (!grepl("https://", url) && register == "CTGOV")) {
    #
    # check
    if (grepl("https://", url) &
        !grepl("clinicaltrials.gov/", url)) {
      warning("ctrGetQueryUrlFromBrowser(): 'url' inconsistent with CTGOV.",
              call. = FALSE, immediate. = TRUE)
      return(invisible(NULL))

    }
    #
    queryterm <-
      sub("https://clinicaltrials.gov/ct2/results[?](.*)",
          "\\1", url)
    #
    queryterm <-
      sub("(.*)&Search[a-zA-Z]*=(Search|Find)[a-zA-Z+]*",
          "\\1", queryterm)
    #
    queryterm <- gsub("[a-z_0-9]+=&", "", queryterm)
    queryterm <- sub("&[a-z_0-9]+=$", "", queryterm)
    #
    message("* Found search query from CTGOV: ", queryterm)
    #
    df <- data.frame(cbind(queryterm, "CTGOV"),
                     stringsAsFactors = FALSE)
    names(df) <- c("query-term", "query-register")
    #
    return(df)
  }
  #
  warning("ctrGetQueryUrlFromBrowser(): no clinical trial register ",
          "search URL found in parameter 'url' or in clipboard.",
          call. = FALSE, immediate. = TRUE)
  #
  return(invisible(NULL))
}
# end ctrGetQueryUrlFromBrowser



#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name, a trade or product name, or a company code(s).
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
#' ctrFindActiveSubstanceSynonyms(
#'   activesubstance = "imatinib"
#' )
#' }
#'
ctrFindActiveSubstanceSynonyms <- function(activesubstance = ""){

  # check parameters
  if ( (length(activesubstance) != 1) ||
       !is.character(activesubstance) ||
       (nchar(activesubstance) == 0) ) {
    stop("ctrFindActiveSubstanceSynonyms(): ",
         "activesubstance should be a single string.",
         call. = FALSE)
  }

  # initialise output variable
  as <- activesubstance

  # check and set proxy if needed to access internet
  setProxy()

  # getting synonyms
  ctgovdfirstpageurl <-
    paste0("https://clinicaltrials.gov/ct2/results/details?term=",
           activesubstance)

  tmp <- xml2::read_html(x = utils::URLencode(ctgovdfirstpageurl))
  tmp <- rvest::html_node(tmp, xpath =
                            '//*[@id="searchdetail"]//table[1]')
  tmp <- rvest::html_table(tmp, fill = TRUE)
  asx <- tmp[, 1]
  asx <- asx[!grepl(
    paste0("(more|synonyms|terms|", as, "|",
           paste0(unlist(strsplit(as, " ")), collapse = "|"),
           ")"), asx, ignore.case = TRUE)]

  # prepare and return output
  as <- c(as, asx)
  as <- unique(as)
  return(as)
}
# end ctrFindActiveSubstanceSynonyms



#' Show the history of queries that were loaded into a database
#'
#' @inheritParams ctrDb
#'
#' @return A data frame with columns: query-timestamp, query-egister,
#'  query-records (note: this is the number of records loaded when last
#'  executing \link{ctrLoadQueryIntoDb}, not the total record number) and
#'  query-term, and with one row for each \link{ctrLoadQueryIntoDb}
#'  loading trial records in this collection.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "my_collection"
#' )
#' dbQueryHistory(
#'   con = db
#' )
#' }
#'
dbQueryHistory <- function(con,
                           verbose = FALSE) {

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # debug
  if (verbose) message("Running dbQueryHistory ...")

  tmp <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{"_id": {"$eq": "meta-info"}}',
    fields = '{"queries": 1}')

  # access array of meta-info
  tmp <- tmp[["queries"]][[1]]

  # Check if meeting expectations
  if (is.null(tmp) ||
      nrow(tmp) == 0L) {
    #
    message("No history found in expected format.")
    #
    # return (class data.frame is expected)
    return(invisible(data.frame(NULL)))
    #
  }
  # else {

  # Inform user
  if (verbose) {

    message("Number of queries in history of \"",
            con$collection, "\": ", nrow(tmp))
    # }

    # total number of records in collection to inform user
    countall <- nodbi::docdb_query(
      src = con,
      key = con$collection,
      query =  '{"_id": {"$ne": "meta-info"}}',
      fields = '{"_id": 1}')[["_id"]]

    # if (verbose)
    message("Number of records in collection \"",
            con$collection, "\": ", length(countall))
  }

  # return
  return(tmp)

}
# end ctrQueryHistoryInDb


#' Find names of fields in the database collection
#'
#' Given part of the name of a field of interest to the user, this
#' function returns the full field names as found in the database.
#'
#' For fields in EUCTR (protocol- and results-related information),
#' see also the register's documentation at
#' \url{https://eudract.ema.europa.eu/}.
#'
#' For fields in CTGOV (protocol-related information), see also
#' the register's definitions at
#' \url{https://prsinfo.clinicaltrials.gov/definitions.html}.
#'
#' Note: Generating a list of fields with this function may take
#' some time, and may involve running a mapreduce function is
#' run on the server. If the user is not not authorised to run
#' such a function on the (local or remote) server,
#' random documents are sampled to generate a list of fields.
#'
#' @param namepart A plain string (can include a regular expression,
#' including Perl-style) to be searched for among all field names
#' (keys) in the database.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#'
#' @inheritParams ctrDb
#'
#' @return Vector of names of found field(s)
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "my_collection"
#' )
#' dbFindFields(
#'   nampepart = "date",
#'   con = db
#' )
#' }
#'
dbFindFields <- function(namepart = "",
                         con,
                         verbose = FALSE) {

  ## sanity checks
  if (!is.atomic(namepart)) stop("'namepart' should be atomic.", call. = FALSE)
  if (length(namepart) > 1) stop("'namepart' should have one element.", call. = FALSE)
  if (namepart == "")       stop("Empty 'namepart' parameter.", call. = FALSE)

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  ## this is the only function in package ctrdata
  ## which uses backend- specific methods, since
  ## no canonical way was found yet to retrieve
  ## field / key names.

  ## check if cache for list of keys in collection exists,
  # otherwise create new environment as session cache
  if (!exists(".dbffenv")) {
    .dbffenv <- new.env(parent = emptyenv())
  }

  ## check if cache environment has entry for the database
  if (exists(x = paste0(con$db, "/", con$collection),
             envir = .dbffenv)) {

    # if true, get keys list from cache
    keyslist <- get(x = paste0(con$db, "/", con$collection),
                    envir = .dbffenv)

    # informing user
    message("Using cache of fields.")

  } else {

    ## get keys list from database
    ## warn if no method yet for backend
    if (!any(c("src_mongo",
               "src_sqlite") %in%
             class(con))) {
      stop("Function dbFindFields() cannot yet handle nodbi ",
           "database backend ", class(con)[1], call. = FALSE)
      ## TODO extended function to additional backends
    }

    # inform user
    message("Finding fields in database (may take some time)")

    ## - method for mongodb
    if ("src_mongo" %in% class(con)) {

      # try mapreduce to get all keys
      keyslist <- try({
        con$con$mapreduce(
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
        )[["_id"]]},
        silent = TRUE)

      # if mapreduce does not work or is not permitted, revert to guessing
      if ("try-error" %in% class(keyslist)) {

        warning("Mongo server returned: ", as.character(keyslist),
                "Using alternative method (extracting keys from ",
                "sample documents, may be incomplete).",
                call. = FALSE, immediate. = TRUE)

        # get 2 random documents, one for each register EUCTR and CTGOV,
        # if in collection, and retrieve keys from documents
        keyslist <- c(
          "", # avoid empty vector
          names(con$con$find(
            query = '{"_id": { "$regex": "^NCT[0-9]{8}", "$options": ""} }',
            limit = 1L)),
          names(con$con$find(
            query = '{"_id": { "$regex": "^[0-9]{4}-[0-9]{6}", "$options": ""} }',
            limit = 1L)))

      } # end if error with mapreduce
    } # end if src_mongo

    ## - method for sqlite
    if ("src_sqlite" %in% class(con)) {

      # uses special function parameter for
      # src_sqlite query method: listfields
      keyslist <- c("", # avoid empty vector
                    nodbi::docdb_query(
                      src = con,
                      key = con$collection,
                      query = '',
                      listfields = TRUE))

    }

    ## store keyslist to environment (cache)
    if (length(keyslist) > 1) {
      assign(x = paste0(con$db, "/", con$collection),
             value = keyslist,
             envir = .dbffenv)
      message("Field names cached for this session.")
    }

  } # end get cached list or generate new list

  ## inform user of unexpected situation
  if ((length(keyslist) == 0) || all(keyslist == "")) {
    warning("No keys could be extracted, please check database ",
            "and contents: ", con$db, "/", con$collection, call. = FALSE)
  }

  ## now do the actual search and find for key name parts
  fields <- keyslist[grepl(pattern = namepart, x = keyslist,
                           ignore.case = TRUE, perl = TRUE)]

  # return the match(es)
  return(fields)

}  # end dbFindFields


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
#' @importFrom nodbi docdb_query
#'
#' @inheritParams ctrDb
#'
#' @return A vector with strings of keys ("_id" in the database) that
#'   represent non-duplicate trials.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "my_collection"
#' )
#' dbFindIdsUniqueTrials(
#'   con = db
#' )
#' }
#'
dbFindIdsUniqueTrials <- function(
  preferregister = "EUCTR",
  prefermemberstate = "GB",
  include3rdcountrytrials = TRUE,
  con,
  verbose = TRUE) {

  # parameter checks
  if (!grepl(preferregister, "CTGOVEUCTR")) {
    stop("Register not known: ", preferregister, call. = FALSE)
  }

  # objective: create a list of mongo database record identifiers (_id)
  # that represent unique records of clinical trials, based on user's
  # preferences for selecting the preferred from any multiple records

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # total number of records in collection to inform user
  countall <- nrow(nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{"_id":{"$ne":"meta-info"}}',
    fields = '{"_id": 1}'))

  # inform user
  if (verbose) {
    message("* Total of ", countall, " records in collection.")
  }

  # 1. get euctr records (note only records with at least on
  # value for at least one variable are retrieved here)
  listofEUCTRids <- try(suppressMessages(suppressWarnings(
    dbGetFieldsIntoDf(fields = c(
      "a2_eudract_number",
      "a41_sponsors_protocol_code_number",
      "a51_isrctn_international_standard_randomised_controlled_trial_number",
      "a52_us_nct_clinicaltrialsgov_registry_number",
      "a53_who_universal_trial_reference_number_utrn"), # a53_ not yet used
      con = con,
      verbose = FALSE,
      stopifnodata = FALSE) # if only ctgov records, an error is triggered
  )),
  silent = TRUE
  )
  attribsids <- attributes(listofEUCTRids)
  if (class(listofEUCTRids) == "try-error") listofEUCTRids <- NULL
  if (all(is.na(listofEUCTRids[, -1])))     listofEUCTRids <- NULL
  if (is.null(listofEUCTRids)) message("No EUCTR records found.")

  # inform user
  message("Searching for duplicates, found ")

  # 2. find unique, preferred country version of euctr
  if (!is.null(listofEUCTRids)) {
    listofEUCTRids <- dfFindUniqueEuctrRecord(
      df = listofEUCTRids,
      prefermemberstate = prefermemberstate,
      include3rdcountrytrials = include3rdcountrytrials)
  }

  # 3. get ctgov records
  listofCTGOVids <- try(suppressMessages(suppressWarnings(
    dbGetFieldsIntoDf(fields = c(
      "id_info.org_study_id",
      "id_info.secondary_id",
      "id_info.nct_alias"),
      con = con,
      verbose = FALSE,
      stopifnodata = FALSE)
  )),
  silent = TRUE
  )
  # keep only ctgov
  if (nrow(listofCTGOVids)) {
    listofCTGOVids <- listofCTGOVids[
      grepl("^NCT[0-9]{8}", listofCTGOVids[["_id"]]), ]
  }
  #
  if (!nrow(listofCTGOVids)) {
    listofCTGOVids <- NULL
    # inform user
    message("No CTGOV records found.")
  }

  # 4. retain unique ctgov records
  if (!is.null(listofCTGOVids)) {
    #
    # make id_info sub-fields into one field
    listofCTGOVids[["id_info"]] <- sapply(
      seq_len(nrow(listofCTGOVids)),
      function(i)
        unique(
          as.character(
            na.omit(
              unlist(
                listofCTGOVids[i, -match("_id", names(listofCTGOVids))])))),
      simplify = FALSE)
    # do not simplify, so that it returns df for any 1-row listofCTGOVids
    #
    # retain only relevant fields
    listofCTGOVids <- listofCTGOVids[, c("_id", "id_info")]

  }

  # 5. find records (_id's) that are in both in euctr and ctgov
  if (!is.null(listofEUCTRids) & !is.null(listofCTGOVids)) {
    #
    # 6. select records from preferred register
    if (preferregister == "EUCTR") {
      #
      # strategy: retain all listofEUCTRids;
      # identify in, and remove from, listofCTGOVids the
      # dupes = listofCTGOVids %in% listofEUCTRids
      # > c(5,4,3) %in% c(1,5)
      # [1]  TRUE FALSE FALSE
      #
      # b2 - ctgov in euctr (_id corresponds to index 1)
      dupes.b2 <- listofCTGOVids[["_id"]] %in%
        listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]
      #
      if (verbose) message(" - ", sum(dupes.b2),
                           " CTGOV _id (nct) in EUCTR a52_us_nct_...")
      #
      # a2 - ctgov in euctr a2_...
      dupes.a2 <- sapply(
        listofCTGOVids[["id_info"]], # e.g. "EUDRACT-2004-000242-20"
        function(x) any(sub(".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*", "\\1", x) %in%
                          listofEUCTRids[["a2_eudract_number"]]))
      #
      if (verbose) {
        message(
          " - ", sum(dupes.a2),
          " CTGOV secondary_id / nct_alias / org_study_id in EUCTR a2_eudract_number")
      }
      #
      # c.2 - ctgov in euctr a52_... (id_info corresponds to index 2)
      # TODO deleteme
      dupes.c2 <- sapply(
        listofCTGOVids[["id_info"]],
        function(x) any(
          x %in%
            listofEUCTRids[["a52_us_nct_clinicaltrialsgov_registry_number"]]))
      #
      if (verbose) {
        message(
          " - ", sum(dupes.c2),
          " CTGOV secondary_id / nct_alias / org_study_id in",
          " EUCTR a52_us_nct_...")}
      #
      # d.2 - ctgov in euctr a51_... (id_info corresponds to index 2)
      dupes.d2 <- sapply(
        listofCTGOVids[["id_info"]],
        function(x) any(
          x %in%
            listofEUCTRids[[
              "a51_isrctn_international_standard_randomised_controlled_trial_number"]]))
      #
      if (verbose) message(" - ", sum(dupes.d2),
                           " CTGOV secondary_id / nct_alias / org_study_id in",
                           " EUCTR a51_isrctn_...")
      #
      # e.2 - ctgov in euctr a41_... (id_info corresponds to index 2)
      dupes.e2 <- sapply(
        listofCTGOVids[["id_info"]],
        function(x) any(
          x %in%
            listofEUCTRids[["a41_sponsors_protocol_code_number"]]))
      #
      if (verbose) {
        message(" - ", sum(dupes.d2),
                " CTGOV secondary_id / nct_alias / org_study_id in",
                " EUCTR a41_sponsors_protocol_...")}
      #
      # finalise results set
      listofEUCTRids <- listofEUCTRids[["_id"]]
      listofCTGOVids <- listofCTGOVids[[
        "_id"]] [ !dupes.a2 & !dupes.b2 & !dupes.c2 & !dupes.d2 & !dupes.e2 ]
      #
      message(
        "Concatenating ",
        length(listofEUCTRids), " records from EUCTR and ",
        length(listofCTGOVids), " from CTGOV:")
      #
      retids <- c(listofEUCTRids, listofCTGOVids)
      #
    }
    #
    if (preferregister == "CTGOV") {
      #
      # a.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.a1 <- listofEUCTRids[["a2_eudract_number"]] %in% sub(
        ".*([0-9]{4}-[0-9]{6}-[0-9]{2}).*", # e.g. "EUDRACT-2004-000242-20"
        "\\1", unlist(listofCTGOVids[["id_info"]]))
      #
      if (verbose) {
        message(" - ", sum(dupes.a1),
                " EUCTR _id in CTGOV secondary_id / nct_alias / org_study_id")
      }
      #
      # b.1 - euctr in ctgov (_id corresponds to index 1)
      dupes.b1 <- listofEUCTRids[[
        "a52_us_nct_clinicaltrialsgov_registry_number"]] %in% listofCTGOVids[["_id"]]
      #
      if (verbose) {
        message(" - ", sum(dupes.b1),
                           " EUCTR a52_us_nct_... in CTGOV _id (nct)")
      }
      #
      # c.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.c1 <- listofEUCTRids[[
        "a52_us_nct_clinicaltrialsgov_registry_number"]] %in%
        unlist(listofCTGOVids[["id_info"]])
      #
      if (verbose) {
        message(
          " - ", sum(dupes.c1),
          " EUCTR a52_us_nct_... in",
          " CTOGV secondary_id / nct_alias / org_study_id")
      }
      #
      # d.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.d1 <- listofEUCTRids[[
        "a51_isrctn_international_standard_randomised_controlled_trial_number"]] %in%
        unlist(listofCTGOVids[["id_info"]])
      #
      if (verbose) {
        message(
          " - ", sum(dupes.d1),
          " EUCTR a51_isrctn_...",
          " in CTOGV secondary_id / nct_alias / org_study_id")
      }
      #
      # e.1 - euctr in ctgov (id_info corresponds to index 2)
      dupes.e1 <- listofEUCTRids[["a41_sponsors_protocol_code_number"]] %in%
        unlist(listofCTGOVids[["id_info"]])
      #
      if (verbose) {
        message(
          " - ", sum(dupes.d1),
          " EUCTR a41_sponsors_protocol_...",
          " in CTOGV secondary_id / nct_alias / org_study_id")
      }
      #
      # finalise results set
      listofCTGOVids <- listofCTGOVids[["_id"]]
      listofEUCTRids <- listofEUCTRids[[
        "_id"]] [ !dupes.a1 & !dupes.b1 & !dupes.c1 & !dupes.d1  & !dupes.e1 ]
      #
      message(
        "Concatenating ",
        length(listofCTGOVids), " records from CTGOV and ",
        length(listofEUCTRids), " from EUCTR:")
      #
      retids <- c(listofCTGOVids, listofEUCTRids)
      #
    }
  } else {
    #
    # fallback
    retids <- c(listofEUCTRids[["_id"]], listofCTGOVids[["_id"]])
    #
  }

  # prepare output
  attributes(retids) <- attribsids[grepl("^ctrdata-", names(attribsids))]

  # avoid returning list() if none found
  if (length(retids) == 0) {
    retids <- character()
  }

  # inform user
  message(
    "= Returning keys (_id) of ", length(retids),
    " out of total ", countall,
    " records in collection \"", con$collection, "\".")

  # return
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
#' For more sophisticated data retrieval from the database, see vignette
#' examples and other packages to query mongodb such as mongolite.
#'
#' @param fields Vector of one or more strings, with names of the sought fields.
#'    See function \link{dbFindFields} for how to find names of fields.
#'
#' @param stopifnodata Stops with an error (\code{TRUE}, default) or with
#' a warning (\code{FALSE}) if the sought field is empty in all,
#' or not available in any of the records in the database collection.
#'
#' @param verbose Printing additional information if set to \code{TRUE};
#' default is \code{FALSE}.
#'
#' @inheritParams ctrDb
#'
#' @return A data frame with columns corresponding to the sought fields.
#' Note: a column for the record _id will always be included.
#' The maximum number of rows of the returned data frame is equal to,
#' or less than the number of records in the data base.
#'
#' @importFrom nodbi docdb_query
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "my_collection"
#' )
#'
#' # access fields that are nested within another field
#' # and can have multiple values with the other field
#' dbGetFieldsIntoDf(
#'   "b1_sponsor.b31_and_b32_status_of_the_sponsor",
#'   con = db
#' )[1,]
#' #                 _id b1_sponsor.b31_and_b32_status_of_the_sponsor
#' # 1 2004-000015-25-GB                  Non-commercial / Commercial
#'
#' # access fields that include a list of values
#' # which are printed as comma separated values
#' dbGetFieldsIntoDf(
#'   "keyword",
#'   con = db
#' )[1,]
#'
#' #           _id                                 keyword
#' # 1 NCT00129259 T1D, type 1 diabetes, juvenile diabetes
#' #
#' str(.Last.value)
#' 'data.frame':	1 obs. of  2 variables:
#' $ _id    : chr "NCT00129259"
#' $ keyword:List of 1
#' ..$ : chr  "T1D" "type 1 diabetes" "juvenile diabetes" ...
#'
#' }
#'
dbGetFieldsIntoDf <- function(fields = "",
                              con, verbose = FALSE,
                              stopifnodata = TRUE) {

  # check parameters
  if (!is.vector(fields) |
      class(fields) != "character") {
    stop("Input should be a vector of strings of field names.", call. = FALSE)
  }

  # remove NA, NULL if included in fields
  fields <- fields[!is.null(fields) & !is.na(fields)]

  # remove _id if included in fields
  fields <- fields["_id" != fields]

  # check if valid fields
  if (any(fields == "") | (length(fields) == 0)) {
    stop("'fields' contains empty elements; ",
         "please provide a vector of strings of field names. ",
         "Function dbFindFields() can be used to find field names. ",
         call. = FALSE)
  }

  ## check database connection
  if (is.null(con$ctrDb)) {
    con <- ctrDb(con = con)
  }

  # provide list of ids
  # idsall <- mongo$find(
  #  query = '{"_id":{"$ne":"meta-info"}}', fields = '{"_id" : 1}')
  idsall <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{"_id":{"$ne":"meta-info"}}',
    fields = '{"_id" : 1}')[["_id"]]

  # initialise output
  result <- NULL

  # iterate over fields so that we can use a custom function to merge results,
  # given that mongodb clients have different approaches and complex returns
  for (item in fields) {
    #
    query <- paste0('{"_id": {"$ne": "meta-info"}}')
    if (verbose) {message("DEBUG: field: ", item)}
    #
    tmp <- try({
      #
      dfi <- nodbi::docdb_query(
        src = con,
        key = con$collection,
        query = query,
        fields = paste0('{"_id": 1, "', item, '": 1}'))
      #
      # some backends return NA if query matches,
      # other only non-NA values when query matches
      # dfi <- na.omit(dfi)
      dfi <- dfi[!is.na(dfi["_id"]) &
                 !sapply(seq_len(nrow(dfi)),
                         function(r) all(is.na(dfi[r, -1]))), ]
      #
      # ensure intended column order
      if (names(dfi)[1] != "_id") {
        dfi <- dfi[, 2:1]
      }
      #
      # simplify if robust
      #
      # - is dfi[,2] is NULL, remove from dfi data frame
      dfi <- dfi[ !sapply(dfi[, 2], length) == 0, ]
      #
      # - if each [,2] is a list with one element, concatenate
      if ((ncol(dfi) == 2) &&
          all(sapply(dfi[, 2],
                     function(x)
                       is.data.frame(x) && ncol(x) == 1))) {
        # concatenate
        dfi[, 2] <- sapply(sapply(dfi[, 2], "[", 1),
                           function(x) paste0(x, collapse = " / "))
        # inform user
        message("Note: requested field ", item, " has subitems ",
                paste0(names(dfi)[-1], collapse = ", "),
                ", collapsed using ' / '")
        # remove extraneous columns
        dfi <- dfi[, 1:2]
      }
      #
      # - if dfi[, 2:ncol(dfi)] is from the same field e.g.
      #   required_header.{download_date,link_text,url}, concatenate
      if ((ncol(dfi) > 2) &&
          all(grepl(paste0(item, "[.].+$"),
                    names(dfi)[-1]))) {
        # concatenate
        dfi[, 2] <- unlist(
          apply(
            X = dfi[, 2:ncol(dfi)],
            MARGIN = 1,
            FUN = function(r)
              paste0(na.omit(unlist(r)), collapse = " / ")))
        # inform user
        message("Note: requested field ", item, " has subitems ",
                paste0(names(dfi)[-1], collapse = ", "),
                ", collapsed using ' / '")
        # remove extraneous columns
        dfi <- dfi[, 1:2]
      }
      #
      # # - if each [,2] is a list of one dataframe with one or more rows,
      # #   turn data frame into list
      # if (all(sapply(dfi[, 2],
      #                function(x) is.data.frame(x)))) {
      #
      #   dfi[, 2] <- tt <- lapply(
      #     seq_len(nrow(dfi)),
      #     function(i) {
      #       tmp <- dfi[i, 2][[1]] # get dataframe
      #       lapply(seq_len(nrow(tmp)),
      #              function(ii) unclass(tmp[ii, ]))})
      # }
      # #
      if (verbose) {
        message("DEBUG: field ", item, " has length ", nrow(dfi))}
      #
    },
    silent = TRUE)
    #
    if (inherits(tmp, "try-error") ||
        !nrow(dfi)) {

      # try-error occured or no data retrieved
      if (stopifnodata) {
        stop("For field '", item, "' no data could be extracted from the ",
             "database collection. Use dbGetFieldsIntoDf(stopifnodata = ",
             "FALSE) to continue extracting other fields. ",
             call. = FALSE)
      } else {
        warning("For field: ", item, " no data could be extracted ",
                "from the database collection. ",
                call. = FALSE,
                immediate. = FALSE)
        # create empty data set
        dfi <- data.frame(cbind(idsall,
                                rep(NA, times = length(idsall))),
                          stringsAsFactors = FALSE)
      }
    }

    # name result set
    names(dfi) <- c("_id", item)
    # type item field
    if (all(!is.na(dfi[, 2]))) dfi <- typeField(dfi)
    # add to result
    if (is.null(result)) {
      result <- dfi
    } else {
      # type fields where defined and possible, then
      # merge the new dfi (a data frame of _id, name of item)
      # with data frame of previously retrieved results
      result <- merge(result, dfi, by = "_id", all = TRUE)
    }
    #} # not stopped
  } # end for item in fields

  # finalise output
  if (is.null(result)) {
    stop("No records found which had values for the specified fields. ",
         call. = FALSE)
  }

  # some results were obtained

  # add metadata
  result <- addMetaData(result,
                        con = con)

  # notify user
  diff <- length(idsall) - nrow(result)
  if (diff > 0) {
    warning(diff, " of ", nrow(result), " records dropped which did not ",
            "have values for any of the specified fields. ",
            call. = FALSE, immediate. = FALSE)}

  # return
  return(result)
}
# dbGetFieldsIntoDf


#' Extract named element(s) from list(s) into long-format
#' data frame
#'
#' The function uses a name (key) to extract an element
#' from a list in a data.frame such as obtained with
#' \link{dbGetFieldsIntoDf}. This helps to simplify
#' working with nested lists and with complex structures.
#'
#' @param df A data frame
#' @param list.key A list of pairs of list names and
#'  key names, where the list name corresponds to the
#'  name of a column in \code{df} that holds a list and
#'  the name of the key identifies the element to be
#'  extracted. See example.
#'
#' @return A data frame in long format with columns
#'  name (identifying the full path in the data frame,
#'  "<list>.<key>"), _id (of the trial record), value
#'  (of name per _id), item (number of value of name
#'  per _id).
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' db <- nodbi::src_sqlite(
#'   collection = "my_collection"
#' )
#' df <- dbGetFieldsIntoDf(
#'   fields = c(
#'     "endPoints.endPoint",
#'     "subjectDisposition.postAssignmentPeriods"),
#'   con = db
#' )
#' dfListExtractKey(
#'   df = df,
#'   list.key = list(
#'       c("endPoints.endPoint",
#'         "^title"),
#'       c("subjectDisposition.postAssignmentPeriods",
#'         "arms.arm.type.value")
#' ))
#' }
dfListExtractKey <- function(
  df,
  list.key =
    list(c("endPoints.endPoint", "^title")
    )) {

  # check
  if (!("_id" %in% names(df))) {
    stop("Data frame 'df' lacks '_id' column.",
         call. = FALSE)
    }

  # helper function to extract from
  # a named vector elements by name
  extractKey <- function(flattenedList, key) {

    # find element by key
    selected <- grepl(key,
                      names(flattenedList),
                      ignore.case = TRUE)


    # extract value for key
    extracted <- flattenedList[selected]

    # if key is not found, return a value
    # e.g. missing value (NA) or empty string ("")
    # please change as wanted for later processing
    if (length(extracted) == 0) extracted <- NA

    # return
    return(extracted)
  }

  # dots needs to be defined because passing
  # it in .Internal(mapply()) is not enough
  out <- lapply(
    list.key,
    function(k)
      lapply(df[[k[1]]], # k[1] = "endPoints.endPoint" identifies
             # the column in data frame with the list
             function(l)
               extractKey(unlist(l, recursive = TRUE),
                          k[2]) # k[2] = "^title" identifies
                                # the key in the sublist
      ))

  out <- sapply(seq_along(list.key), function(li) {

    tmp <- out[[li]]

    tmp <- sapply(

      seq_along(tmp),
      function(ii) {

        data.frame(
          name = gsub("[-0-9]*$", "", # trialing number
                      gsub("[^a-zA-Z0-9_.-]", "",
                      paste0(list.key[[li]],
                             collapse = "."))),
          "_id" = df[["_id"]][[ii]],
          value = tmp[[ii]],
          item = seq_along(tmp[[ii]]),
          row.names = NULL,
          stringsAsFactors = FALSE,
          check.names = FALSE)
      }, simplify = FALSE)

    do.call(rbind, tmp)

  }, simplify = FALSE)

  out <- do.call(rbind, out)

  # return
  out
}


#' Merge two variables into one, optionally map values to new levels
#'
#' @param df A \link{data.frame} in which there are two variables (columns)
#' to be merged into one.
#' @param colnames A vector of length two with names of the two columns
#' that hold the variables to be merged. See \link{colnames} for how to
#' obtain the names of columns of a data frame.
#' @param levelslist A list with one slice each for a new value to be
#' used for a vector of old values (optional).
#' @param ... for deprecated varnames parameter (will be removed)
#'
#' @return A vector of strings
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' statusvalues <- list(
#'   "ongoing" = c("Recruiting", "Active", "Ongoing",
#'                 "Active, not recruiting", "Enrolling by invitation"),
#'   "completed" = c("Completed", "Prematurely Ended", "Terminated"),
#'   "other" = c("Withdrawn", "Suspended",
#'               "No longer available", "Not yet recruiting"))
#'
#' dfMergeTwoVariablesRelevel(
#'   df = result,
#'   colnames = c("Recruitment", "x5_trial_status"),
#'   levelslist = statusvalues)
#' }
#'
dfMergeTwoVariablesRelevel <- function(
  df = NULL,
  colnames = "",
  levelslist = NULL,
  ...) {

  # check parameters

  # FIXME migrate from previously
  # used parameter "varnames"
  tmp <- match.call()
  tmp <- tmp["varnames"]
  tmp <- as.list(tmp)[[1]]
  if (length(tmp) == 3 && colnames == "") {
    colnames <- unlist(as.list(tmp[-1]))
    warning("Parameter varnames is deprecated, use colnames instead.",
            call. = FALSE)
  }

  # other checks
  if (class(df) != "data.frame") {
    stop("Need a data frame as input.", call. = FALSE)
  }
  if (length(colnames) != 2) {
    stop("Please provide exactly two column names.", call. = FALSE)
  }

  # find variables in data frame and merge
  tmp <- match(colnames, names(df))
  df <- df[, tmp]
  df[, 1] <- ifelse(is.na(tt <- df[, 1]), "", tt)
  df[, 2] <- ifelse(is.na(tt <- df[, 2]), "", tt)
  tmp <- paste0(df[, 1], df[, 2])

  if (!is.null(levelslist)) {

    # check
    if (class(levelslist) != "list") {
      stop("Need list for parameter 'levelslist'.", call. = FALSE)
    }

    # helper function to collapse factor levels into the first
    refactor <- function(x, collapselevels, levelgroupname){
      levels(x) [match(collapselevels, levels(x))] <- levelgroupname
      return(x)
    }

    # convert result to factor as this is needed for helper function
    tmp <- as.factor(tmp)

    # apply helperfunction to elements of the list
    for (i in seq_len(length(levelslist))) {
      tmp <- refactor(tmp, unlist(levelslist[i]),
                      attr(levelslist[i], "names"))
    }

    # convert factor back into string vector
    tmp <- as.character(tmp)

  }

  # check and inform user
  if (length(tt <- unique(tmp)) > 10) {
    message("Unique values returned (limited to ten): ",
            paste(tt[1:10], collapse = ", "))
  } else {
    message("Unique values returned: ",
            paste(tt, collapse = ", "))
  }

  # return
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
#' @param prefermemberstate Code of single EU Member State for which records
#' should returned. If not available, a record for GB or lacking this, any
#' other record for the trial will be returned. For a list of codes of EU
#'   Member States, please see vector \code{countriesEUCTR}. Alternatively,
#'   "3RD" will lead to return a Third Country record of a trial, if available.
#' @param include3rdcountrytrials A logical value if trials should be retained
#'   that are conducted exclusively in third countries, that is, outside
#'   the European Union.
#'
#' @return A data frame as subset of \code{df} corresponding to the sought
#'   records.
#'
#' @keywords internal
#
dfFindUniqueEuctrRecord <- function(
  df = NULL,
  prefermemberstate = "GB",
  include3rdcountrytrials = TRUE) {

  # check parameters
  if (class(df) != "data.frame") {
    stop("Parameter df is not a data frame.", call. = FALSE)
  }
  #
  if (is.null(df[["_id"]]) ||
      is.null(df["a2_eudract_number"])) {
    stop('Data frame does not include "_id"',
         ' and "a2_eudract_number" columns.',
         call. = FALSE)
  }
  #
  if (nrow(df) == 0) {
    stop("Data frame does not contain records (0 rows).",
         call. = FALSE)
  }
  #
  if (!(prefermemberstate %in% countriesEUCTR)) {
    stop("Value specified for prefermemberstate does not match",
         " one of the recognised codes: ",
         paste(sort(countriesEUCTR), collapse = ", "),
         call. = FALSE)
  }

  # notify it mismatching parameters
  if (prefermemberstate == "3RD" & !include3rdcountrytrials) {
    warning("Preferred EUCTR version set to 3RD country trials, but ",
            "'include3rdcountrytrials' was FALSE, setting it to TRUE.",
            call. = FALSE,
            noBreaks. = FALSE,
            immediate. = FALSE)
    include3rdcountrytrials <- TRUE
  }

  # as a first step, handle 3rd country trials e.g. 2010-022945-52-3RD
  # if retained, these trials would count as record for a trial
  if (!include3rdcountrytrials) {
    df <- df[!grepl("-3RD", df[["_id"]]), ]
  }

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

  # finds per trial the desired record;
  # uses prefermemberstate and nms
  result <- lapply(nst,
                   function(x) removeMSversions(x))
  result <- unlist(result)

  # eleminate the unwanted EUCTR records
  df <- df[!(df[["_id"]] %in% result), ]

  # also eliminate the meta-info record
  df <- df[!(df[["_id"]] == "meta-info"), ]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result))) {
    message(
      " - ", tmp,
      " EUCTR _id were not preferred EU Member State record of trial")
  }

  # return
  return(df)

}
# end dfFindUniqueEuctrRecord


#' Check if a document exists based on its unique identier
#'
#' @return logical, FALSE (document or database does not
#'  exist) or TRUE
#'
#' @importFrom nodbi docdb_exists docdb_query
#'
#' @inheritParams ctrDb
#'
#' @keywords internal
#'
checkDoc <- function(con, id) {

  # check if table exists
  restbl <- nodbi::docdb_exists(
    src = con,
    key = con$collection)

  # table exists, check for document
  if (restbl) {
    resdoc <- try(
      nodbi::docdb_query(
        src = con,
        key = con$collection,
        query = paste0('{"_id": "', id, '"}'),
        fields = '{"_id": 1}'),
      silent = TRUE)
  } else {
    resdoc <- NULL
  }

  # if no error, check if 1 document found
  if ("try-error" %in% class(resdoc)) {
    resdoc <- FALSE
  } else {
    resdoc <- ifelse(restbl,
                     nrow(resdoc) == 1L,
                     FALSE)
  }

  # return false or true
  return(resdoc)

}



#' Change type of field based on name of field
#'
#' @param dfi a data frame of columns _id, fieldname
#'
#' @keywords internal
#'
typeField <- function(dfi){

  # check
  if (ncol(dfi) != 2) {
    stop("Expect data frame with two columns, _id and a field.",
         call. = FALSE)
  }

  # clean up anyway in input
  #
  # - just return if all is a list, such as with parent elements
  if (inherits(dfi[,2], "list")) return(dfi)
  #
  # - if NA as string, change to empty string
  if (all(class(dfi[, 2]) == "character")) dfi[ dfi[, 2] == "NA", 2] <- ""
  #
  # - if empty string, change to NA
  # if (all(class(dfi[, 2]) == "character")) dfi[ dfi[, 2] == "", 2] <- NA
  #
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
    "trialInformation.primaryCompletionDate"                                 = as.Date(dfi[, 2], format = "%Y-%m-%d"),
    "trialInformation.globalEndOfTrialDate"                                  = as.Date(dfi[, 2], format = "%Y-%m-%dT%H:%M:%S"),
    "trialInformation.recruitmentStartDate"                                  = as.Date(dfi[, 2], format = "%Y-%m-%dT%H:%M:%S"),
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
#' @inheritParams ctrDb
#'
#' @keywords internal
#'
addMetaData <- function(x, con) {

  # add metadata
  attr(x, "ctrdata-dbname")         <- con$db
  attr(x, "ctrdata-table")          <- con$collection
  attr(x, "ctrdata-dbqueryhistory") <- dbQueryHistory(
    con = con,
    verbose = FALSE)

  # return annotated object
  return(x)

} # end addMetaData



#' Function to set proxy
#'
#' @importFrom curl ie_proxy_info
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



#' Convenience function to install a minimal cygwin environment under MS
#' Windows, including perl, sed and php
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
#'   "host_or_ip:port". \code{installCygwinWindowsDoInstall} may detect and use
#'   the proxy configuration uset in MS Windows to use an automatic proxy
#'   configuration script. Authenticated proxies are not supported at this time.
#'
installCygwinWindowsDoInstall <- function(
  force = FALSE,
  proxy = ""){

  # checks
  if (.Platform$OS.type != "windows") {
    stop(
      "This function is only for MS Windows operating systems.",
      call. = FALSE)
  }
  #
  if (!force & dir.exists("c:\\cygwin")) {
    stop(
      "cygwin is already installed. To overwrite, use force = TRUE.",
      call. = FALSE)
  }

  # define installation command
  installcmd <- paste0(
    "--no-admin --quiet-mode --upgrade-also --no-shortcuts --prune-install ",
    "--root c:/cygwin ",
    "--site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ ",
    "--packages perl,php-simplexml,php-json")

  # create R session temporary directory
  tmpfile <- paste0(tempdir(), "/cygwin_inst")
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")

  # generate download url
  tmpurl <- ifelse(
    grepl("x64", utils::win.version()),
    "setup-x86_64.exe",
    "setup-x86.exe")
  tmpurl <- paste0("https://cygwin.org/", tmpurl)

  # inform user
  message("Attempting cygwin download using ",
          tmpurl, " ...")

  # check and set proxy if needed to access internet
  setProxy()

  # download.file uses the proxy configured in the system
  tmpdl <- try({
    utils::download.file(
      url = tmpurl,
      destfile = dstfile,
      quiet = FALSE,
      mode = "wb")
  }, silent = TRUE)

  # check
  if (!file.exists(dstfile) ||
      file.size(dstfile) < (5 * 10 ^ 5) ||
      ("try-error" %in% class(tmpdl))) {
    stop("Failed, please download manually and install with:\n",
         tmpurl, " ", installcmd,
         call. = FALSE)
  }

  # proxy handling
  if (proxy != "") {
    # manual setting overriding
    proxy <- paste0(" --proxy ", proxy)
    message("Setting cygwin proxy install argument to: ",
            proxy, ", based on provided parameter.")
  } else {
    # detect proxy
    proxy <- curl::ie_proxy_info()$Proxy
    if (!is.null(proxy)) {
      message("Setting cygwin proxy install argument to: ",
              proxy, ", based on system settings.")
      proxy <- paste0(" --proxy ", proxy)
    }
  }

  # execute cygwin setup command
  system(paste0(dstfile, " ", installcmd,
                " --local-package-dir ", tmpfile, " ", proxy))

  # return cygwin installation test
  return(installCygwinWindowsTest(verbose = TRUE))

}
# end installCygwinWindowsDoInstall


#' Convenience function to test for working cygwin installation
#'
#' @param verbose If \code{TRUE}, prints confirmatory
#'  message (default \code{FALSE})
#'
#' @return Information if cygwin can be used, \code{TRUE}
#'  or \code{FALSE}, or NULL if not under MS Windows
#'
#' @keywords internal
#
installCygwinWindowsTest <- function(verbose = FALSE) {
  #
  if (.Platform$OS.type != "windows") {
    message("Function installCygwinWindowsTest() is ",
            "only for MS Windows operating systems.")
    return(invisible(NULL))
  }
  #
  tmpcygwin <- try({
    suppressWarnings(
      system(
        paste0("cmd.exe /c ",
               rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
               " --version"),
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
    message("cygwin is not available for this package, ",
            "ctrLoadQueryIntoDb() will not work.\n",
            "Consider calling ",
            "ctrdata::installCygwinWindowsDoInstall() ",
            "from within R.")
    return(invisible(FALSE))
  }
}
# end installCygwinWindowsTest


#' Check availability of binaries installed locally
#'
#' @param commandtest Command to be used for testing
#' the availability of the binary, e.g. "php -v".
#' Note internal quotes need to be escaped, e.g.
#' \code{installFindBinary('php -r
#' \"simplexml_load_string(\'\');\"')}.
#' See R/onload.R for tested binaries.
#'
#' @param verbose Set to \code{TRUE} to see printed
#' return value of \code{commandtest}
#'
#' @return A logical if executing commandtest
#' returned an error or not
#'
#' @keywords internal
#
installFindBinary <- function(commandtest = NULL, verbose = FALSE) {
  #
  if (is.null(commandtest)) {
    stop("Empty argument: commandtest",
         call. = FALSE)
  }
  #
  if (.Platform$OS.type == "windows") {
    commandtest <-
      paste0(rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
             " --login -c ",
             shQuote(commandtest))}
  #
  if (verbose) {print(commandtest)}
  #
  commandresult <- try(
    suppressWarnings(
      system(commandtest,
             intern = TRUE,
             ignore.stderr =
               ifelse(.Platform$OS.type == "windows",
                      FALSE, TRUE))),
    silent = TRUE
  )
  #
  commandreturn <- ifelse(
    class(commandresult) == "try-error" ||
      grepl("error|not found", tolower(paste(commandresult, collapse = " "))) ||
      (!is.null(attr(commandresult, "status")) &&
         (attr(commandresult, "status") != 0)),
    FALSE, TRUE)
  #
  if (!commandreturn) {
    warning(commandtest, " not found.",
            call. = FALSE,
            immediate. = FALSE)
  } else {
    if (interactive()) {
      message(". ", appendLF = FALSE)
    }
  }
  #
  if (verbose) {
    print(commandresult)
  }
  #
  return(commandreturn)
  #
}
# end installFindBinary
