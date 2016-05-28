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



#' Open advanced search pages of register(s) or execute search in default web
#' browser.
#'
#' @param register Register(s) to open. Either "EUCTR" or "CTGOV" or a vector of
#'   both. Default is to open both registers' advanced search pages. To open the
#'   browser with a previous search, register (or queryterm) can be the output
#'   of ctrGetQueryUrlFromBrowser() or can be one row from
#'   ctrQueryHistoryInDb().
#' @param copyright (Optional) If set to \code{TRUE}, opens copyright pages of
#'   register(s).
#' @param queryterm (Optional) Show results of search for \code{queryterm} in
#'   browser. To open the browser with a previous search, (register or)
#'   queryterm can be the output of ctrGetQueryUrlFromBrowser() or can be one
#'   row from ctrQueryHistoryInDb().
#' @param ... Any additional parameter to use with browseURL, which is called by
#'   this function.
#'
#' @export ctrOpenSearchPagesInBrowser
#' @return Is always true, invisibly.
#'
ctrOpenSearchPagesInBrowser <- function(register = c("EUCTR", "CTGOV"), copyright = FALSE, queryterm = "", ...) {
  #
  # check arguments
  if (!is.atomic(register) || (register == '' & queryterm == '')) stop("No usable argument found.")
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
  # try to deduce queryterm and register from a url that is provided as anonymous first parameter
  if(queryterm == '' && grepl ("^https.+clinicaltrials.+", register)) queryterm <- ctrdata::ctrGetQueryUrlFromBrowser(content = register)
  #
  # graciously deduce queryterm and register if a url is unexpectedly provided as queryterm
  if(is.character(queryterm) && grepl ("^https.+clinicaltrials.+", queryterm)) queryterm <- ctrdata::ctrGetQueryUrlFromBrowser(content = queryterm)
  #
  # deal with register or queryterm being
  # - a list as returned from ctrGetQueryUrlFromBrowser() or
  # - a data frame as returned from ctrQueryHistoryInDb()
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
    if (class(tmp) == "try-error") stop("ctrOpenSearchPagesInBrowser(): Could not use parameters, please check.")
    #
  }
  #
  if (queryterm != "") {
    message("Opening in browser previous search: ", queryterm, ", in register: ", register)
    if ("CTGOV" %in% register) utils::browseURL(paste0("https://clinicaltrials.gov/ct2/results?", queryterm), ...)
    if ("EUCTR" %in% register) utils::browseURL(paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=", queryterm), ...)
  }
  #
  invisible(TRUE)
}


#' Import from clipboard the URL of a search in one of the registers
#'
#' @param content URL from browser address bar. Defaults to clipboard contents.
#' @return A string of query parameters that can be used to retrieve data from
#'   the register.
#'
#' @import clipr
#'
#' @export ctrGetQueryUrlFromBrowser
#'
#' @return A list with a query term and the register name that can directly be
#'   used in \link{ctrLoadQueryIntoDb}
#'
#' @examples
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
#'
#' @inheritParams ctrdata::ctrLoadQueryIntoDb
#'
#' @return Vector of first keys (fields) found (or of all keys, see above)
#' @import rmongodb curl
#' @export dbFindVariable
#' @examples
#' \dontrun{
#'  dbFindVariable ("date")
#' }
#'
dbFindVariable <- function(namepart = "",
                           mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"), ns = "ctrdata",
                           allmatches = FALSE, debug = FALSE, forceupdate = FALSE) {

  # sanity checks
  if (!is.atomic(namepart)) stop("Name part should be atomic.")
  if (length(namepart) > 1) stop("Name part should have only one element.")
  if (namepart == "" & !forceupdate) stop("Empty name part string.")

  # check program availability
  if (.Platform$OS.type == "windows") {
    installMongoFindBinaries()
    if (is.na(get("mongoBinaryLocation", envir = .privateEnv))) stop("Not running dbFindVariable because mongo binary was not found.")
  }

  # check if database with variety results exists or should be forced to be updated
  if (forceupdate || length(mongo.get.database.collections(mongo, db = "varietyResults")) == 0L ||
      length(grepl(paste0(ns, "Keys"), mongo.get.database.collections(mongo, db = "varietyResults"))) == 0L) {
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


# Convert a mongo query result object into a data frame
#
# @param x A result of object of a mongo query
# @return A data frame with the data that was stored in the input object
# @import rmongodb
# @export mongo2df
#
# mongo2df <- function(x) {
#   xclass <- class(x)
#   #
#   if (xclass == "mongo.bson") {
#     tmp <- rmongodb::mongo.bson.to.list(x)
#     df <- data.frame(do.call(rbind, tmp))
#     df <- df[2:nrow(df), ]
#     return(df)
#   }
#   if (xclass == "mongo.cursor") {
#     df <- rmongodb::mongo.cursor.to.data.frame(x)
#     return(df)
#   }
#   # other type of object
#   warning("Could not use input, not a bson or mongo cursor.")
#   return(NULL)
# }



#' This function checks for duplicate records of clinical trialss in the
#' database based on the clinical trial identifier, and it returns a list of ids
#' of unique trials.
#'
#' If records for a clinical trial are found from more than one register, the
#' record from EUCTR is returned. The function currently relies on CTGOV
#' recording other identifiers such as the EudraCT number in the field "Other
#' IDs".
#'
#' @inheritParams ctrdata::ctrLoadQueryIntoDb
#'
#' @return A vector with strings of keys (_id in the database) that are
#'   non-duplicate trials.
#'
#' @export dbFindIdsUniqueTrials
#' @import rmongodb
#' @examples
#' \dontrun{
#' uniqueCTRdata (mongo, "ctrdata")
#' }
#'
dbFindIdsUniqueTrials <- function(mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"), ns = "ctrdata") {
  #
  #
  # TODO: "a52_us_nct_clinicaltrialsgov_registry_number" found in euctr from 2016
  #
  # CTGOV: "Other IDs" has been split into the indexed array "otherids"
  listofCTGOVids <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns),
                                             query  = list('_id' = list('$regex' = 'NCT[0-9]{8}')),
                                             fields = list('otherids' = 1L, '_id' = 1L))

  # EUCTR / EudraCT number is "_id" for EUCTR records
  listofEUCTRids <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns),
                                             query  = list('_id' = list('$regex' = '[0-9]{4}-[0-9]{6}-[0-9]{2}-[A-Z]{2}')),
                                             fields = list('_id' = 1L))

  # 1. search for eudract numbers among otherids, by ctgov _ids
  # for this search, write eudract numbers as stored in ctgov =
  # sometimes with prefix EUDRACT-, sometimes without such prefix
  listofEUCTRids <- sapply(listofEUCTRids, "[[", "_id")
  listofEUCTRidsForSearch <- c(substr(listofEUCTRids, 1, 14), paste0("EUDRACT-", substr(listofEUCTRids, 1, 14)))
  #test: listofEUCTRidsForSearch <- c("2014-004697-41","2015-002154-12", "EUDRACT-2006-000205-34")

  # find CTGOV _ids with respective otherids NOT being one of the EUCTR numbers in database
  uniques <- sapply(listofCTGOVids, "[[", "_id")[!sapply(sapply(listofCTGOVids, "[[", "otherids"),
                                                         function(x) sum(listofEUCTRidsForSearch %in% unlist(x)))]
  # add found records to vector of unique _ids
  uniques <- c(listofEUCTRids, uniques)
  message("Searched for EUCTR identifiers in second id fields of CTGOV records.")

  # 2. search for ctgov numbers among otherids, by ctgov _ids
  dupes <- sapply(listofCTGOVids, "[[", "_id") %in% unlist(sapply(listofCTGOVids, "[[", "otherids"))
  message("Searched for CTGOV identifiers in second id fields of CTGOV records.")
  # there may be circular references for records _id1 -> otherid2, _id2 -> otherid1
  # these are currently unresolved and require more work to be found. for now just flag:
  if (sum(dupes) > 0) warning('Please manually check "_id" and "otherids", because more than one record found for CTGOV trial(s): \n',
                              listofCTGOVids[dupes])

  # prepare output
  # avoid returning list() if none found
  if(length(uniques) == 0) uniques <- character()
  countall <- length(listofCTGOVids) + length(listofEUCTRids)
  message(paste0("Total ", countall - length(uniques), " duplicate(s) found, returning keys (_id) of ", length(uniques), " records."))
  #
  return(uniques)
  #
}


#' Create a data frame from records in the database that have specified fields
#'
#' With this convenience function, fields in the mongo database are retrieved
#' into an R dataframe. As mongo fields can be hierarchical and structured, the
#' function includes provisions for arrays (only the first slice is returned at
#' this time) and for multiple entries in a field (which are contatenated in the
#' returned results using ' / ').
#'
#' For more sophisticated retrieval from the database, see vignette examples and
#' packages such as mongolite.
#'
#' @param fields Vector of strings, with names of the sought fields.
#' @inheritParams ctrdata::ctrLoadQueryIntoDb
#'
#' @return A data frame with columns corresponding to the sought fields. Note
#'   that a column for the record _id will always be included. The maximum
#'   number of rows of the returned data frame is equal to or less than the
#'   number of records in the data base.
#'
#' @export dbGetVariablesIntoDf
#'
dbGetVariablesIntoDf <- function(fields = "", mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"),
                                 ns = "ctrdata", debug = FALSE) {
  #
  if (!is.vector(fields) | class(fields) != "character") stop("Input should be a vector of strings of field names.")
  #
  # total number of records in collection. for information of user at end of function.
  countall <- rmongodb::mongo.count(mongo, paste0(attr(mongo, "db"), ".", ns),
                                    query  = rmongodb::mongo.bson.from.JSON('{"_id":{"$ne":"meta-info"}}'))
  #
  # initialise output
  result <- NULL
  #
  # helper function
  fieldIsArray <- function(fieldsearched) {
    # retrieve from varietyKeys what type of variable the searched field is
    tmp <- mongo.find.all(mongo, paste0("varietyResults.", ns, "Keys"),
                          query  = rmongodb::mongo.bson.from.JSON(paste0('{"_id.key": "', fieldsearched, '"}')),
                          fields = rmongodb::mongo.bson.from.JSON('{"_id": 0, "value.types": 1, "value.types": {"$slice": 1}}'))
    # no relevant result retrieved
    if(is.null(tmp) || length(tmp) == 0) return(FALSE)
    # result is not empty
    if (debug) message("DEBUG: variable nesting according to varietyKeys: ", tmp)
    # example:
    # list(value = list(types = list(Array = 951)))
    # list(value = list(types = list(Array = 1618, String = 1)))
    tmp <- unlist(tmp)
    tmp <- names(tmp)
    # example:
    # value.types.Array value.types.String
    # use right-most element
    tmp <- tmp [length(tmp)]
    tmp <- sub('value\\.types\\.(.+)', '\\1', tmp)
    if (debug) message("DEBUG: variable info according to varietyKeys: ", tmp)
    #
    if(tmp == "String") return(FALSE)
    if(tmp == "Array")  return(TRUE)
    # default
    return(FALSE)
  }
  #
  for (item in fields) {
    #
    query <- paste0('{"_id": {"$ne": "meta-info"}, "', item, '": {"$gt": ""}}')
    part1 <- sub("(.*)[.].*", "\\1", item)
    if (debug) message("DEBUG: variable corresponding to first part of field, before dot: ", part1)
    #
    if (fieldIsArray(part1)) {
      tmp <- try({
        if (debug) message("DEBUG: variable ", item, " handled as array")
        dfi <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), '.', ns), data.frame = FALSE,
                                        query  = rmongodb::mongo.bson.from.JSON(query),
                                        fields = rmongodb::mongo.bson.from.JSON(paste0('{"_id": 1, "', part1, '": {"$slice": 1}, "', item, '": 1}')))
        if (debug) message("DEBUG: variable ", item, " has length ", length(dfi))
        # attempt custom function to condense into a data frame instead of using data.frame = TRUE
        dfi <- as.data.frame(cbind(sapply(dfi, function(x) as.vector(x[[1]])),
                                   sapply(dfi, function(x) as.vector(unlist (x[[2]])))),
                             stringsAsFactors = FALSE)
        names(dfi) <- c("_id", item)
        #
      }, silent = FALSE)
      warning(paste0("For variable: ", item, " only the first slice of the array is returned."), immediate. = TRUE)
    } else {
      tmp <- try({
        if (debug) message("DEBUG: variable ", item, " handled as string")
        dfi <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), '.', ns), data.frame = FALSE,
                                        query  = rmongodb::mongo.bson.from.JSON(query),
                                        fields = rmongodb::mongo.bson.from.JSON(paste0('{"_id": 1, "', item, '": 1}')))
        if (debug) message("DEBUG: variable ", item, " has length ", length(dfi))
        # attempt custom function to condense into a data frame instead of using data.frame = TRUE
        dfi <- as.data.frame(cbind(sapply(dfi, function(x) as.vector(unlist(x[1]))),
                                   sapply(dfi, function(x) paste0(as.vector(unlist(x[2])), collapse = " / "))),
                             stringsAsFactors = FALSE)
        names(dfi) <- c("_id", item)
        #
      }, silent = FALSE)
    }
    #
    if ((class(tmp) != "try-error") && (nrow(dfi) > 0)) {

      if (is.null(result)) {
        result <- dfi
      } else {
        result <- merge(result, dfi, by = '_id', all = TRUE)
      }

    } else {# try-error occured
      stop(paste0("For variable: ", item, " no data could be extracted, please check the contents of the database."))
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
#' @param prefer Code of single EU Member State for which records should
#'   returned if available. (If not available, a record for GB or lacking this
#'   any other record for the trial will be returned.) For a list of codes of EU
#'   Member States, please see vector \code{countriesEUCTR}.
#'
#' @return A data frame as subset of \code{df} corresponding to the sought
#'   records.
#'
#' @export dfFindUniqueEuctrRecord
#'
dfFindUniqueEuctrRecord <- function(df = NULL, prefer = "GB") {
  #
  if (class(df) != "data.frame") stop("Parameter df is not a data frame.")
  if (is.null(df [['_id']]) || is.null(df$a2_eudract_number)) stop('Data frame does not include "_id" and "a2_eudract_number" columns.')
  if (nrow(df) == 0) stop("Data frame does not contain records (0 rows).")
  if (!(prefer %in% countriesEUCTR)) stop("Value specified for prefer does not match recognised codes, see countriesEUCTR.")

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
    # Function uses prefer, nms from the caller environment
    #
    recordnames <- nms[indexofrecords]
    #
    # fnd should be only a single string, may need to be checked
    if (sum(fnd <- grepl(prefer, recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    if (sum(fnd <- grepl("GB", recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    return(recordnames[-1])
  }

  # finds per trial the desired record; uses prefer and nms
  result <- lapply(nst, function(x) removeMSversions(x))
  result <- unlist(result)

  # eleminate the unwanted EUCTR records
  df <- df [!(df [['_id']] %in% result), ]
  # also eliminate the meta-info record
  df <- df [!(df [['_id']] == "meta-info"), ]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result))) message(tmp, ' EUCTR records dropped that were not the preferred of multiple records for the trial.')

  return(df)
  #
}



#' Convenience function to install a cygwin environment under MS Windows,
#' including perl and php
#'
#' @export installCygwinWindowsDoInstall
#' @param overwrite Set to true to force updating and overwriting an existing
#'   installation in \code{c:\\cygwin}
#' @param proxy Specify any proxy to be used for downloading via http, e.g.
#'   "host_or_ip:port". \code{installCygwinWindowsDoInstall} may detect and use
#'   the proxy configuration uset in MS Windows to use an automatic proxy
#'   configuration script. Authenticated proxies are not supported at this time.
#'   Alternatively and in case of difficulties, download and run the cygwin
#'   setup yourself as follows: cygwinsetup.exe --no-admin --quiet-mode
#'   --verbose --root c:/cygwin --site
#'   http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages
#'   perl,php-jsonc,php-simplexml
#'
installCygwinWindowsDoInstall <- function(overwrite = FALSE, proxy = ""){
  #
  if (.Platform$OS.type != "windows")        stop("This function is only for MS Windows operating systems.")
  if (!overwrite & dir.exists("c:\\cygwin")) stop("cygwin is already installed. To overwrite, call this function with overwrite = TRUE.")
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
  if (file.size(dstfile) < 5*10 ^ 5) stop("Download seem to have failed - file too small. Please install manually.")
  #
  # find and use proxy settings for actually running the cygwin setup
  tmp <- utils::readRegistry('Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings', hive = "HCU")
  if (proxy != "") {
    # manual setting overrides all
    proxy <- paste0('--proxy ', proxy)
  } else {
    # detect proxy to be used, automatically or manually configured?
    if (tmp$AutoConfigURL != "") {
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
    } else {
      if (is.null(tmp$ProxyServer)) stop('A proxy could not be identified by this function.',
                                         ' Please set manually: installCygwinWindowsDoInstall (proxy = "host_or_ip:port"')
      proxy <- paste0('--proxy ', tmp$ProxyServer)
    }
  }
  #
  # compose installation command
  installcmd <- "--no-admin --quiet-mode --verbose  --root c:/cygwin --site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages perl,php-jsonc,php-simplexml"
  #
  # execute cygwin setup command
  system(paste0(dstfile, " ", installcmd, " --local-package-dir ", tmpfile, ' ', proxy))
  #
  # test cygwin installation
  installCygwinWindowsTest()
  #
}



#' Convenience function to test for working cygwin installation
#'
#' @return Information if cygwin can be used, \code{TRUE} or \code{FALSE}
#'
#' @export installCygwinWindowsTest
#'
installCygwinWindowsTest <- function() {
  #
  if (.Platform$OS.type != "windows") stop("This function is only for MS Windows operating systems.")
  #
  tmpcygwin <- system("cmd.exe /c c:\\cygwin\\bin\\env", intern = TRUE)
  # TODO check for packages perl and php installed?
  #
  if (length(tmpcygwin) > 5) {
    message("cygwin seems to be working correctly.")
    invisible(TRUE)
  } else {
    warning("cygwin does not seem to be installed correctly.")
    invisible(FALSE)
  }
}


#' Convenience function to find location of mongo database binaries (mongo,
#' mongoimport)
#'
#' @param mongoDirWin Only used under MS Windows: folder that contains mongo
#'   binaries, defaults to "c:\\mongo\\bin\\" as used on
#'   \url{http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows#interactive-installation}
#'
#' @return Either an empty string if \code{mongoimport} was found on the path
#'   or, under MS Windows, a string representing the path to the folder of the
#'   mongo binaries
#'
#' @export installMongoFindBinaries
#'
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
        assign("mongoBinaryLocation", location, envir = .privateEnv)
        message("mongoimport / mongo found in ", location)
        invisible(location)
        #
      }
    }
  }
}


#' Check the version of the build of the mongo server to be used
#'
#' In addition to the returned value, the function will generate a warning
#' message if applicable.
#'
#' @inheritParams ctrdata::ctrLoadQueryIntoDb
#'
#' @return A logical value indicating if the mongodb version is acceptable for
#'   use with this package.
#'
#' @export installMongoCheckVersion
#'
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
#' @export dfMergeTwoVariablesRelevel
#' @examples
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
  tmp <- df[, tmp]
  tmp1 <- ifelse(is.na(tt <- tmp[ ,1]), "", tt)
  tmp2 <- ifelse(is.na(tt <- tmp[ ,2]), "", tt)
  tmp <- paste0(tmp1, tmp2)

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

  # inform user on levels found
  message("Unique values returned: ", paste(unique(tmp), collapse = ", "))

  return(tmp)
}
# end dfMergeTwoVariablesRelevel



#' Show the history of queries that were loaded into a database
#'
#' @inheritParams ctrdata::ctrLoadQueryIntoDb
#'
#' @return A data frame with variables: timestamp, register, number of records
#'   loaded, query term
#'
#' @export ctrQueryHistoryInDb
#'
#' @examples
#' \dontrun{
#' ctrQueryHistoryInDb()
#' }
#'
ctrQueryHistoryInDb <- function(mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"), ns = "ctrdata") {
  #
  message("Total number of records: ",
          rmongodb::mongo.count(mongo, paste0(attr(mongo, "db"), ".", ns),
                                query  = rmongodb::mongo.bson.from.JSON('{"_id":{"$ne":"meta-info"}}')))
  #
  tmp <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns),
                                  query = list("_id" = "meta-info"), fields = list("query" = 1L, "_id" = 0L))
  if (length(tmp) == 0) {
    # no history found
    tmp <- NULL
    message("No query history found in database in expected format.")
    #
  } else {
    #
    tmp <- tmp[[1]]
    tmp <- sapply(tmp, function(x) do.call(rbind, x))
    tmp <- t(tmp)
    tmp <- data.frame(tmp, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
    names(tmp) <- c("query-timestamp", "query-register", "query-records", "query-term")
    message("Number of queries in history: ", nrow(tmp))
    # TODO: type timestampt, number of records
  }
  #
  return(tmp)
  #
}
# end ctrQueryHistoryInDb


#' Check availability of binaries installed in operating system
#'
#' @param commandtest Command to be used for testing the availability of the binary, e.g. "php -v"
#'
#' @return A logical if executing commandtest returned an error or not
#'
#' @export findBinary
#'
findBinary <- function(commandtest = NULL) {
  #
  if (is.null(commandtest)) stop ("Empty argument: commandtest")
  #
  commandresult <- try(
    system(commandtest, intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE),
    silent = TRUE
  )
  #
  commandreturn <- ifelse (class(commandresult) == "try-error", FALSE, TRUE)
  #
  if(!commandreturn) warning(commandtest, " not found.")
  #
  return(commandreturn)
  #
}

