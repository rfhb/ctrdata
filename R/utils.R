### ctrdata package
### utility functions


## global variable definitions
#
# EUCTR definitions
agegroupsEUCTR <- c("Preterm newborn infants", "Newborns", "Infants and toddlers", "Children", "Adolescents",
                    "Under 18", "Adults", "Elderly")
variablesEUCTR <- c("EudraCT Number", "Sponsor Protocol Number", "Sponsor Name", "Full Title", "Start Date",
                    "Medical condition", "Disease", "Population Age", "Gender", "Trial protocol", "Link")
countriesEUCTR <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT",
                    "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB")


#' Open advanced search pages of register(s) in default web browser.
#'
#' @param register Register(s) to open. Either "EUCTR" or "CTGOV" or a vector of both. Default is to open both registers' advanced search pages.
#' @param copyright If set to \code{TRUE}, opens copyright pages of register.
#' @param ... Any additional parameter to use with browseURL, which is called by this function.
#' @export openCTRWebBrowser
#' @return Is always true, invisibly.
#'
openCTRWebBrowser <- function(register = c("EUCTR"), copyright = FALSE, ...) {
  #
  if (copyright == TRUE) {
    if ("CTGOV" %in% register) browseURL("https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use", ...)
    if ("EUCTR" %in% register) browseURL("http://www.ema.europa.eu/ema/index.jsp?curl=pages/regulation/general/general_content_000178.jsp&mid=", ...)
  } else {
    if ("CTGOV" %in% register) browseURL("https://clinicaltrials.gov/ct2/search/advanced", ...)
    if ("EUCTR" %in% register) browseURL("https://www.clinicaltrialsregister.eu/ctr-search/search", ...)
  }
  #
  invisible(TRUE)
}


#' Import from clipboard the URL of a search in one of the registers
#'
#' @param content URL from browser address bar. Defaults to clipboard contents.
#' @return A string of query parameters that can be used to retrieve data from the register.
#' @import clipr
#' @export getCTRQueryUrl
#' @return A query term that can be used with \link{getCTRdata}
#' @examples
#' \dontrun{
#' getCTRdata (getCTRQueryUrl, register = "CTGOV")
#' }
#'
getCTRQueryUrl <- function(content = clipr::read_clip()) {
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
    return(queryterm)
  }
  #
  if (grepl("https://clinicaltrials.gov/ct2/results", content)) {
    #
    queryterm <- sub("https://clinicaltrials.gov/ct2/results[?]term=(.*)", "\\1", content)
    queryterm <- sub("(.*)&Search=Search", "\\1", queryterm)
    message("Found search query from CTGOV.")
    return(queryterm)
  }
  #
  stop(paste("Clipboard content is not a clinical trial register search URL. Returning NULL."))
  return(NULL)
}


#' Find names of keys (fields) in the data base
#'
#' Note that generating a list of keys with variety.js as used in this function may not work with certain remote mongo databases,
#' for example when the host or port is different per database, such as with a free mongolab plan
#'
#' @param namepart A plain string (not a regular expression) to be searched for among all field names (keys) in the database.
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata".
#' @param allmatches If \code{TRUE}, returns all keys if more than one is found (default is \code{FALSE}).
#' @param forceupdate If \code{TRUE}, refreshes collection of keys (default is \code{FALSE}).
#' @param debug If \code{TRUE}, prints additional information (default is \code{FALSE}).
#' @return Vector of first keys (fields) found (or of all keys, see above)
#' @import rmongodb curl
#' @export findCTRkey
#' @examples
#' \dontrun{
#'  findCTRkey ("other")
#' }
#'
findCTRkey <- function(namepart = "",
                       mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata",
                       allmatches = FALSE, debug = FALSE, forceupdate = FALSE) {

  # sanity checks
  if (!is.atomic(namepart)) stop("Name part should be atomic.")
  if (length(namepart) > 1) stop("Name part should only have one element.")
  if (namepart == "" & !forceupdate) stop("Empty name part string.")

  # check program availability
  if (.Platform$OS.type == "windows") {
    findMongo()
    if (is.na(mongoBinaryLocation)) stop("Not starting findCTRkey because mongo was not found.")
  }

  # check if database with variety results exists or should be forced to be updated
  if (forceupdate || length(mongo.get.database.collections(mongo, db = "varietyResults")) == 0L ||
      length(grepl(paste0(ns, "Keys"), mongo.get.database.collections(mongo, db = "varietyResults"))) == 0L) {
    #
    if (!grepl("localhost", attr(mongo, "host"))) warning("variety.js may fail with certain remote servers (for example when the host or port ",
                                                          "is different per database, such as with a free mongolab plan).", immediate. = TRUE)
    #
    # check if extension is available (system.file under MS Windows does not end with slash) ...
    varietylocalurl <- paste0(system.file("", package = "ctrdata"), "/exec/variety.js")
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
      varietymongo <- paste(mongoBinaryLocation, varietymongo)
    }
    #
    message("Calling mongo with variety.js and adding keys to data base ...")
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


#' Convert a mongo query result object into a data frame
#'
#' @param x A result of object of a mongo query
#' @return A data frame with the data that was stored in the input object
#' @import rmongodb
#' @export mongo2df
#'
mongo2df <- function(x) {
  xclass <- class(x)
  #
  if (xclass == "mongo.bson") {
    tmp <- rmongodb::mongo.bson.to.list(x)
    df <- data.frame(do.call(rbind, tmp))
    df <- df[2:nrow(df), ]
    return(df)
  }
  if (xclass == "mongo.cursor") {
    df <- rmongodb::mongo.cursor.to.data.frame(x)
    return(df)
  }
  # other type of object
  warning("Could not use input, not a bson or mongo cursor.")
  return(NULL)
}



#' This function checks for duplicates in the database based on the clinical trial identifier and returns a list of ids of unique trials.
#'
#' In case a record for a clinical trial is found from more than one register, the record from EUCTR is returned. The function currently
#' relies on CTGOV recording other identifiers such as the EudraCT number in the field "Other IDs".
#'
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata"
#' @return A vector with strings of keys (_id in the database) that are non-duplicate trials.
#' @export uniquetrialsCTRdata
#' @import rmongodb
#' @examples
#' \dontrun{
#' uniqueCTRdata (mongo, "ctrdata")
#' }
#'
uniquetrialsCTRdata <- function(mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
  #
  # CTGOV: "Other IDs" has been split into the indexed array "otherids"
  listofCTGOVids <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns),
                                             query  = list('_id' = list('$regex' = 'NCT.*')),
                                             fields = list("otherids" = 1L))
  #
  # EUCTR / EudraCT number is "_id" for EUCTR records
  listofEUCTRids <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"),
                                             query  = list('_id' = list('$regex' = '[0-9]{4}-[0-9]{6}-[0-9]{2}-[A-Z]{2}')),
                                             fields = list("_id" = 1L))
  listofEUCTRids <- sapply(listofEUCTRids, "[[", "_id")
  #
  # search for eudract numbers among otherids, by euctr _ids
  #  for this search write eudract numbers as stored in ctgov
  #  for this search make all otherids into atomic vector of strings
  #dupes <- paste0("EUDRACT-", substr(listofEUCTRids, 1, 14)) %in% unlist(sapply(listofCTGOVids, "[[", "otherids"))
  #uniques <- c(listofEUCTRids[!dupes], unlist(sapply(listofCTGOVids, "[[", "_id")))
  #
  # search for eudract numbers among otherids, by ctgov _ids
  #  for this search write eudract numbers as stored in ctgov =
  #  sometimes with prefix EUDRACT- sometimes without such prefix
  listofEUCTRidsForSearch <- c(substr(listofEUCTRids, 1, 14), paste0("EUDRACT-", substr(listofEUCTRids, 1, 14)))
  #test
  #listofEUCTRidsForSearch <- c("2014-004697-41","2015-002154-12", "EUDRACT-2006-000205-34")
  uniques <- sapply(listofCTGOVids, "[[", "_id")[!sapply(sapply(listofCTGOVids, "[[", "otherids"),
                                                         function(x) sum(listofEUCTRidsForSearch %in% unlist(x)))]
  uniques <- c(listofEUCTRids, uniques)
  #
  countall <- length(listofCTGOVids) + length(listofEUCTRids)
  #
  message(paste0("Total ", countall - length(uniques), " duplicate(s) found, returning keys (_id) of ", countall, " records."))
  #
  return(uniques)
  #
}


#' Create a data frame from records that have specified fields in the data base
#'
#' @param fields Vector of strings, with names of the sought fields. (Do not use a list)
#' @return A data frame with columns corresponding to the sought fields.
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata"
#' @export dbCTRGet
#'
dbCTRGet <- function(fields = "", mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
  if (!is.vector(fields) | class(fields) != "character") stop("Input should just be a vector of strings of field names.")
  #
  countall <- rmongodb::mongo.count(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"))
  #
  q <- sapply(fields, function(x) list(list('$gt' = '')))
  f <- sapply(fields, function(x) list(2L))
  #
  result <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"),
                                     query = q, fields = f, data.frame = TRUE)
  #
  if (countall > nrow(result)) warning(paste0(countall - nrow(result), " of ", countall,
                                              " records dropped as one or more of the specified fields were not found."))
  #
  return(result)
}

#' Create a data frame from all records in the data base
#'
#' @return A data frame with columns corresponding to the sought fields.
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata"
#' @export dbCTRGetAll
#'
dbCTRGetAll <- function(mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
  #
  result <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"), data.frame = TRUE)
  #
  return(result)
}



#' Convenience function to install a cygwin environment under MS Windows, including perl
#'
#' @export installCygwin
#' @param overwrite Set to true to force updating and overwriting an existing installation in \code{c:\\cygwin}
#'
installCygwin <- function(overwrite = FALSE){
  #
  if (.Platform$OS.type == "windows") {
    #
    if (!overwrite & dir.exists("c:\\cygwin")) stop("cygwin is already installed. To overwrite, call this function with overwrite = TRUE.")
    #
    installcmd <- "--no-admin --quiet-mode --verbose --site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages perl"
    tmpfile <- tempdir()
    dstfile <- paste0(tmpfile, "/cygwinsetup.exe")
    #
    if (.Platform$r_arch == "x86_64") download.file(url = "http://cygwin.org/setup-x86_64.exe", destfile = dstfile, quiet = TRUE, mode = "wb")
    if (.Platform$r_arch == "i386")   download.file(url = "http://cygwin.org/setup-x86.exe", destfile = dstfile, quiet = TRUE, mode = "wb")
    #
    if (!file.exists(dstfile))         stop("Download failed. Please install manually.")
    if (file.size(dstfile) < 5*10 ^ 5) stop("Download seem to have failed - file too small. Please install manually.")
    #
    system(paste0(dstfile, ' ', installcmd))
    unlink(tmpfile, recursive = TRUE)
    #
    # test cygwin installation
    ctrdata::testCygwin()
    #
  } else {
    #
    warning("This function is only for MS Windows operating systems.")
    #
  }
  #
}




#' Convenience function to test for working cygwin installation
#'
#' @return Information if cygwin can be used, \code{TRUE} or \code{FALSE}
#' @export testCygwin
#'
testCygwin <- function() {
  #
  if (.Platform$OS.type != "windows") stop("This function is only for MS Windows operating systems.")
  #
  tmpcygwin <- system("cmd.exe /c c:\\cygwin\\bin\\env", intern = TRUE)
  #
  if (length(tmpcygwin) > 5) {
    message("cygwin seems to be working correctly.")
  } else {
    warning("cygwin does not seem to be installed correctly.")
  }
  #
  invisible(tmpcygwin)
  #
}


#' Convenience function to find location of mongoimport
#'
#' @return Either an empty string if \code{mongoimport} was found on the path or, under MS Windows, a string representing the path
#' @export findMongo
#'
findMongo <- function() {
  #
  # debug: mongoBinaryLocation <- "/usr/bin/"
  tmp <- ifelse(.Platform$OS.type != "windows", "mongoimport", "mongoimport.exe")
  #
  if (exists("mongoBinaryLocation") && !is.na(mongoBinaryLocation)
      && file.exists(paste0(mongoBinaryLocation, tmp))) {
    #
    message("mongoimport / mongo is in ", mongoBinaryLocation)
    invisible(mongoBinaryLocation)
    #
  } else {
    #
    # not found: reset any information and start searching
    assign("mongoBinaryLocation", NA, envir = .GlobalEnv)
    #
    # first test for binary in the path
    tmp <- try(if (.Platform$OS.type != "windows") {
      system('mongoimport --version', intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    } else {
      system('mongoimport --version', intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE)
    }, silent = TRUE)
    #
    if (class(tmp) != "try-error") {
      #
      # found it in the path, save empty location string in user's global environment
      message("mongoimport / mongo found in the path.")
      assign("mongoBinaryLocation", "", envir = .GlobalEnv)
      invisible("")
      #
    } else {
      #
      warning("mongoimport / mongo was not found in the path (%PATH%).", immediate. = TRUE)
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
      if (!tmp) stop("Cannot continue. mongoimport not found in folder recorded in the registry, ", location, ".")
      #
      # found it, save in user's global environment
      assign("mongoBinaryLocation", location, envir = .GlobalEnv)
      invisible(location)
      #
    }
    #
  }
  #
}





