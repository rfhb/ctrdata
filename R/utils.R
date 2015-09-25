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
openCTRWebBrowser <- function(register = c("EUCTR", "CTGOV"), copyright = FALSE, ...) {
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
#' @return A list with a query term and the register name that can directly be used in \link{getCTRdata}
#' @examples
#' \dontrun{
#' getCTRdata (getCTRQueryUrl())
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
    return(list(queryterm = queryterm, register = "EUCTR"))
  }
  #
  if (grepl("https://clinicaltrials.gov/ct2/results", content)) {
    #
    queryterm <- sub("https://clinicaltrials.gov/ct2/results[?]term=(.*)", "\\1", content)
    queryterm <- sub("(.*)&Search=Search", "\\1", queryterm)
    message("Found search query from CTGOV.")
    return(list(queryterm = queryterm, register = "CTGOV"))
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
#' @export dbFindCTRkey
#' @examples
#' \dontrun{
#'  dbFindCTRkey ("date")
#' }
#'
dbFindCTRkey <- function(namepart = "",
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
      varietymongo <- paste0(mongoBinaryLocation, varietymongo)
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
#' @export dbCTRGetUniqueTrials
#' @import rmongodb
#' @examples
#' \dontrun{
#' uniqueCTRdata (mongo, "ctrdata")
#' }
#'
dbCTRGetUniqueTrials <- function(mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
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
  #
  #test:
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


#' Create a data frame from records in the data base that have specified fields in the data base
#'
#' @param fields Vector of strings, with names of the sought fields. (Do not use a list.)
#' @return A data frame with columns corresponding to the sought fields. Note that a column for the record _id will always be included.
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata"
#' @param all.x If \code{TRUE}, returns one row for each record, even if \code{fields} could not be found. This is
#' useful if the data base includes records from different registers. Default is \code{FALSE} to avoid time intensive operations.
#' @export dbCTRGet
#'
dbCTRGet <- function(fields = "", mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"),
                     ns = "ctrdata", all.x = FALSE) {
  #
  if (!is.vector(fields) | class(fields) != "character") stop("Input should just be a vector of strings of field names.")
  #
  countall <- rmongodb::mongo.count(mongo, paste0(attr(mongo, "db"), ".", ns))

  if (all.x) {
    # in this case create a data frame with a row for each _id
    #
    # initialise output
    result <- NULL
    #
    for (item in fields) {
      #
      q <- sapply(item, function(x) list(list('$gt' = '')))
      f <- sapply(item, function(x) list(2L))
      #
      dfi <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns), query = q, fields = f, data.frame = TRUE)
      #
      if (is.null(result)) {
        result <- dfi
      } else {
        result <- merge(result, dfi, by = '_id', all = TRUE)
      }
    }
    #
    if (countall > nrow(result)) warning(paste0(countall - nrow(result), " of ", countall,
                                                " records dropped which did not have values for any of the specified fields."))
    #
  } else {
    #
    q <- sapply(fields, function(x) list(list('$gt' = '')))
    f <- sapply(fields, function(x) list(2L))
    #
    result <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns), query = q, fields = f, data.frame = TRUE)
    #
    if (is.null(result)) stop('No records found which had values for all specified fields. Consider specifying all.x = TRUE.')
    #
    diff <- countall - nrow(result)
    if (diff > 0) warning(diff, " of ", countall, " records dropped which did not have values for all specified fields.", immediate. = TRUE)
    if ((diff / countall) > 0.3) message('  Consider specifying all.x = TRUE.')
    #
  }
  #
  return(result)
}


#' Select a single trial record when there are records for different EU Member States for this trial.
#'
#' The EUCTR provides one record per trial per EU Member State in which the trial is conducted.
#' For all trials conducted in more than one Member State, this function returns only one record
#' per trial. A preferred Member State can be specified by the user, and a record of the trial in the
#' preferred Member State will be returned if available. If not, an english record ("GB") or lacking this,
#' any other available record will be returned.
#'
#' Note: To depuplicate trials from different registers (EUCTR and CTGOV), please first use function
#' \code{\link{dbCTRGetUniqueTrials}}.
#'
#' @return A data frame as subset of \code{df} corresponding to the sought records.
#'
#' @param df A data frame created from the data base that includes the keys (variables) "_id" and "a2_eudract_number",
#' for example created with function dbCTRGet(c("_id", "a2_eudract_number")).
#' @param prefer Code of single EU Member State for which records should returned if available. (If not available,
#' a record for GB or lacking this any other record for the trial will be returned.) For a list of codes of EU
#' Member States, please see vector \code{countriesEUCTR}.
#' \code{\link{countries}}
#' @export uniqueTrialsEUCTRrecords
#'
uniqueTrialsEUCTRrecords <- function(df = NULL, prefer = "GB") {
  #
  if (class(df) != "data.frame") stop("Parameter df is not a data frame.")
  if (is.null(df$`_id`) || is.null(df$a2_eudract_number)) stop('Data frame does not include "_id" and "a2_eudract_number" columns.')
  if (nrow(df) == 0) stop("Data frame does not contain records (0 rows).")
  if (!(prefer %in% countriesEUCTR)) stop("Value specified for prefer does not match recognised codes, see countriesEUCTR.")

  # count number of records by eudract number
  tbl <- table(df$`_id`, df$a2_eudract_number)
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
  df <- subset(df, subset = !(`_id` %in% result))

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result))) message(tmp, ' records dropped that were not the preferred of multiple EUCTR records for a trial.')

  return(df)
  #
}



#' Convenience function to install a cygwin environment under MS Windows, including perl
#'
#' @export installCygwin
#' @param overwrite Set to true to force updating and overwriting an existing installation in \code{c:\\cygwin}
#' @param proxy Specify any proxy to be used for downloading via http, e.g. "host_or_ip:port". \code{installCygwin}
#' detects and uses the proxy configuration unless this is set in MS Windows to use an automatic proxy configuration script
#'
installCygwin <- function(overwrite = FALSE, proxy = ""){
  #
  if (.Platform$OS.type != "windows")        stop("This function is only for MS Windows operating systems.")
  if (!overwrite & dir.exists("c:\\cygwin")) stop("cygwin is already installed. To overwrite, call this function with overwrite = TRUE.")
  #
  # create directory within R sessions temporary directory
  tmpfile <- paste0(tempdir(), '/cygwin_inst')
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")
  #
  if (.Platform$r_arch == "x86_64") download.file(url = "http://cygwin.org/setup-x86_64.exe", destfile = dstfile, quiet = TRUE, mode = "wb")
  if (.Platform$r_arch == "i386")   download.file(url = "http://cygwin.org/setup-x86.exe",    destfile = dstfile, quiet = TRUE, mode = "wb")
  #
  if (!file.exists(dstfile))         stop("Download failed. Please install manually.")
  if (file.size(dstfile) < 5*10 ^ 5) stop("Download seem to have failed - file too small. Please install manually.")
  #
  # find and use proxy settings. seems only needed for install because download.file() respects system settings.
  # so far only simplest case covered, $AutoConfigURL not yet implemented
  tmp <- utils::readRegistry('Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings', hive = "HCU")
  if (proxy != "") {
    # manual setting overrides all
    proxy <- paste0('--proxy ', proxy)
  } else {
    # detect if any proxy to be used
    if (tmp$ProxyEnable == 1) {
      # automatically or manually configured?
      if (tmp$AutoConfigURL != "") {
        # retrieve settings
        proxypacfile <- paste0(tmpfile, 'pacfile.txt')
        download.file(tmp$AutoConfigURL, proxypacfile)
        # for testing: proxypacfile <- "private/proxypacfile"
        # find out and select last mentioned proxy line
        proxypac <- readLines(proxypacfile)
        proxypac <- proxypac[grepl('PROXY', proxypac)]
        proxypac <- proxypac[length(proxypac)]
        proxy <- sub('.* PROXY ([0-9]+.[0-9]+.[0-9]+.[0-9]+:[0-9]+).*', '\\1', proxypac)
        if (proxy == '') stop('A proxy could not be identified from the automatic configuration script used by the system. Please set manually a proxy = "host_or_ip:port"')
      } else {
        proxy <- paste0('--proxy ', tmp$ProxyServer)
      }
    }
  }
  #
  # compose installation command
  installcmd <- "--no-admin --quiet-mode --verbose --site http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages perl"
  #
  # first change to temporary directory, then execute command
  system(paste0(dstfile, " ", installcmd, " --local-package-dir ", tmpfile, ' ', proxy))
  #
  # test cygwin installation
  ctrdata::testCygwin()
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


#' Convenience function to find location of mongo database binaries (mongo, mongoimport)
#'
#' Sets variable \code{mongoBinaryLocation} in the user's global environment for access by other functions of the package.
#'
#' @param mongoDirWin Only used under MS Windows: folder that contains mongo binaries, defaults to "c:\\mongo\\bin\\"
#' as used on \url{http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows#interactive-installation}
#' @return Either an empty string if \code{mongoimport} was found on the path or, under MS Windows,
#' a string representing the path to the folder of the mongo binaries
#' @export findMongo
#'
findMongo <- function(mongoDirWin = "c:\\mongo\\bin\\") {
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
    # check folder specified in parameter
    mongoDirWin <- gsub("[\\]*$", "\\\\", mongoDirWin)
    #
    if ((.Platform$OS.type == "windows") && (file.exists(paste0(mongoDirWin, 'mongoimport.exe')))) {
      #
      assign("mongoBinaryLocation", mongoDirWin, envir = .GlobalEnv)
      message("mongoimport / mongo is in ", mongoDirWin)
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
        message("mongoimport / mongo found in ", location)
        invisible(location)
        #
      }
    }
  }
}


#' Check the version of the build of the mongo server to be used
#'
#' In addition to the returned value, the function will generate a warning message if applicable.
#'
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#'
#' @return A logical value indicating if the mongodb version is acceptable for use with this package.
#' @export checkMongoVersionOk
#'
checkMongoVersionOk <- function(mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users")) {
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
    warning("mongodb not version 3. Earlier versions have limitations that may break function getCTRdata() in package ctrdata.\n Please upgrade, see http://docs.mongodb.org/manual/installation/. \n Trying to continue. Support for versions other than 3 may be discontinued.", immediate. = TRUE)
    return(FALSE)
  }
  #
}



#' Merge related variables into a single variable, and optionally map values to a new set of values.
#'
#' @param df A data frame in which there are two variables (columns) to be merged into one.
#' @param varnames A vector with names of the two variables to be merged.
#' @param levelslist A list with one slice each for a new value to be used for a vector of old values.
#'
#' @return A vector of strings
#' @export mergeVariables
#' @examples
#' \dontrun{
#' statusvalues <- list("ongoing" = c("Recruiting", "Active", "Ongoing", "Active, not recruiting", "Enrolling by invitation"),
#'                      "completed" = c("Completed", "Prematurely Ended", "Terminated"),
#'                      "other" = c("Withdrawn", "Suspended", "No longer available", "Not yet recruiting"))
#' mergeVariables(result, c("Recruitment", "x5_trial_status"), statusvalues)
#' }
#'
mergeVariables <- function(df = NULL, varnames = "", levelslist = NULL) {
  #
  if (class(df) != "data.frame")   stop("Need a data frame as input.")
  if (length(varnames)  != 2)      stop("Please provide exactly two variable names.")

  # find variables in data frame and merge
  tmp <- match(varnames, names(df))
  tmp <- df[, tmp]
  tmp1 <- ifelse(is.na(tt <- tmp[ ,1]), "", tt)
  tmp2 <- ifelse(is.na(tt <- tmp[ ,2]), "", tt)
  tmp <- paste0(tmp1, tmp2)

  # inform user on levels found
  message("Unique values found in the new variable: ", paste(unique(tmp), collapse = ", "))

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

  return(tmp)
}








