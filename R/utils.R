### ctrdata package
### utility functions


## global variable definitions
#
# EUCTR definitions
agegroupsEUCTR <- c ("Preterm newborn infants", "Newborns", "Infants and toddlers", "Children", "Adolescents",
                     "Under 18", "Adults", "Elderly")
variablesEUCTR <- c ("EudraCT Number", "Sponsor Protocol Number", "Sponsor Name", "Full Title", "Start Date",
                     "Medical condition", "Disease", "Population Age", "Gender", "Trial protocol", "Link")
countriesEUCTR <- c ("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT",
                     "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB")


#' Open advanced search pages of register(s) in default web browser.
#'
#' @param register Register(s) to open. Either "EUCTR" or "CTGOV" or a vector of both. Default is to open both registers' advanced search pages.
#' @param copyright If set to \code{TRUE}, opens copyright pages of register.
#' @param ... Any additional parameter to use with browseURL, which is called by this function.
#' @export openCTRWebBrowser
#' @return Is always true, invisibly.
#'
openCTRWebBrowser <- function (register = c("EUCTR"), copyright = FALSE, ...) {
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
getCTRQueryUrl <- function (content = clipr::read_clip()) {
  #
  if(length(content) == 0) {
    warning("System clipboard contained no readable text. Returning NULL.\n")
    return(NULL)
  }
  #
  # TODO change to use switch in prepration for other registers
  if(grepl("https://www.clinicaltrialsregister.eu/ctr-search/", content) |
     grepl("https://clinicaltrials.gov/ct2/results", content) ) {
    #
    queryterm <- sub ("https://www.clinicaltrialsregister.eu/ctr-search/search[?]query=(.*)", "\\1", content)
    queryterm <- sub ("https://clinicaltrials.gov/ct2/results[?]term=(.*)", "\\1", content)
    #
    return(queryterm)
  }
  else {
    warning(paste ("System clipboard content is not a clinical trial register search URL. Returning NULL. Clipboard content: ", content, "\n"))
    return(NULL)
  }
}


#' Find names of keys (fields) in the data base
#'
#' @param namepart A plain string (not a regular expression) to be searched for among all field names (keys) in the database.
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata"
#' @param allmatches If \code{TRUE}, returns all keys if more than one is found (default is \code{FALSE})
#' @return Vector of first keys (fields) found (or of all keys, see above)
#' @import rmongodb curl
#' @export findCTRkey
#' @examples
#' \dontrun{
#'  findCTRkey ("other")
#' }
#'
findCTRkey <- function (namepart = "id",
                        mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata",
                        allmatches = TRUE) {
  #
  if (!is.atomic(namepart)) stop("Name part should be atomic.\n")
  if (length(namepart) > 1) stop("Name part should only have one element.\n")
  if (namepart == "")       stop("Empty name part string.\n")
  #
  # check if database with variety results exists
  if (!(paste0(ns, "Keys") %in% mongo.get.database.collections(mongo, db = attr(mongo, "db")))) {
    #
    # check if extension is available, if not load it
    varietylocalurl <- system.file("exec/variety.js", package = "ctrdata")
    if (varietylocalurl == "") {
      cat("Downloading variety.js and adding keys to data base ...\n")
      varietysourceurl <- "https://raw.githubusercontent.com/variety/variety/master/variety.js"
      curl::curl_download(varietysourceurl, paste0(system.file("", package = "ctrdata"), "exec/variety.js"))
    }
    #
    varietymongo <- paste0("mongo ", attr(mongo, "db"),
                           ifelse (attr(mongo, "username") != "", paste0(" --username ", attr(mongo, "username")), ""),
                           ifelse (attr(mongo, "password") != "", paste0(" --password ", attr(mongo, "password")), ""),
                           " --eval \"var collection = '", ns, "', persistResults=true\" variety.js")
    tmp <- system(paste0(varietymongo), intern = TRUE)
    #
  }
  #
  # mongo get fieldnames
  # TODO avoid data.frame = TRUE
  tmp <- rmongodb::mongo.find.all(mongo, paste0("varietyResults", ".", ns, "Keys"), fields = list("key"=1L), data.frame = TRUE)
  fieldnames <- tmp[,1]
  #
  # return the first match / all matches
  fieldname <- fieldnames[grep(tolower(namepart), tolower(fieldnames))]
  fieldname <- ifelse (allmatches, fieldname, fieldname[1])
  #
  return(fieldname)
  #
}


#' Convert a mongo query result object into a data frame
#'
#' @param x A result of object of a mongo query TODO create link
#' @return A data frame with the data that was stored in the input object
#' @import rmongodb
#' @export mongo2df
#'
mongo2df <- function (x) {
  xclass <- class (x)
  #
  if(xclass == "mongo.bson") {
    tmp <- rmongodb::mongo.bson.to.list(x)
    df <- data.frame(do.call(rbind, tmp))
    df <- df [2 : nrow(df), ]
    return(df)
  }
  if(xclass == "mongo.cursor") {
    df <- rmongodb::mongo.cursor.to.data.frame(x)
    return(df)
  }
  # other type of object
  warning("Could not use input, not a bson or mongo cursor.\n")
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
uniquetrialsCTRdata <- function (mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
  #
  stop("uniquetrialsCTRdata: function is not yet completely implemented.\n")
  #
  # CTGOV: "Other IDs" has been split into the indexed array "otherids"
  listofCTGOVids <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns),
                                             query  = list('otherids' = list('$regex' = 'EUDRACT-.*')),
                                             fields = list("otherids"=1L))
  # extract _ids of records that have a EUCTR / EudraCT number
  listofCTGOVids <- sapply(listofCTGOVids, "[[", "_id")
  #
  # EUCTR / EudraCT number is "_id" for EUCTR records
  listofEUCTRids <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"),
                                             query  = list('_id' = list('$regex' = '[0-9]{4}-[0-9]{6}-[0-9]{2}-[A-Z]{2}')),
                                             fields = list("_id"=1L))
  listofEUCTRids <- sapply(listofEUCTRids, "[[", "_id")
  #
  dupes   <- listofEUCTRids %in% listofCTGOVids
  uniques <- listofEUCTRids[!dupes]
  numres  <- as.data.frame(table(dupes))
  #
  cat(paste0("Total ", numres$Freq[2], " duplicates found, returning keys (_id) of ", numres$Freq[1], " records.\n"))
  #
  return(res)
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
dbCTRGet <- function (fields = "", mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
  if (!is.vector(fields) | class(fields)!="character") stop("Input should just be a vector of strings of field names.\n")
  #
  countall <- rmongodb::mongo.count(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"))
  #
  q <- sapply(fields, function (x) list (list ('$gt' = '')))
  f <- sapply(fields, function (x) list (2L))
  #
  result <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"),
                                     query = q, fields = f, data.frame = TRUE)
  #
  if (countall > nrow(result)) warning(paste0(countall - nrow(result), " of ", countall,
                                              " records dropped as one or more of the specified fields were not found.\n"))
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
dbCTRGetAll <- function (mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata") {
  #
  result <- rmongodb::mongo.find.all(mongo, paste0(attr(mongo, "db"), ".", ns = "ctrdata"), data.frame = TRUE)
  #
  return(result)
}

