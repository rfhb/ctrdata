## RH 2019-09-28

# testing in local development directory:
# tinytest::test_all()

# testing of built and installed package:
# tinytest::test_package("ctrdata")

# functions for testing
# to be sourced by test_*.R

# there is no testing done in this file,
# only helper functions and global variables

#### setup ####
library(tinytest)

suppressMessages(library(ctrdata))

#### helper functions for data ####

getSublistKey <- function(fulllist,
                          keyssublists =
                            list(c("endPoints.endPoint", "^title"))) {

  # dots needs to be defined because passing it in .Internal(mapply()) is not enough
  dots <- lapply(
    keyssublists,
    function(k) lapply(fulllist[[k[1]]], # k[1] = "endPoints.endPoint" identifies the sublist
                       function(l) extractKey(unlist(l, recursive = TRUE),
                                              k[2]) # k[2] = "^title" identifies the key in the sublist
    ))

  # add trial identifer if available
  if ("_id" %in% names(fulllist)) dots <- c("_id" = list(fulllist[["_id"]]), dots)

  # do the magic, which is expanding the data.frame
  # by recycling shorter vectors to align with data
  out <- .Internal(mapply(cbind, dots, NULL))
  out <- do.call(rbind, out)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  row.names(out) <- NULL

  # make names from names of keys and of sublists
  no <- sapply(keyssublists, paste0, collapse = ".")
  no <- gsub("[^a-zA-Z.]", "", no)
  names(out)[-1] <- no

  # return
  out
}

extractKey <- function(flattenedList, key) {

  # extract value for key
  extracted <- flattenedList[ grepl(key, names(flattenedList), ignore.case = TRUE) ]

  # if key is not found, return a value
  # e.g. missing value (NA) or empty string ("")
  # please change as wanted for later processing
  if (length(extracted) == 0) extracted <- NA

  # return
  return(extracted)
}

getNames <- function(thevector) {

  sort(unique(sub("[0-9]+$", "", names(thevector))))

}


#### global variables for data bases ####
tmpname <- paste0("ctrdata_tinytest_",
                  format(Sys.time(),
                         "%Y%m%d%H%M%S",
                         tz = "UTC"))

# local mongodb
mongo_local_rw_collection <- tmpname
mongo_local_rw_db         <- tmpname

# remote mongodb read only
mongo_remote_ro_collection <- "dbperm"
mongo_remote_ro_db         <- "dbperm"

# remote mongodb read write
mongo_remote_rw_collection <- tmpname
mongo_remote_rw_db         <- tmpname

# credentials

mongo_remote_ro_url <- "mongodb+srv://DWbJ7Wh:bdTHh5cS@cluster0-b9wpw.mongodb.net/"
# permissions are restricted to "find" in "dbperm" in "dbperm"
# no other functions can be executed, no login possible

mongo_remote_rw_url <- Sys.getenv(x = "ctrdatamongouri")
# this environment variable is set on development
# and continuous integration systems



#### helper functions system detection ####

check_internet <- function(){

  curl::has_internet()

}


check_sqlite <- function(){

  tmp <- try(nodbi::src_sqlite(), silent = TRUE)

  on.exit(RSQLite::dbDisconnect(conn = tmp$con))

  all(c("src_sqlite", "docdb_src") %in% class(tmp))

}


check_mongo_local <- function(){

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpname,
      db = tmpname,
      url = "mongodb://localhost"),
    silent = TRUE)

  on.exit(tmp$con$disconnect())

  all(c("src_mongo", "docdb_src") %in% class(tmp))

}


check_mongo_remote_ro <- function(){

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpname,
      db = tmpname,
      url = mongo_remote_ro_url),
    silent = TRUE)

  on.exit(tmp$con$disconnect())

  all(c("src_mongo", "docdb_src") %in% class(tmp))

}

check_mongo_remote_rw <- function(){

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpname,
      db = tmpname,
      url = mongo_remote_rw_url),
    silent = TRUE)

  on.exit(tmp$con$disconnect())

  all(c("src_mongo", "docdb_src") %in% class(tmp))

}

