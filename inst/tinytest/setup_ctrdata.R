## RH 2019-09-28

# functions for testing
# to be sourced by test_*.R

# there is no testing done in this file,
# only helper functions and global variables

#### setup ####

library(tinytest)
suppressMessages(library(ctrdata))


#### global variables for data bases ####

tmpName <- paste0(
  "ctrdata_test_",
  format(Sys.time(),
    "%Y%m%d%H%M%S",
    tz = "UTC"
  ), "_",
  paste0(sample(letters, 6), collapse = "")
)

# local mongodb
mongoLocalRwCollection <- tmpName
mongoLocalRwDb <- "users"

# remote mongodb read only
mongoRemoteRoCollection <- "dbperm"
mongoRemoteRoDb <- "dbperm"

# remote mongodb read write
mongoRemoteRwCollection <- tmpName
mongoRemoteRwDb <- "users"

# credentials

mongoRemoteRoUrl <-
  "mongodb+srv://DWbJ7Wh:bdTHh5cS@cluster0-b9wpw.mongodb.net/"
# permissions are restricted to "find" in "dbperm" in "dbperm"
# no other functions can be executed, no login possible

mongoRemoteRwUrl <-
  Sys.getenv(x = "ctrdatamongouri")
# this environment variable is set on development
# and continuous integration systems


#### helper functions system detection ####

checkBinaries <- function() {
  all(ctrdata:::checkBinary())
}


checkInternet <- function() {
  tmp <- try(
    httr::HEAD("https://httpbin.org/anything", httr::timeout(5)),
    silent = TRUE
  )

  out <- !("try-error" %in% class(tmp))
  out
}


checkSqlite <- function() {
  tmp <- try(nodbi::src_sqlite(), silent = TRUE)

  out <- all(c("src_sqlite", "docdb_src") %in% class(tmp))
  if (out) RSQLite::dbDisconnect(conn = tmp$con)
  out
}


checkMongoLocal <- function() {
  tmp <- try(
    nodbi::src_mongo(
      collection = tmpName,
      db = tmpName,
      url = "mongodb://localhost"
    ),
    silent = TRUE
  )

  out <- all(c("src_mongo", "docdb_src") %in% class(tmp))
  if (out) tmp$con$disconnect()
  out
}


checkMongoRemoteRo <- function() {
  tmp <- try(
    nodbi::src_mongo(
      collection = tmpName,
      db = tmpName,
      url = mongoRemoteRoUrl
    ),
    silent = TRUE
  )

  out <- all(c("src_mongo", "docdb_src") %in% class(tmp))
  if (out) tmp$con$disconnect()
  out
}

checkMongoRemoteRw <- function() {
  tmp <- try(
    nodbi::src_mongo(
      collection = tmpName,
      db = tmpName,
      url = mongoRemoteRwUrl
    ),
    silent = TRUE
  )

  out <- all(c("src_mongo", "docdb_src") %in% class(tmp))
  if (out) tmp$con$disconnect()
  out
}
