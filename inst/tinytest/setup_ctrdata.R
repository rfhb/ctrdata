## RH 2019-09-28

# functions for testing
# to be sourced by test_*.R

# there is no testing done in this file,
# only helper functions and global variables

#### setup ####

library(tinytest)
suppressPackageStartupMessages(library(ctrdata))

# test with dplyr
if (any("tibble" == .packages()))
  suppressPackageStartupMessages(library(tibble))

# throw error for && and || with vectors longer than 1 element
Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "TRUE")

# throw error for if statement with length > 1 vector
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "TRUE")


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
mongoRemoteRoUrl <- Sys.getenv(x = "CTRDATAMONGOURIRO")
mongoRemoteRwUrl <- Sys.getenv(x = "CTRDATAMONGOURIRW")


#### helper functions system detection ####

checkInternet <- function() {
  tmp <- try(
    curl::nslookup("r-project.org"),
    silent = TRUE
  )

  out <- !inherits(tmp, "try-error")
  out
}

checkSqlite <- function() {
  tmp <- try(nodbi::src_sqlite(), silent = TRUE)
  out <- inherits(tmp, c("src_sqlite", "docdb_src"))
  if (out) RSQLite::dbDisconnect(conn = tmp$con)
  out
}

checkDuckdb <- function() {
  !inherits(try(
    duckdb::dbDisconnect(nodbi::src_duckdb()$con, shutdown = TRUE),
    silent = TRUE), "try-error")
}

checkPostgres <- function() {
  tmp <- try(nodbi::src_postgres(), silent = TRUE)
  out <- inherits(tmp, c("src_postgres", "docdb_src"))
  if (out) RPostgres::dbDisconnect(conn = tmp$con)
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

  out <- inherits(tmp, c("src_mongo", "docdb_src"))
  if (out) tmp$con$disconnect()
  out
}


checkMongoRemoteRo <- function() {

  if (mongoRemoteRoUrl == "") return(FALSE)

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpName,
      db = tmpName,
      url = mongoRemoteRoUrl
    ),
    silent = TRUE
  )

  out <- inherits(tmp, c("src_mongo", "docdb_src"))
  if (out) tmp$con$disconnect()
  out
}

checkMongoRemoteRw <- function() {

  if (mongoRemoteRwUrl == "") return(FALSE)

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpName,
      db = tmpName,
      url = mongoRemoteRwUrl
    ),
    silent = TRUE
  )

  out <- inherits(tmp, c("src_mongo", "docdb_src"))
  if (out) tmp$con$disconnect()
  out
}

newTempDir <- function() {

  d <- tempfile()
  dir.create(d)
  d

}
