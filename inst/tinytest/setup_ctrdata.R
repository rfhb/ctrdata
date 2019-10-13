## RH 2019-09-28

# testing in local development directory:
# tinytest::test_all()

# testing of built and installed package:
# tinytest::test_package("ctrdata")

# options(tt.pr.passes = TRUE)

# functions for testing
# to be sourced by test_*.R

# there is no testing done in this file,
# only helper functions and global variables

#### setup ####

library(tinytest)
suppressMessages(library(ctrdata))


#### global variables for data bases ####

tmpname <- paste0("ctrdata_test_",
                  format(Sys.time(),
                         "%Y%m%d%H%M%S",
                         tz = "UTC"), "_",
                  paste0(sample(letters, 6), collapse = ""))

# local mongodb
mongo_local_rw_collection <- tmpname
mongo_local_rw_db         <- "users"

# remote mongodb read only
mongo_remote_ro_collection <- "dbperm"
mongo_remote_ro_db         <- "dbperm"

# remote mongodb read write
mongo_remote_rw_collection <- tmpname
mongo_remote_rw_db         <- "users"

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

