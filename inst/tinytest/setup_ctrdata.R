## RH 2019-09-28

# functions for testing
# to be sourced by test_*.R

# there is no testing done in this file,
# only helper functions and global variables

#### setup ####

library(tinytest)
suppressPackageStartupMessages(library(ctrdata))


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

mongo_remote_ro_url <-
  "mongodb+srv://DWbJ7Wh:bdTHh5cS@cluster0-b9wpw.mongodb.net/"
# permissions are restricted to "find" in "dbperm" in "dbperm"
# no other functions can be executed, no login possible

mongo_remote_rw_url <-
  Sys.getenv(x = "ctrdatamongouri")
# this environment variable is set on development
# and continuous integration systems


#### helper functions system detection ####

check_binaries <- function(){

  out <- TRUE

  if (.Platform$OS.type == "windows") {
    if (!suppressMessages(ctrdata:::installCygwinWindowsTest())) {
      out <- FALSE
    }
  }

  out &&

    suppressWarnings(ctrdata:::installFindBinary(
      commandtest = "php --version")) &&

    suppressWarnings(ctrdata:::installFindBinary(
      commandtest = "php -r 'simplexml_load_string(\"\");'")) &&

    suppressWarnings(ctrdata:::installFindBinary(
      commandtest = "php -r 'json_encode(\"<foo>\");'")) &&

    suppressWarnings(ctrdata:::installFindBinary(
      commandtest = "echo x | sed s/x/y/")) &&

    suppressWarnings(ctrdata:::installFindBinary(
      commandtest = "perl -V:osname"))

}


check_internet <- function(){

  tmp <- try({
    httr::HEAD("www.clinicaltrials.gov", httr::timeout(5))
    httr::HEAD("www.clinicaltrialsregister.eu", httr::timeout(5))
  }, silent = TRUE)

  out <- !("try-error" %in% class(tmp))
  out

}


check_sqlite <- function(){

  tmp <- try(nodbi::src_sqlite(), silent = TRUE)

  out <- all(c("src_sqlite", "docdb_src") %in% class(tmp))
  if (out) RSQLite::dbDisconnect(conn = tmp$con)
  out

}


check_mongo_local <- function(){

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpname,
      db = tmpname,
      url = "mongodb://localhost"),
    silent = TRUE)

  out <- all(c("src_mongo", "docdb_src") %in% class(tmp))
  if (out) tmp$con$disconnect()
  out

}


check_mongo_remote_ro <- function(){

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpname,
      db = tmpname,
      url = mongo_remote_ro_url),
    silent = TRUE)

  out <- all(c("src_mongo", "docdb_src") %in% class(tmp))
  if (out) tmp$con$disconnect()
  out

}

check_mongo_remote_rw <- function(){

  tmp <- try(
    nodbi::src_mongo(
      collection = tmpname,
      db = tmpname,
      url = mongo_remote_rw_url),
    silent = TRUE)

  out <- all(c("src_mongo", "docdb_src") %in% class(tmp))
  if (out) tmp$con$disconnect()
  out
}

