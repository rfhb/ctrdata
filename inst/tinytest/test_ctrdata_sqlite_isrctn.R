## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkSqlite())   exit_file("Reason: no SQLite")
if (!checkInternet()) exit_file("Reason: no internet connectivity")
if (!checkBinaries()) exit_file("Reason: no binaries php or sed or perl")

#### ISRCTN ####
tf <- function() {

  # create database object
  dbc <- suppressWarnings(nodbi::src_sqlite(
    dbname = ":memory:",
    collection = mongoLocalRwCollection))

  # register clean-up
  on.exit(expr = {
    try({
      RSQLite::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RSQLite::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  }, add = TRUE)

  # check server
  httr::set_config(httr::timeout(seconds = 60))
  if (httr::status_code(
    httr::GET("https://www.isrctn.com/editAdvancedSearch")) != 200L
  ) return(exit_file("Reason: ISRCTN not working"))

  # do tests
  source("ctrdata_isrctn.R", local = TRUE)

}
tf()
