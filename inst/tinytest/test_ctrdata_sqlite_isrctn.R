## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkSqlite())   exit_file("Reason: no SQLite")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

#### ISRCTN ####
tf <- function() {
  
  # test
  expect_error(
    dbQueryHistory(
      nodbi::src_sqlite(
        dbname = ":memory:")
    ),
    "Specify parameter"
  )
  
  # create database object
  dbc <- suppressWarnings(nodbi::src_sqlite(
    dbname = ":memory:",
    collection = mongoLocalRwCollection))
  
  # register clean-up
  on.exit(expr = {
    try({
      if (DBI::dbExistsTable(conn = dbc$con, name = dbc$collection))
        DBI::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RSQLite::dbDisconnect(conn = dbc$con)
      rm(dbc)
    },
    silent = TRUE)
  }, add = TRUE)
  
  # check server
  if (inherits(try(suppressWarnings(
    ctrLoadQueryIntoDb(
      "https://www.isrctn.com/search?q=neuroblastoma", 
      only.count = TRUE)), 
    silent = TRUE), 
    "try-error")) {
    exit_file("Reason: ISRCTN not working")
  }
  
  # do tests
  source("ctrdata_isrctn.R", local = TRUE)
  
}
tf()
