## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkSqlite())   exit_file("Reason: no SQLite")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

#### CTGOV ####
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

  # do tests
  source("ctrdata_ctgov2.R", local = TRUE)

}

# check server
if (httr::status_code(
  httr::HEAD("https://www.clinicaltrials.gov/",
             httr::timeout(10L))) != 200L
) return(exit_file("Reason: CTGOV2 not working"))

# test
tf()
