## RH 2022-10-30

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkDuckdb())   exit_file("Reason: no DuckDB")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

#### ISRCTN ####
tf <- function() {

  # test
  expect_error(
    dbQueryHistory(
      nodbi::src_duckdb(
        dbdir = ":memory:")),
    "Specify parameter"
  )

  # create database object
  dbc <- suppressWarnings(nodbi::src_duckdb(
    dbdir = ":memory:",
    collection = mongoLocalRwCollection))

  # register clean-up
  on.exit(expr = {
    try({
      if (DBI::dbExistsTable(conn = dbc$con, name = dbc$collection))
        DBI::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      duckdb::dbDisconnect(dbc$con, shutdown = TRUE)
      rm(dbc)
    },
    silent = TRUE)
  }, add = TRUE)

  # do tests
  source("ctrdata_isrctn.R", local = TRUE)

}

# check server
if (inherits(try(suppressWarnings(
  ctrLoadQueryIntoDb(
    "https://www.isrctn.com/search?q=neuroblastoma",
    only.count = TRUE)),
  silent = TRUE),
  "try-error")) {
  exit_file("Reason: ISRCTN not working")
}

# run testfile
tf()
