## RH 2022-10-30

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkDuckdb())   exit_file("Reason: no DuckDB")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

#### EUCTR ####
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

  # check certificates
  if (inherits(testOnce, "try-error") &&
      grepl("SSL certificate.*local issuer certificate", testOnce)) {
    # message("Switching off certificate verification")
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
    testOnce <- testGet()
  }

  # do tests
  source("ctrdata_euctr.R", local = TRUE)

}

# check server
testUrl <- "https://www.clinicaltrialsregister.eu/ctr-search/search"
testGet <- function() try(httr::HEAD(testUrl, httr::timeout(10L)), silent = TRUE)
testOnce <- testGet()
if (inherits(testOnce, "try-error") ||
    httr::status_code(testOnce) != 200L
) return(exit_file("Reason: EUCTR not working"))

# test
tf()
