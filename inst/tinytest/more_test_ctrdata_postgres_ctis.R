## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkPostgres()) exit_file("Reason: no PostgreSQL")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

#### ISRCTN ####
tf <- function() {

  # test
  expect_error(
    dbQueryHistory(
      nodbi::src_postgres()
    ),
    "Specify attribute"
  )

  # create database object
  dbc <- nodbi::src_postgres()
  dbc[["collection"]] <- mongoLocalRwCollection

  # register clean-up
  on.exit(expr = {
    try({
      RPostgres::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RPostgres::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  }, add = TRUE)

  # do tests
  source("ctrdata_ctis.R", local = TRUE)

}

# check server
testUrl <- "https://euclinicaltrials.eu/ctis-public/search"
testGet <- function() try(httr::HEAD(testUrl, httr::timeout(10L)), silent = TRUE)
testOnce <- testGet()

if (inherits(testOnce, "try-error") ||
    httr::status_code(testOnce) != 200L
) return(exit_file("Reason: CTIS not working"))

# test
tf()
