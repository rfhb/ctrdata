## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkPostgres()) exit_file("Reason: no PostgreSQL")
if (!checkInternet()) exit_file("Reason: no internet connectivity")
if (!checkBinaries()) exit_file("Reason: no binaries sed or perl")

#### ISRCTN ####
tf <- function() {

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

  # check server
  testUrl <- "https://www.clinicaltrialsregister.eu/ctr-search/search"
  testGet <- function() try(httr::HEAD(testUrl, httr::timeout(10L)), silent = TRUE)
  testOnce <- testGet()

  if (inherits(testOnce, "try-error") &&
      grepl("SSL certificate.*local issuer certificate", testOnce)) {
    # message("Switching off certificate verification")
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
    testOnce <- testGet()
  }
  if (inherits(testOnce, "try-error") ||
      httr::status_code(testOnce) != 200L
  ) return(exit_file("Reason: EUCTR not working"))

  # do tests
  source("ctrdata_euctr.R", local = TRUE)

}
tf()
