## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkMongoLocal()) exit_file("Reason: no local MongoDB")
if (!checkInternet())   exit_file("Reason: no internet connectivity")
if (!checkBinaries())   exit_file("Reason: no binaries sed or perl")

#### EUCTR ####
tf <- function() {

  # create database object
  dbc <- nodbi::src_mongo(
    db = mongoLocalRwDb,
    collection = mongoLocalRwCollection,
    url = "mongodb://localhost")

  # register clean-up
  on.exit(expr = {
    try({
      dbc$con$drop()
      dbc$con$disconnect()
    },
    silent = TRUE)
  })

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

  # clean up
  rm(testUrl, testGet, testOnce)

  # do tests
  source("ctrdata_euctr.R", local = TRUE)
}
tf()
