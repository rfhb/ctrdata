## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkMongoRemoteRw()) exit_file("Reason: no remote rw MongoDB")
if (!checkInternet())      exit_file("Reason: no internet connectivity")

#### EUCTR ####
tf <- function() {

  # create database object
  dbc <- nodbi::src_mongo(
    db = mongoRemoteRwDb,
    collection = mongoRemoteRwCollection,
    url = mongoRemoteRwUrl)

  # register clean-up
  on.exit(expr = {
    try({
      dbc$con$drop()
      dbc$con$disconnect()
    },
    silent = TRUE)
  })

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
