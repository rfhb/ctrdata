## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkMongoRemoteRw()) exit_file("Reason: no remote rw MongoDB")
if (!checkInternet())      exit_file("Reason: no internet connectivity")

#### CTGOV ####
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
  source("ctrdata_ctgov.R", local = TRUE)

}

# check server
if (httr::status_code(
  httr::HEAD("https://classic.clinicaltrials.gov/ct2/search",
             httr::timeout(10L))) != 200L
) return(exit_file("Reason: CTGOV not working"))

# test
tf()
