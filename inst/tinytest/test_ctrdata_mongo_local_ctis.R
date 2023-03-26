## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkMongoLocal()) exit_file("Reason: no local MongoDB")
if (!checkInternet())   exit_file("Reason: no internet connectivity")

#### ISRCTN ####
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
  httr::set_config(httr::timeout(60L))
  if (httr::status_code(
    httr::HEAD("https://euclinicaltrials.eu/data-protection-and-privacy/")) != 200L
  ) return(exit_file("Reason: CTIS not working"))

  # do tests
  source("ctrdata_ctis.R", local = TRUE)
}
tf()
