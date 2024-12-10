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
