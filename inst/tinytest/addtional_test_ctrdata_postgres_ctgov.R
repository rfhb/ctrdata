## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkPostgres()) exit_file("Reason: no PostgreSQL")
if (!checkInternet()) exit_file("Reason: no internet connectivity")
if (!checkBinaries()) exit_file("Reason: no binaries php or sed or perl")

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
  if (httr::status_code(
    httr::GET("https://clinicaltrials.gov/ct2/search",
              httr::timeout(5))) != 200L
  ) return(exit_file("Reason: CTGOV not working"))

  # do tests
  source("ctrdata_ctgov.R", local = TRUE)

}
tf()
