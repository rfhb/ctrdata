## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkPostgres()) exit_file("Reason: no PostgreSQL")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

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
  httr::set_config(httr::timeout(60L))
  if (httr::status_code(
    httr::HEAD("https://euclinicaltrials.eu/data-protection-and-privacy/")) != 200L
  ) return(exit_file("Reason: ISRCTN not working"))

  # do tests
  source("ctrdata_ctis.R", local = TRUE)

}
tf()
