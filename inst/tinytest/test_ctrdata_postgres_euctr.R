## RH 2019-09-28

#### SETUP ####
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!checkPostgres()) exit_file("Reason: no PostgreSQL")
if (!checkInternet()) exit_file("Reason: no internet connectivity")
if (!checkBinaries()) exit_file("Reason: no binaries php or sed or perl")

# create database object
dbc <- nodbi::src_postgres()
dbc[["collection"]] <- mongoLocalRwCollection

#### ISRCTN ####
tf <- function() {
  # register clean-up
  on.exit(expr = {
    try({
      RPostgres::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RPostgres::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  })
  # do tests
  source("ctrdata_euctr.R", local = TRUE)
}
tf()
