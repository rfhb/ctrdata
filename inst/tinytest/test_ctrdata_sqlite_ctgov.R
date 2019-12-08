## RH 2019-09-28

#### SETUP ####
#if(!at_home()) exit_file("skipping")
source("setup_ctrdata.R")

if (!check_sqlite())   exit_file("Not available: SQLite")
if (!check_internet()) exit_file("Not available: internet connectivity")
if (!check_binaries()) exit_file("Not available: binaries php or sed or perl")

# create database object
dbc <- nodbi::src_sqlite(
  dbname = ":memory:",
  collection = mongo_local_rw_collection)

#### CTGOV ####
source("ctrdata_ctgov.R", local = TRUE)

#### close ####
try({
  RSQLite::dbRemoveTable(conn = dbc$con, name = dbc$collection)
  RSQLite::dbDisconnect(conn = dbc$con)
},
silent = TRUE)
