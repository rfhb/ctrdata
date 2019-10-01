## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

if (!check_sqlite())   exit_file("Not available: SQLite")
if (!check_internet()) exit_file("Not available: internet connectivity")

# create database object
dbc <- nodbi::src_sqlite(
  dbname = ":memory:",
  collection = mongo_local_rw_collection)

#### CTGOV ####
source("ctrdata_ctgov.R", local = TRUE)

#### close ####
RSQLite::dbRemoveTable(conn = dbc$con, name = dbc$collection)
RSQLite::dbDisconnect(conn = dbc$con)
