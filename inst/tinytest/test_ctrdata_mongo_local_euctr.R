## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

if (!check_mongo_local()) exit_file("Not available: local MongoDB")
if (!check_internet())    exit_file("Not available: internet connectivity")
if (!check_binaries())    exit_file("Not available: binaries php or sed or perl")

# create database object
dbc <- nodbi::src_mongo(
  db = mongo_local_rw_db,
  collection = mongo_local_rw_collection,
  url = "mongodb://localhost")

#### CTGOV ####
source("ctrdata_euctr.R", local = TRUE)

#### close ####
dbc$con$drop()
dbc$con$disconnect()

