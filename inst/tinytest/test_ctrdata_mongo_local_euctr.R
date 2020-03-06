## RH 2019-09-28

#### SETUP ####
if(!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!check_mongo_local()) exit_file("Reason: no local MongoDB")
if (!check_internet())    exit_file("Reason: no internet connectivity")
if (!check_binaries())    exit_file("Reason: no binaries php or sed or perl")

# create database object
dbc <- nodbi::src_mongo(
  db = mongo_local_rw_db,
  collection = mongo_local_rw_collection,
  url = "mongodb://localhost")

#### CTGOV ####
source("ctrdata_euctr.R", local = TRUE)

#### close ####
try({
  dbc$con$drop()
  dbc$con$disconnect()
},
silent = TRUE)

