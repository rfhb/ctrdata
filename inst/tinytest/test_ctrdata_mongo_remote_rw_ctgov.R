## RH 2019-09-28

#### SETUP ####
if(!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

if (!check_mongo_remote_rw()) exit_file("Reason: no remote rw MongoDB")
if (!check_internet())        exit_file("Reason: no internet connectivity")
if (!check_binaries())        exit_file("Reason: no binaries php or sed or perl")

# create database object
dbc <- nodbi::src_mongo(
  db = mongo_remote_rw_db,
  collection = mongo_remote_rw_collection,
  url = mongo_remote_rw_url)

#### CTGOV ####
source("ctrdata_ctgov.R", local = TRUE)

#### close ####
try({
  dbc$con$drop()
  dbc$con$disconnect()
},
silent = TRUE)

