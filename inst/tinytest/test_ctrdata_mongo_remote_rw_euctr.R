## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

if (!check_mongo_remote_rw()) exit_file("Not available: remote rw MongoDB")
if (!check_internet())        exit_file("Not available: internet connectivity")
if (!check_binaries())        exit_file("Not available: php or sed or perl")

# create database object
dbc <- nodbi::src_mongo(
  db = mongo_remote_rw_db,
  collection = mongo_remote_rw_collection,
  url = mongo_remote_rw_url)

#### CTGOV ####
source("ctrdata_euctr.R", local = TRUE)

#### close ####
dbc$con$drop()
dbc$con$disconnect()



