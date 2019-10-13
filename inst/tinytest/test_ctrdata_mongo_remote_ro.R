## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

if (!check_internet())        exit_file("Not available: internet connectivity")
if (!check_mongo_remote_ro()) exit_file("Not available: remote ro MongoDB")

# create database object
dbc <- nodbi::src_mongo(
  db = mongo_remote_ro_db,
  collection = mongo_remote_ro_collection,
  url = mongo_remote_ro_url)

# database has been pre-loaded
# and can only be accessed as
# read-only, static collection

#### CTGOV and EUCTR ####

# test
expect_message(
  suppressWarnings(
    dbFindFields(
      namepart = "date",
      con = dbc)),
  "Finding fields")

# test
tmp <- dbGetFieldsIntoDf(
  fields = c(
    "a2_eudract_number",
    "overall_status",
    "record_last_import",
    "primary_completion_date",
    "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
    "study_design_info.intervention_model",
    "e71_human_pharmacology_phase_i"),
  con = dbc)

# tests
expect_equal(dim(tmp)[2], 8)

# test
expect_true("POSIXct" %in%
              class(tmp[["record_last_import"]]))

# test
expect_true("character" %in%
              class(tmp[["study_design_info.intervention_model"]]))


#### close ####
dbc$con$disconnect()
