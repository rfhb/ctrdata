## RH 2019-09-28

#### SETUP ####

source("test_ctrdata.R")

if (!check_sqlite())   exit_file("Not available: SQLite")
if (!check_internet()) exit_file("Not available: internet connectivity")

# create database object
dbc <- nodbi::src_sqlite(
  dbname = ":memory:",
  collection = mongo_local_rw_collection)

#### CTGOV ####

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "CTGOV",
      con = dbc)),
  "Imported or updated 1 trial")

# test
expect_equal(tmp_test$n, 1L)

# test
expect_equal(tmp_test$success, "NCT01471782")

# test
expect_true(length(tmp_test$failed) == 0L)

#### update ####

# new query
q <- paste0("https://clinicaltrials.gov/ct2/results?",
            "term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = paste0(q, "12%2F31%2F2008"),
      con = dbc)),
  "Imported or updated ")

# manipulate history to test updating
# implemented in dbCTRUpdateQueryHistory
hist <- suppressWarnings(dbQueryHistory(con = dbc))
hist[nrow(hist), "query-term"] <-
  sub("(.*&lup_e=).*", "\\112%2F31%2F2009", hist[nrow(hist), "query-term"])

# convert into json object
json <- jsonlite::toJSON(list("queries" = hist))

# update database
nodbi::docdb_update(
  src = dbc,
  key = dbc$collection,
  value = data.frame("_id" = "meta-info",
                     "content" = as.character(json),
                     stringsAsFactors = FALSE,
                     check.names = FALSE))

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc)),
  "Imported or updated")

# test
expect_true(tmp_test$n > 2L)

# test
expect_true(length(tmp_test$success) > 2L)

# test
expect_true(length(tmp_test$failed) == 0L)

#### results ####

# get results
result <- suppressWarnings(
  dbGetFieldsIntoDf(
    fields = c(
      "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
      "clinical_results.baseline.group_list.group",
      "clinical_results.baseline.analyzed_list.analyzed.units",
      "study_design_info.allocation",
      "location"),
    con = dbc))

result$number_sites <- sapply(
  result$location,
  function(x) length(x[["facility"]][["name"]]))

# test
expect_true(sum(result$number_sites, na.rm = TRUE) > 30L)

# test
expect_true("character" == class(result[[
  "clinical_results.baseline.analyzed_list.analyzed.count_list.count"]]))

# test
expect_true("list" == class(result[[
  "clinical_results.baseline.group_list.group"]]))

# test
expect_true(
  sum(nchar(getSublistKey(
    result,
    list(c("location", "name"))
  )),
  na.rm = TRUE) > 1000L)


#### close ####
RSQLite::dbDisconnect(conn = dbc$con)


