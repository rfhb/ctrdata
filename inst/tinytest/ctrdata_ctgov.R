## RH 2019-09-28

# set server
httr::set_config(httr::timeout(seconds = 60))

#### ctrLoadQueryIntoDb ####

# test
expect_equal(
  suppressMessages(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "CTGOV",
      only.count = TRUE))[["n"]], 1L)

# test
nodbi::docdb_create(
  src = dbc,
  key = dbc$collection,
  value = data.frame()
)
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        querytoupdate = "last",
        con = dbc))),
  "no previous queries")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "SHOULDNOTEXISTATALL",
      register = "CTGOV",
      con = dbc)),
  "no.*trials found")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "CTGOV",
      con = dbc)),
  "Imported or updated 1 trial")

# test
expect_equal(tmpTest$n, 1L)

# test
expect_equal(tmpTest$success, "NCT01471782")

# test
expect_true(length(tmpTest$failed) == 0L)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "NCT01471782",
      register = "CTGOV",
      con = dbc)),
  "Imported or updated 1 trial")


# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = paste0(
          "https://classic.clinicaltrials.gov/ct2/results?cond=Cancer&type=Intr&phase=0",
          "&strd_s=01%2F02%2F2005&strd_e=12%2F31%2F2017"),
        con = dbc))),
  "more than 10,000) trials")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "someQueryForErrorTriggering",
      register = "CTGOV",
      verbose = TRUE,
      only.count = TRUE,
      con = dbc)),
  "term=someQueryForErrorTriggering")


#### ctrLoadQueryIntoDb update ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      verbose = TRUE,
      con = dbc)),
  "no.*trials found")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      forcetoupdate = TRUE,
      con = dbc)),
  "Imported or updated ")

# test
expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = 999L,
      con = dbc)),
  "'querytoupdate': specified query number.*not found")

# new query
q <- paste0("https://classic.clinicaltrials.gov/ct2/results?",
            "term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = paste0(q, "12%2F31%2F2008"),
      con = dbc)),
  "Imported or updated ")

# manipulate history to test updating
# implemented in dbCTRUpdateQueryHistory
hist <- suppressWarnings(dbQueryHistory(con = dbc))
hist[nrow(hist), "query-term"] <-
  sub("(.*&lup_e=).*", "\\112%2F31%2F2011", hist[nrow(hist), "query-term"])

# convert into json object
json <- jsonlite::toJSON(list("queries" = hist))

# update database
expect_equal(
  nodbi::docdb_update(
    src = dbc,
    key = dbc$collection,
    value = as.character(json),
    query = '{"_id": "meta-info"}'), 1L)

# test
tmpTest2 <- suppressWarnings(
  dbQueryHistory(con = dbc))
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      verbose = TRUE,
      con = dbc)),
  "Imported or updated")

# test
expect_true(tmpTest$n > rev(tmpTest2[["query-records"]])[[1]])

# test
expect_true(length(tmpTest$failed) == 0L)


#### ctrLoadQueryIntoDb results ####

# get results
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(
        "primary_outcome.measure",
        "start_date",
        "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
        "clinical_results.baseline.group_list.group",
        "clinical_results.baseline.analyzed_list.analyzed.units",
        "clinical_results.outcome_list.outcome",
        "clinical_results",
        "study_design_info.allocation",
        "eligibility.maximum_age",
        "location.facility.name",
        "location"
        ),
      con = dbc)
  ))

# test
expect_equal(
  rev(
    sapply(
      result[["location"]],
      function(x) length(x[["facility"]][["name"]])))[1:2],
  c(30, 1))

# test
expect_true("character" == class(result[[
  "study_design_info.allocation"]]))

# test
expect_true("character" == class(result[[
  "primary_outcome.measure"]]))

# test
expect_true(
  any(grepl(" / ", result[["primary_outcome.measure"]])))

# test
expect_true("Date" == class(result[[
  "start_date"]]))

# test
expect_true("difftime" == class(result[[
  "eligibility.maximum_age"]]))

# test
expect_true(
  any(grepl(" / ", result[["location.facility.name"]])))
expect_true(
  length(unlist(strsplit(
    result[["location.facility.name"]], " / "))) >= 32L)

# test
expect_true("list" == class(result[[
  "clinical_results.baseline.group_list.group"]]))

# test
tmpTest <- c(
  "clinical_results.baseline", "clinical_results.outcome_list.outcome",
  "clinical_results.reported_events", "clinical_results.participant_flow",
  "clinical_results.point_of_contact", "clinical_results.certain_agreements"
  # not in the downloaded but in other trials:
  # "clinical_results.limitations_and_caveats"
  )
expect_true(
  length(setdiff(tmpTest, names(result))) == 0L)

# convert to long
df <- suppressMessages(
  dfTrials2Long(
    df = result
  ))

# test
expect_identical(
  names(df),
  c("_id", "identifier", "name", "value")
)

# test
expect_true(
  nrow(df) > 800L
)

# select value from
# measure in where
df2 <- suppressMessages(
  dfName2Value(
    df = df,
    valuename = paste0(
      "clinical_results.*category_list.category.measurement_list.measurement.value|",
      "clinical_results.outcome_list.outcome.measure.units"
    ),
    wherename = "clinical_results.outcome_list.outcome.measure.title",
    wherevalue = "duration of response"
  ))

# test
expect_true(
  any("NCT01471782" %in% df2[["_id"]])
)

# test
expect_true(
  all(grepl("5", df2[["identifier"]][ df2[["_id"]] == "NCT01471782" ]))
)

# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "term=ET743OVC3006",
        register = "CTGOV",
        annotation.text = "something",
        annotation.mode = "WRONG",
        con = dbc))),
  "'annotation.mode' incorrect")

#### documents.path ####

if (!length(dbc$url) || grepl("localhost", dbc$url)) {
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "cond=Neuroblastoma&type=Intr&recrs=e&phase=1&u_prot=Y&u_sap=Y&u_icf=Y",
        register = "CTGOV",
        documents.path = newTempDir(),
        documents.regexp = ".*",
        con = dbc
      )),
    "Newly saved [0-9]+ document"
  )
}

#### dbFindFields ####

# test
expect_equal(
  suppressMessages(
    suppressWarnings(
      dbFindFields(
        namepart = "thisdoesnotexist",
        con = dbc))),
  "")

# get all field names
tmpf <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = ".*",
      con = dbc)))

# get all data
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = tmpf,
      con = dbc,
      verbose = FALSE,
      stopifnodata = FALSE)
  ))

# develop
# print(length(names(result)))

# test
expect_true(
  length(names(result)) > 150L)

# determine all classes
tmpr <- names(result)
tmpr <- tmpr[tmpr != "_id"]
tmpc <- sapply(result, class, USE.NAMES = FALSE)
tmpc <- unlist(tmpc)
tmpc <- table(tmpc)

# develop
#
# print(tmpc)
#
# 2022-02-20
#
# tinytest::run_test_file("inst/tinytest/test_ctrdata_mongo_local_ctgov.R") # 155
# Downloading: 8.3 kB     tmpcov.R    0 tests
# character data.frame       Date   difftime    integer       list    logical
# 116          4          7          1          2         82          5
# test_ctrdata_mongo_local_ctgov.R   34 tests OK 12.2s
# All ok, 34 results (12.2s)
# > tinytest::run_test_file("inst/tinytest/test_ctrdata_mongo_remote_rw_ctgov.R") # 155
# Downloading: 8.3 kB     tmpc_ctgov.R    0 tests
# character data.frame       Date   difftime    integer       list    logical
# 116          4          7          1          2         82          5
# test_ctrdata_mongo_remote_rw_ctgov.R   34 tests OK 41.7s
# All ok, 34 results (41.7s)
# > tinytest::run_test_file("inst/tinytest/test_ctrdata_postgres_ctgov.R") # 155
# Downloading: 8.3 kB     tmpcR.    0 tests
# character      Date  difftime   integer      list   logical
# 138         7         1         2        73        14
# test_ctrdata_postgres_ctgov.R.   34 tests OK 22.7s
# All ok, 34 results (22.7s)
# > tinytest::run_test_file("inst/tinytest/test_ctrdata_sqlite_ctgov.R") # 155
# Downloading: 8.3 kB     tmpc..    0 tests
# character      Date  difftime   integer      list   logical
# 134         7         1         2        59        32
# test_ctrdata_sqlite_ctgov.R...   34 tests OK 21.8s
# All ok, 34 results (21.8s)

# clean up
rm(df, df2, tmpf, result, tmpc)
