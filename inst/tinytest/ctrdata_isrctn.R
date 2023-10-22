## RH 2021-04-25

# set server
httr::set_config(httr::timeout(seconds = 60))

#### ctrLoadQueryIntoDb ####

# test with slightly incorrect url
expect_message(
  ctrLoadQueryIntoDb(
    queryterm = "https://www.isrctn.com/search?neuroblastoma",
    only.count = TRUE),
  "search query from ISRCTN: q=neuroblastoma")

# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "https://www.isrctn.com/search?q=",
        con = dbc))),
  "consider correcting or splitting queries")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "SHOULDNOTEXISTATALL",
      register = "ISRCTN",
      con = dbc)),
  "no.*trials")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "neuroblastoma",
      register = "ISRCTN",
      verbose = TRUE,
      con = dbc)),
  "Imported or updated ")

# test
expect_true(tmpTest$n >= 8L)

# test
expect_true(all(c("98918118", "79815499") %in% tmpTest$success))

# test
expect_true(length(tmpTest$failed) == 0L)

# clean up
rm(tmpTest)

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://www.isrctn.com/search?q=&filters=phase%3APhase+III%2CLE+lastEdited%3A2018-01-01",
      only.count = TRUE,
      verbose = TRUE,
      con = dbc)),
  "Retrieved overview")

# test
expect_true(tmpTest$n < 250L)

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "q=neuroblastoma+OR+lymphoma&filters=phase%3APhase+III%2CLE+lastEdited%3A2018-01-01",
      register = "ISRCTN",
      only.count = TRUE,
      verbose = TRUE,
      con = dbc)),
  "are to be downloaded")

# test
expect_true(tmpTest$n < 15L)

# clean up
rm(tmpTest)

# test
tmpTest <- suppressWarnings(
  ctrLoadQueryIntoDb(
    queryterm = "21727048+OR+10746820+OR+98918118+OR+40708286+OR+57509500+OR+37126758+OR+67769193",
    register = "ISRCTN",
    only.count = TRUE,
    verbose = TRUE))

# test
expect_true(tmpTest$n == 7L)


#### ctrLoadQueryIntoDb update ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc)),
  "Search result page empty")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc,
      verbose = TRUE)),
  "Updating using this additional query term")


# new query (last edited before)
oldQueryDate <- format(Sys.Date() - 365 * 2,  "%Y-%m-%d")
q <- paste0("https://www.isrctn.com/search?q=neuroblastoma&filters=condition:Cancer",
            ",LE+lastEdited:", oldQueryDate,
            "T00:00:00.000Z&searchType=advanced-search")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      con = dbc)),
  "Imported or updated ")

# clean up
rm(q, tmpTest)

# test
expect_warning(
  ctrLoadQueryIntoDb(
    querytoupdate = "last",
    con = dbc),
  "running again with these limits")

# manipulate history to remove lastEdited filter
# test updating using dbCTRUpdateQueryHistory
hist <- suppressWarnings(dbQueryHistory(con = dbc))
hist[nrow(hist), "query-term"] <-
  sub(",LE.*Z", "", hist[nrow(hist), "query-term"])
hist[nrow(hist), "query-timestamp"] <-
  sub(format(Sys.Date(),  "%Y-%m-%d"), oldQueryDate, hist[nrow(hist), "query-timestamp"])

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
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc)),
  "Imported or updated")

# test
expect_true(tmpTest$n >= 1L)

# test
expect_true(length(tmpTest$success) >= 1L)

# test
expect_true(length(tmpTest$failed) == 0L)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "someQueryForErrorTriggering",
      register = "ISRCTN",
      verbose = TRUE,
      only.count = TRUE,
      con = dbc)),
  "Search result page empty")

# clean up
rm(tmpTest, json, hist, oldQueryDate)


#### annotating ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "q=98918118",
      register = "ISRCTN",
      annotation.text = "just_this",
      annotation.mode = "replace",
      con = dbc)),
  "Annotated .*1 record")

#### dbGetFieldsIntoDf ####

res <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c("annotation"),
      verbose = TRUE,
      con = dbc)
  ))

# test
expect_true(
  res[, "annotation", drop = TRUE] == "just_this" &
    res[, "_id", drop = TRUE] == "98918118")

# clean up
rm(res)

# test
expect_error(
  suppressMessages(
    suppressWarnings(
      dbGetFieldsIntoDf(
        fields = c("doesnotexist"),
        con = dbc))),
  "No data could be extracted for")

# test
expect_warning(
  suppressMessages(
    dbGetFieldsIntoDf(
      fields = c("doesnotexist"),
      stopifnodata = FALSE,
      con = dbc)
  ),
  "No records with values for any specified field")

# test
suppressWarnings(
  suppressMessages(
    tmpDf <- dbGetFieldsIntoDf(
      fields = c(
        "participants.totalFinalEnrolment"
        ),
      stopifnodata = FALSE,
      con = dbc)))
#
expect_equivalent(
  sapply(tmpDf, typeof),
  c("character", "integer")
)

# clean up
rm(tmpDf)

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
#print(length(names(result)))

# test
expect_true(
  length(names(result)) > 40L)

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
# 2022-10-30
# tmpc
# character data.frame       Date    integer       list    logical
#        49          2          5          2          2          3
#
# 2022-02-20
#
# > tinytest::run_test_file("inst/tinytest/test_ctrdata_postgres_isrctn.R") # 53
# Downloading: 10 kB     tmpcn.R    0 tests
# character      Date   integer      list   logical
# 50         5         2         3         4
# test_ctrdata_postgres_isrctn.R   26 tests OK 9.0s
# All ok, 26 results (9.0s)
# > tinytest::run_test_file("inst/tinytest/test_ctrdata_mongo_local_isrctn.R") # 55
# Downloading: 10 kB     tmpcrctn.R    0 tests
# character      Date   integer      list   logical
# 51         5         2         1         4
# test_ctrdata_mongo_local_isrctn.R   26 tests OK 4.1s
# All ok, 26 results (4.1s)
# > tinytest::run_test_file("inst/tinytest/test_ctrdata_sqlite_isrctn.R") # 55
# Downloading: 10 kB     tmpcR..    0 tests
# character      Date   integer      list   logical
# 50         5         2         3         4
# test_ctrdata_sqlite_isrctn.R..   26 tests OK 5.8s
# All ok, 26 results (5.8s)

rm(tmpf, tmpr, tmpc, result)

#### dbFindIdsUniqueTrials ####

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 5L)
rm(res)


#### ctrLoadQueryIntoDb documents ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

expect_message(
  ctrLoadQueryIntoDb(
    queryterm = "https://www.isrctn.com/search?q=alzheimer",
    con = dbc,
    documents.path = tmpDir,
    documents.regexp = ".*"
  ),
  "Newly saved [0-9]+ document[(]s[)] for [0-9]+ trial"
)
