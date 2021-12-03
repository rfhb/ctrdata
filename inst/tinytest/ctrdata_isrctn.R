## RH 2021-04-25

# check server
httr::set_config(httr::timeout(seconds = 60))

if (httr::status_code(
  httr::GET("https://www.isrctn.com/editAdvancedSearch")) != 200L
) exit_file("Reason: ISRCTN not working")

#### ctrLoadQueryIntoDb ####

# test with slightly incorrect url
expect_message(
  ctrLoadQueryIntoDb(
    queryterm = "https://www.isrctn.com/search?neuroblastoma",
    only.count = TRUE),
  "search query from ISRCTN: q=neuroblastoma")

# test
expect_error(
  suppressMessages(
    ctrLoadQueryIntoDb(
      queryterm = "https://www.isrctn.com/search?q=")),
  "consider correcting or splitting queries")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "neuroblastoma",
      register = "ISRCTN", verbose = F,
      con = dbc)),
  "Imported or updated ")

# test
expect_true(tmpTest$n >= 8L)

# test
expect_true(all(c("98918118", "79815499") %in% tmpTest$success))

# test
expect_true(length(tmpTest$failed) == 0L)


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
expect_true(tmpTest$n > 2L)

# test
expect_true(length(tmpTest$success) > 2L)

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
      con = dbc)
  ))

# test
expect_true(
  res[, "annotation", drop = TRUE] == "just_this" &
    res[, "_id", drop = TRUE] == "98918118")
rm(res)

# test
expect_error(
  suppressMessages(
    suppressWarnings(
      dbGetFieldsIntoDf(
        fields = c("doesnotexist"),
        con = dbc))),
  "No data could be extracted for")

#### all fields ####

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

# TODO
print(length(names(result)))

# test
expect_true(
  length(names(result)) > 50L)
rm(result)

#### dbFindIdsUniqueTrials ####

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 5L)
rm(res)

