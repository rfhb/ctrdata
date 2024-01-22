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

tempDf <- data.frame(
  `query-term` = c("q=neuroblastoma", "q=neuroblastoma"),
  `query-register` = c("ISRCTN", "ISRTCN"),
  check.names = FALSE
)

# test
expect_warning(
  ctrLoadQueryIntoDb(
    queryterm = tempDf,
    con = dbc),
  "Using last row of queryterm parameter")

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
suppressWarnings(
  suppressMessages(
    tmpDf <- dbGetFieldsIntoDf(
      fields = c(
        "participants.totalFinalEnrolment"
      ), con = dbc)))
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
tmpFields <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = ".*",
      con = dbc)))

#### dbGetFieldsIntoDf ####

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[1:length(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

tmpFields <- tmpFields[grepl("date$", tmpFields, ignore.case = TRUE)]
tmpFields <- tmpFields[1:min(length(tmpFields), 49L)]

tmpData <- dbGetFieldsIntoDf(fields = tmpFields, con = dbc)
expect_true(nrow(tmpData) > 0L)
expect_true(ncol(tmpData) > 0L)

expect_true(all(
  unique(unlist(lapply(
    tmpData[, -1, drop = FALSE],
    function(i) sapply(i, function(ii) class(ii))))) %in%
    c("Date", "POSIXct", "POSIXt")
))

# determine all classes
# tmpr <- names(result)
# tmpr <- tmpr[tmpr != "_id"]
# tmpc <- sapply(result, class, USE.NAMES = FALSE)
# tmpc <- unlist(tmpc)
# tmpc <- table(tmpc)


#### dbFindIdsUniqueTrials ####

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 7L)
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
