## RH 2021-04-25

#### ctrLoadQueryIntoDb ####

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "neuroblastoma",
      register = "ISRCTN",
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
nodbi::docdb_update(
  src = dbc,
  key = dbc$collection,
  value = data.frame("_id" = "meta-info",
                     "content" = as.character(json),
                     stringsAsFactors = FALSE,
                     check.names = FALSE))

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
  "Imported or updated 1 trial")


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

#### dbFindIdsUniqueTrials ####

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 5L)
rm(res)

