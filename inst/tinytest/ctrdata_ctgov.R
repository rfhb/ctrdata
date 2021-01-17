## RH 2019-09-28

#### ctrLoadQueryIntoDb ####

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

#### ctrLoadQueryIntoDb update ####

# new query
q <- paste0("https://clinicaltrials.gov/ct2/results?",
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
      register = "CTGOV",
      verbose = TRUE,
      only.count = TRUE,
      con = dbc)),
  "term=someQueryForErrorTriggering")


#### ctrLoadQueryIntoDb results ####

# get results
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(
        "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
        "clinical_results.baseline.group_list.group",
        "clinical_results.baseline.analyzed_list.analyzed.units",
        "clinical_results.outcome_list.outcome",
        "study_design_info.allocation",
        "location"),
      con = dbc)))

result$numberSites <- sapply(
  result$location,
  function(x) length(x[["facility"]][["name"]]))

# test
expect_true(sum(result$numberSites, na.rm = TRUE) > 30L)

# test
expect_true("character" == class(result[[
  "study_design_info.allocation"]]))

# test
expect_true("list" == class(result[[
  "clinical_results.baseline.group_list.group"]]))

# test
expect_true(
  sum(nchar(
    # note: deprecated function
    suppressWarnings(
      dfListExtractKey(
    result,
    list(c("location", "name"))
  ))),
  na.rm = TRUE) > 1000L)

# convert to long
df <- suppressMessages(
  dfTrials2Long(
    df = result
  ))

# test
expect_identical(
    names(df),
    c("trial_id", "main_id",
      "sub_id", "name", "value")
)

# test
expect_true(
  nrow(df) > 900L
)

# select value from
# measure in where
df2 <- dfName2Value(
  df = df,
  valuename = paste0(
    "clinical_results.*category_list.category.measurement_list.measurement.value|",
    "clinical_results.outcome_list.outcome.measure.units"
  ),
  wherename = "clinical_results.outcome_list.outcome.measure.title",
  wherevalue = "duration of response"
)

# test
expect_true(
  nrow(df2) > 2L
)

# test
expect_true(
  all(df2$main_id[df2$trial_id == "NCT01471782"] == 5L)
)
