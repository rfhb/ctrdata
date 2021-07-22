## RH 2019-09-28

# check server
if (httr::status_code(
  httr::GET("https://clinicaltrials.gov/ct2/search",
             httr::timeout(5))) != 200L
) exit_file("Reason: CTGOV not working")

#### ctrLoadQueryIntoDb ####

# test
expect_equal(
  suppressMessages(
  ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "CTGOV",
    only.count = TRUE))[["n"]], 1L)

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
        "https://clinicaltrials.gov/ct2/results?cond=Cancer&type=Intr&phase=0",
        "&strd_s=01%2F02%2F2005&strd_e=12%2F31%2F2017"),
      con = dbc))),
  "more than 10,000) trials")


#### ctrLoadQueryIntoDb update ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      verbose = TRUE,
      con = dbc)),
  "No trials or number of trials could not be determined")

# test
expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = 999L,
      con = dbc)),
  "'querytoupdate': specified number not found")

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
        "location.facility.name",
        "location"),
      con = dbc)
  ))

# test
expect_equal(
  sapply(
    result[["location"]],
    function(x) length(x[["facility"]][["name"]])),
  c(1, 1, 1, 30))

# test
expect_true("character" == class(result[[
  "study_design_info.allocation"]]))

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
expect_true(
  sum(nchar(
    # note: deprecated function
    suppressWarnings(
      dfListExtractKey(
        result,
        list(c("location", "name"))
      ))[["value"]]),
    na.rm = TRUE) > 1000L)

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
  any("NCT01471782" %in% df2[["_id"]])
)

# test
expect_true(
  all(grepl("^0.5", df2[["identifier"]][ df2[["_id"]] == "NCT01471782" ]))
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
