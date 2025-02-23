## RH 2019-09-28

#### ctrLoadQueryIntoDb ####

# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "query=",
        register = "EUCTR",
        con = dbc))),
  "more than 10,000) trials")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "SHOULDNOTEXISTATALL",
      register = "EUCTR",
      con = dbc)),
  "no.*trials found")

# test
expect_true(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "2005-001267-63+OR+2008-003606-33",
        register = "EUCTR",
        only.count = TRUE)))[["n"]] >= 2L)

# test
expect_true(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "2016-004774-17",
        register = "EUCTR",
        con = dbc)))[["n"]] >= 1L)

# correct _id association

# test
expect_true(
  setequal(
    suppressWarnings(
      suppressMessages(
        ctrLoadQueryIntoDb(
          queryterm = "2005-001267-63+OR+2008-003606-33",
          register = "EUCTR",
          euctrresults = TRUE,
          con = dbc
        )[["success"]])),
    c("2008-003606-33-GB",
      "2005-001267-63-AT",
      "2005-001267-63-IT")))

# test
expect_identical(
  suppressWarnings(
    suppressMessages(
      dbGetFieldsIntoDf(
        fields = "endPoints.endPoint.title",
        con = dbc
      )))[1, "_id", drop = TRUE],
  "2008-003606-33-GB")

# next
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "neuroblastoma&resultsstatus=trials-with-results&phase=phase-one&country=pl")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      con = dbc)),
  "Imported or updated")

# test
expect_true(tmpTest$n > 10L)

# test
expect_true(all(
  c("2007-000371-42-FR", "2010-019340-40-GB",
    "2010-019340-40-3RD") %in%
    tmpTest$success))

# test
expect_true(length(tmpTest$failed) == 0L)
rm(tmpTest, q)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      # trial with f1111, f1121 to check typeField
      queryterm = "2013-003488-71",
      register = "EUCTR",
      con = dbc)),
  "Imported or updated")

#### ctrLoadQueryIntoDb update ####

# manipulate history to test updating
# and query string handling
hist <- suppressWarnings(dbQueryHistory(con = dbc))
#
hist[nrow(hist), "query-term"] <-
  sub("query=", "", hist[nrow(hist), "query-term"])
hist[nrow(hist), "query-timestamp"] <- "2000-01-01 00:00:00"
#
# convert into json object
json <- jsonlite::toJSON(list("queries" = hist))
#
# update database
expect_equal(
  nodbi::docdb_update(
    src = dbc,
    key = dbc$collection,
    value = as.character(json),
    query = '{"_id": "meta-info"}'), 1L)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc,
      verbose = TRUE)),
  "search[?]query=2013-003488-71")

# checking as only works for last 7 days with rss mechanism
# query just based on date is used to avoids no trials are found
#
date.today <- Sys.time()
date.from  <- format(date.today - (60 * 60 * 24 * 9), "%Y-%m-%d")
date.to    <- format(date.today - (60 * 60 * 24 * 4), "%Y-%m-%d")
#
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&phase=phase-two",
            "&dateFrom=", date.from, "&dateTo=", date.to)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = paste0(q),
      verbose = TRUE,
      con = dbc)),
  "(Imported or updated|First result page empty)")

# manipulate history to test updating
hist <- suppressWarnings(dbQueryHistory(con = dbc))
#
hist[nrow(hist), "query-term"] <-
  sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
#
hist[nrow(hist), "query-timestamp"] <-
  paste0(date.to, " 23:59:59")
#
# convert into json object
json <- jsonlite::toJSON(list("queries" = hist))
#
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
      verbose = TRUE,
      con = dbc)),
  "(Imported or updated|First result page empty)")

# test
expect_true(tmpTest$n >= 0L)

# test
expect_true(is.character(tmpTest$success) || is.null(tmpTest$success))
rm(tmpTest, q, hist, json, date.from, date.to, date.today)

# test
expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = 3.14159,
      con = dbc)),
  "needs to be an integer")

#### ctrLoadQueryIntoDb results ####

# get trials with results
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "2013-003420-37+OR+2009-011454-17+OR+2006-005357-29")

if (!length(dbc$url) || grepl("localhost", dbc$url)) {
  # test
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = q,
        euctrresults = TRUE,
        euctrresultshistory = TRUE,
        documents.path = tempfile(),
        con = dbc)),
    "Created directory")
  # test
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = q,
        euctrresults = TRUE,
        euctrresultshistory = TRUE,
        documents.path = newTempDir(),
        con = dbc)),
    "Imported or updated results for 3")
} else {
  # test
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = q,
        euctrresults = TRUE,
        euctrresultshistory = TRUE,
        con = dbc)),
    "Imported or updated results for 3")
}
rm(q)


# get results
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(
        "a2_eudract_number",
        "p_date_of_the_global_end_of_the_trial",
        "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
        "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
        "endPoints.endPoint.statisticalAnalyses",
        "trialInformation.globalEndOfTrialDate",
        "trialInformation.analysisForPrimaryCompletion",
        "trialInformation.sponsors",
        "trialInformation",
        "endPoints.endPoint",
        "e71_human_pharmacology_phase_i",
        "dimp.d21_imp_to_be_used_in_the_trial_has_a_marketing_authorisation",
        "trialInformation",
        "f11_trial_has_subjects_under_18",
        "e824_number_of_treatment_arms_in_the_trial"
      ),
      con = dbc)
  ))

# test
expect_true(nrow(result) > 30L)
expect_true(ncol(result) > 30L)
expect_true(ncol(result) > 15L)

# keep only one record for trial
result2 <- suppressWarnings(suppressMessages(
  result[result[["_id"]] %in%
           dbFindIdsUniqueTrials(con = dbc), ]
))
# test
expect_identical(
  length(unique(result$a2_eudract_number)),
  nrow(result2)
)

# test
expect_true(all(
  as.Date(c("2013-10-28")) %in%
    result$trialInformation.globalEndOfTrialDate))

# test
expect_true("logical" == class(result[[
  "e71_human_pharmacology_phase_i"]]))

# test
expect_true("logical" == class(result[[
  "trialInformation.analysisForPrimaryCompletion"]]))

# test
expect_true("Date" == class(result[[
  "trialInformation.globalEndOfTrialDate"]]))

# test
expect_true("list" == class(result[[
  "endPoints.endPoint"]]))

# test
tmp <- result[["dimp.d21_imp_to_be_used_in_the_trial_has_a_marketing_authorisation"]]
expect_true((class(tmp) == "list" && (length(unlist(tmp)) > length(tmp)) &&
               all(class(unlist(tmp)) == "logical")) || (class(tmp) == "logical"))

# test
expect_true("logical" == class(result[[
  "f11_trial_has_subjects_under_18"]]))

# test
expect_true("integer" == class(result[[
  "e824_number_of_treatment_arms_in_the_trial"]]))

# cleanup
rm(tmp)


#### dfTrials2Long ####

# convert to long
df <- suppressMessages(
  dfTrials2Long(
    df = result2
  ))

# test
expect_identical(
  names(df),
  c("_id", "identifier", "name", "value")
)

# test
expect_true(
  nrow(df) > 3300L
)

# test
expect_error(
  dfTrials2Long(
    df = result2[, -1]),
  "Missing _id column or other variables in 'df'")

expect_error(
  dfTrials2Long(
    df = df),
  "'df' should not come from dfTrials2Long")

#### dfName2Value ####

# extract
df2 <- suppressMessages(
  dfName2Value(
    df = df,
    valuename = "subjectDisposition.*postAssignmentPeriod.arms.arm.type.value"
  ))
# test
expect_true(
  length(unique(df2[["_id"]])) >= 2L
)

# extract
df2 <- suppressMessages(
  dfName2Value(
    df = df,
    valuename = "subjectDisposition.*postAssignmentPeriod.arms.arm.type.value",
    wherename = "endPoints.endPoint.title",
    wherevalue = "percentage"
  ))

# test
expect_true(
  length(unique(df2[["_id"]])) >= 2L
)

# test
expect_true(
  length(unique(df2[["value"]])) >= 2L
)

# test
expect_true(all(
  df2[["value"]] %in%
    c("ARM_TYPE.placeboComp", "ARM_TYPE.experimental"))
)

# extract
df2 <- suppressMessages(
  dfName2Value(
    df = df,
    valuename = "^endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value$",
    wherename = "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value",
    wherevalue = "HYPOTHESIS_METHOD.*"
  ))

# test
expect_true(
  all(df2[["value"]] < 1)
)

# test
expect_error(
  suppressMessages(
    dfName2Value(
      df = df,
      valuename = "doesnotexist")),
  "No rows found for 'valuename'")

# test
expect_error(
  suppressMessages(
    dfName2Value(
      df = df,
      valuename = "^endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value$",
      wherename = "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value",
      wherevalue = "doesnotexist")),
  "No rows found for 'wherename' and 'wherevalue'")

# cleanup
rm(result, result2, df, df2)

#### dbFindFields #####

# test
expect_error(
  dbFindFields(
    namepart = c("onestring", "twostring"),
    con = dbc),
  "'namepart' should have one element.")

# test
expect_error(
  dbFindFields(
    namepart = list("onestring", "twostring"),
    con = dbc),
  "'namepart' should be atomic.")

# test
expect_error(
  dbFindFields(namepart = "",
               con = dbc),
  "Empty 'namepart' parameter.")

# test
tmpTest <- suppressMessages(suppressWarnings(
  dbFindFields(
    namepart = "date",
    con = dbc)))
expect_true("character" %in% class(tmpTest))
expect_true(length(tmpTest) >= 4L)
rm(tmpTest)


#### annotations #####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "NCT00001209+OR+NCT00001436+OR+NCT00187109+OR+NCT01516567+OR+NCT01471782",
      register = "CTGOV",
      con = dbc,
      verbose = TRUE,
      annotation.text = "ANNO",
      annotation.mode = "replace")),
  "Imported or updated.*5 trial")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "EUCTR",
      con = dbc,
      annotation.text = "ANNO",
      annotation.mode = "replace")),
  "Imported or updated")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "EUCTR",
      con = dbc,
      annotation.text = "EU",
      annotation.mode = "prepend")),
  "Imported or updated")

tmpTest <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = "annotation",
      con = dbc)))

# test
expect_true(all(
  tmpTest[grepl("2010-024264-18", tmpTest[["_id"]]), "annotation"] == "EU ANNO"))
# test
expect_true(all(
  tmpTest[grepl("^NCT", tmpTest[["_id"]]), "annotation"] == "ANNO"))

# clean up
rm(tmpTest)


#### deduplicate ####

trialsCtgov <- suppressMessages(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      verbose = TRUE,
      preferregister = "CTGOV")))

trialsEuctr <- suppressMessages(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "EUCTR")))

# test
expect_equal(length(trialsCtgov), length(trialsEuctr))

# test
expect_true(
  all(c("NCT00001209", "NCT00001436", "NCT00187109", "NCT01516567") %in%
        trialsCtgov))

# test
expect_false(any(c("2010-024264-18-GB") %in% trialsCtgov))

# test
expect_true(
  all(c("NCT00001209", "NCT00001436", "NCT00187109", "NCT01516567",
        "2013-003420-37-BE") %in% trialsEuctr))

# test
expect_false(any(c("NCT01471782") %in% trialsEuctr))

# clean up
rm(trialsCtgov, trialsEuctr)

# test
expect_error(
  dbFindIdsUniqueTrials(
    con = dbc,
    preferregister = "WRONG"),
  "unknown")

# test
expect_error(
  suppressMessages(
    suppressWarnings(
      dbFindIdsUniqueTrials(
        con = dbc,
        prefermemberstate = "WRONG"))),
  "unknown")

# test
expect_message(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "CTGOV")),
  "Returning keys \\(_id\\) of [1-9][0-9]+ records")

# test
expect_warning(
  suppressMessages(
    tmpTest <- dbFindIdsUniqueTrials(
      con = dbc,
      prefermemberstate = "3RD",
      include3rdcountrytrials = FALSE)),
  "Preferred .* 3RD .* setting it to TRUE")

# test
expect_true(
  length(
    suppressWarnings(
      suppressMessages(
        dbFindIdsUniqueTrials(
          con = dbc,
          include3rdcountrytrials = FALSE)
      ))) > 5L)

# test, reusing the query string
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "2013-003420-37+OR+2009-011454-17+OR+2006-005357-29")
tmpQ <- strsplit(q, "+OR+", fixed = TRUE)[[1]]
tmpQ <- gsub(".+=(.?)", "\\1", tmpQ)
expect_true(all(
  tmpQ %in%
    gsub("([0-9]{4}-[0-9]{6}-[0-9]{2})-.*", "\\1", tmpTest)))

# clean up
rm(tmpTest, tmpQ, q)

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
      sample = FALSE,
      con = dbc)))

# test
expect_true(
  length(tmpFields) > 600)

#### dbGetFieldsIntoDf ####

# test
expect_error(
  dbGetFieldsIntoDf(
    fields = 1:3,
    con = dbc),
  "Input should be a vector of strings of field names")

# test
expect_error(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(NA, "", NULL),
      con = dbc)),
  " empty")

# test as many fields as possible for typing

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[seq_along(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

tmpFields <- tmpFields[grepl("date$", tmpFields, ignore.case = TRUE)]
tmpFields <- tmpFields[1:min(seq_along(tmpFields), 49L)]

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


#### ctrOpenSearchPagesInBrowser #####

# test
expect_equal(
  suppressWarnings(
    ctrOpenSearchPagesInBrowser(
      dbQueryHistory(con = dbc))),
  # opens last query loaded into the database
  paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?",
         "query=2010-024264-18#tabs"))
