## RH 2019-09-28

#### ctrLoadQueryIntoDb ####

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "neuroblastoma&status=completed&phase=phase-one&country=pl")

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      con = dbc)),
  "Imported or updated")

# test
expect_true(tmp_test$n > 10L)

# test
expect_true(all(
  c("2007-000371-42-FR", "2010-019340-40-GB",
    "2010-019340-40-3RD") %in%
    tmp_test$success))

# test
expect_true(length(tmp_test$failed) == 0L)


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
nodbi::docdb_update(
  src = dbc,
  key = dbc$collection,
  value = data.frame("_id" = "meta-info",
                     "content" = as.character(json),
                     stringsAsFactors = FALSE,
                     check.names = FALSE))
# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc,
      verbose = TRUE)),
  "DEBUG: queryterm is .*?search\\?query=neuro")

# checking as only works for last 7 days with rss mechanism
# query based on date is used since this avoids no trials are found

date.today <- Sys.time()
date.from  <- format(date.today - (60 * 60 * 24 * 15), "%Y-%m-%d")
date.to    <- format(date.today - (60 * 60 * 24 *  9), "%Y-%m-%d")

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "&dateFrom=", date.from, "&dateTo=", date.to)

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      paste0(q),
      con = dbc)),
  "Imported or updated ")

# manipulate history to test updating
hist <- suppressWarnings(dbQueryHistory(con = dbc))
#
hist[nrow(hist), "query-term"] <-
  sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
#
hist[nrow(hist), "query-timestamp"] <-
  paste0(date.to, " 23:59:59")

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
  "(Imported or updated|First result page empty)")

# test
expect_true(tmp_test$n > 10L)

# test
expect_true(length(tmp_test$success) > 10L)


#### ctrLoadQueryIntoDb results ####

# https://www.clinicaltrialsregister.eu/ctr-search/search?query=&
# age=under-18&phase=phase-three&resultsstatus=trials-with-results

# get trials with results
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "2013-003420-37+OR+2009-011454-17+OR+2006-005357-29")

expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      euctrresults = TRUE,
      euctrresultshistory = TRUE,
      verbose = TRUE,
      con = dbc)),
  "Imported or updated results for")

# get results
# dbFindFields(namepart = c("statistic"),
#              con = dbc)
result <- suppressWarnings(
  dbGetFieldsIntoDf(
    fields = c(
      "a2_eudract_number",
      "trialInformation.globalEndOfTrialDate",
      "p_date_of_the_global_end_of_the_trial",
      "trialInformation.recruitmentStartDate",
      "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
      "endPoints.endPoint", # this is also a list
      "trialInformation.analysisForPrimaryCompletion",
      "e71_human_pharmacology_phase_i"
    ),
    con = dbc,
    stopifnodata = FALSE)
)

# keep only one record for trial
result <- suppressWarnings(suppressMessages(
  result[ result[["_id"]] %in%
            dbFindIdsUniqueTrials(con = dbc), ]
))

# test
expect_true(all(as.Date(c("2013-10-28", "2018-03-13")) %in%
                  result$trialInformation.globalEndOfTrialDate))

# test
expect_true("logical" == class(result[[
  "e71_human_pharmacology_phase_i"]]))

# test
expect_true("Date" == class(result[[
  "trialInformation.globalEndOfTrialDate"]]))

# test
expect_true(
  sum(nchar(
    dfListExtractKey(
      df = result,
      list.key = list(
        c("subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
          "title"),
        c("subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
          "type.value"))
    )[["value"]]
  ), na.rm = TRUE)
  > 250L)

# test
expect_true(
  sum(nchar(
    dfListExtractKey(
      df = result,
      list.key = list(
        c("endPoints.endPoint", "title"))
    )[["value"]]
  ), na.rm = TRUE)
  > 3000L)

# prepare
tmp_test <- dfListExtractKey(
  result,
  list(
    c("endPoints.endPoint", "^title"),
    c("endPoints.endPoint", "^type.value")
  )
)
# test
expect_true(all(
  tmp_test$endPoints.endPoint.type.value %in%
    c("ENDPOINT_TYPE.primary", "ENDPOINT_TYPE.secondary",
      "ENDPOINT_TYPE.other", NA)))

# test
tmp_test <- dfListExtractKey(
  result,
  list(
    c("endPoints.endPoint", "armReportingGroups.armReportingGroup.subjects[0-9]*$"),
    c("endPoints.endPoint", "armReportingGroups.armReportingGroup.@attributes.id"),
    c("endPoints.endPoint", "armReportingGroups.armReportingGroup.@attributes.armId")
  )
)

# test
expect_true(
  sum(
    as.numeric(
      tmp_test[["value"]][
        tmp_test[["name"]] ==
          "endPoints.endPoint.armReportingGroups.armReportingGroup.subjects"
      ]),
    na.rm = TRUE) > 2000L)

# test
tmp_test <- dfListExtractKey(
  result,
  list(
    c("endPoints.endPoint", "statisticalAnalyses.statisticalAnalysis.title"),
    c("endPoints.endPoint", "statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value"),
    c("endPoints.endPoint", "statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value[0-9]*$")
  )
)

# test
expect_true(
  all(na.omit(as.numeric(
    tmp_test[["value"]][
      tmp_test[["name"]] ==
        "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value"
    ])) < 1L))

# test
expect_true(all(
  c("HYPOTHESIS_METHOD.cochranMantelHaenszel", "HYPOTHESIS_METHOD.ancova",
    "HYPOTHESIS_METHOD.regressionLogistic") %in%
    tmp_test[["value"]][
      tmp_test[["name"]] ==
        "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value"]))

# test
expect_error(
  dfListExtractKey(
    result[, -1],
    list(
      c("endPoints.endPoint", "^type.value")
    )
  ),
  "Data frame 'df' lacks '_id' column")


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
tmp_test <- suppressMessages(suppressWarnings(
  dbFindFields(
    namepart = "date",
    con = dbc)))
expect_true("character" %in% class(tmp_test))
expect_true(length(tmp_test) >= 4L)

#### dbFindIdsUniqueTrials #####

# test
expect_message(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "EUCTR")),
  "Searching for duplicates")

# test
expect_message(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "CTGOV")),
  "Returning keys \\(_id\\) of [4-9][0-9]")

# test
expect_warning(
  suppressMessages(
    tmp_test <- dbFindIdsUniqueTrials(
      con = dbc,
      prefermemberstate = "3RD",
      include3rdcountrytrials = FALSE)),
  "Preferred EUCTR version set to 3RD country trials")

# test, reusing the query string
tmp_q <- strsplit(q, "+OR+", fixed = TRUE)[[1]]
tmp_q <- gsub(".+=(.?)", "\\1", tmp_q)
expect_true(all(
  tmp_q %in%
    gsub("([0-9]{4}-[0-9]{6}-[0-9]{2})-.*", "\\1", tmp_test)))

#### annotations #####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "NCT01516567",
      register = "CTGOV",
      con = dbc,
      verbose = TRUE,
      annotation.text = "ANNO",
      annotation.mode = "replace")),
  "Imported or updated 1 trial")

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

tmp_test <- suppressWarnings(
  dbGetFieldsIntoDf(
    fields = "annotation",
    con = dbc))

tmp_test <-
  tmp_test[
    tmp_test[["_id"]] %in%
      suppressMessages(
        suppressWarnings(
          dbFindIdsUniqueTrials(
            con = dbc))) , ]

# test
expect_equal(sort(tmp_test[["annotation"]]),
             sort(c("EU ANNO", "ANNO")))

# test
expect_message(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "CTGOV")),
  "Concatenating 1 records from CTGOV and [4-9][0-9] from EUCTR")


#### dbGetFieldsIntoDf ####

# test
expect_error(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(NA, "willNeverBeFound"),
      con = dbc)),
  "For field 'willNeverBeFound' no data could be extracted")

# test
expect_error(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(NA, "willNeverBeFound", ""),
      con = dbc)),
  "'fields' contains empty elements")

# test as many fields as possible for typing

# get all field names
tmpf <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = "*",
      con = dbc)))
tmpf <- tmpf[tmpf != ""]
# get all data (takes long with sqlite)
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = tmpf,
      con = dbc,
      stopifnodata = FALSE)
  ))
# determine all classes
tmpc <- sapply(result, class,
               USE.NAMES = FALSE)
tmpc <- unlist(tmpc)
tmpc <- table(tmpc)

# test
expect_equal(
  sort(names(tmpc))[1:5],
  c("character", "Date", "integer", "list", "logical", "POSIXct", "POSIXt")[1:5]
)
# tests
expect_true(tmpc[1] > 400)
expect_true(tmpc[2] >   5)
expect_true(tmpc[3] >  20)
expect_true(tmpc[4] >  15)
expect_true(tmpc[5] >  50)


#### ctrOpenSearchPagesInBrowser #####

# test
expect_message(
  suppressWarnings(
    ctrOpenSearchPagesInBrowser(
      dbQueryHistory(con = dbc)[1, ])),
  "Opening browser for search:")
