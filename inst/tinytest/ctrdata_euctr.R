## RH 2019-09-28

#### SETUP ####
# this file is called from various files

#### ctrLoadQueryIntoDb ####

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "neuroblastoma&status=completed&phase=phase-one&country=pl")

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      con = dbc)),
  "Imported or updated",
  info = "ctrdata_euctr.R#18")

# test
expect_true(tmp_test$n > 10L,
            info = "ctrdata_euctr.R#22")

# test
expect_true(all(c("2007-000371-42-FR", "2010-019340-40-GB", "2010-019340-40-3RD")
                %in% tmp_test$success),
            info = "ctrdata_euctr.R#27")

# test
expect_true(length(tmp_test$failed) == 0L,
            info = "ctrdata_euctr.R#31")

#### ctrLoadQueryIntoDb update ####

# only works for last 7 days with rss mechanism
# query based on date is used since this avoids no trials are found

date.today <- Sys.time()
date.from  <- format(date.today - (60 * 60 * 24 * 12), "%Y-%m-%d")
date.to    <- format(date.today - (60 * 60 * 24 *  9), "%Y-%m-%d")

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "&dateFrom=", date.from, "&dateTo=", date.to)

# test
expect_message(
  tmp_test <- suppressWarnings(
    ctrLoadQueryIntoDb(
      paste0(q),
      con = dbc)),
  "Imported or updated ",
  info = "ctrdata_euctr.R#52")

# manipulate history to test updating
# implemented in dbCTRUpdateQueryHistory
hist <- suppressWarnings(dbQueryHistory(con = dbc))
hist[nrow(hist), "query-term"]      <- sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
hist[nrow(hist), "query-timestamp"] <- paste0(date.to, " 23:59:59")

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
  "(Imported or updated|First result page empty)",
  info = "ctrdata_euctr.R#79")

# test
expect_true(tmp_test$n > 10L,
            info = "ctrdata_euctr.R#83")

# test
expect_true(length(tmp_test$success) > 10L,
            info = "ctrdata_euctr.R#87")

# test
expect_true(length(tmp_test$failed) == 0L,
            info = "ctrdata_euctr.R#91")

#### ctrLoadQueryIntoDb results ####

# get trials with results
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "2007-000371-42+OR+2011-004742-18")

expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      euctrresults = TRUE,
      con = dbc)),
  "Imported or updated results for",
  info = "ctrdata_euctr.R#106")

# tmp <- nodbi::docdb_get(src = dbc, key = dbc$collection)
# tmp <- nodbi::docdb_query(src = dbc, key = dbc$collection,
#                           query = '{}', listfields = 1L)

# get results
result <- suppressWarnings(
  dbGetFieldsIntoDf(
    fields = c("a2_eudract_number",
               "trialInformation.primaryCompletionDate",
               "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
               "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.title",
               "endPoints.endPoint.armReportingGroups.armReportingGroup",
               "endPoints.endPoint.type.value" ,
               "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value",
               "endPoints.endPoint",
               "trialInformation.analysisForPrimaryCompletion",
               "firstreceived_results_date",
               "e71_human_pharmacology_phase_i",
               "version_results_history"),
    con = dbc,
    stopifnodata = FALSE))

# keep only one record for trial
result <- suppressWarnings(suppressMessages(
  result[ result[["_id"]]
          %in% dbFindIdsUniqueTrials(con = dbc), ]
))

# test
expect_true(all(as.Date(c("2015-07-29", "2016-07-28"))
                %in% result$firstreceived_results_date),
            info = "ctrdata_euctr.R#138")

# test
expect_true("logical" == class(result[[
  "e71_human_pharmacology_phase_i"]]),
  info = "ctrdata_euctr.R#144")

# test
expect_true("Date" == class(result[[
  "trialInformation.primaryCompletionDate"]]),
  info = "ctrdata_euctr.R#149")

# test
expect_true(
  sum(nchar(getSublistKey(
    result,
    list(
      c("subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
        "title"))
  )),
  na.rm = TRUE)
  > 200L,
  info = "ctrdata_euctr.R#161")

# test
expect_true(
  sum(nchar(getSublistKey(
    result,
    list(
      c("endPoints.endPoint", "title"))
  )),
  na.rm = TRUE)
  > 2000L,
  info = "ctrdata_euctr.R#172")

# test
# tmp2 <- unlist(result[["endPoints.endPoint"]], recursive = TRUE)
# getNames(tmp2)
tmp_test <- getSublistKey(
  fulllist = result,
  keyssublists =
    list(
      c("endPoints.endPoint", "^title"),
      c("endPoints.endPoint", "statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value[0-9]*$"),
      c("endPoints.endPoint", "statisticalAnalyses.statisticalAnalysis.title[0-9]*$"),
      c("endPoints.endPoint", "^type.value")
    )
)
expect_true(all(tmp_test$endPoints.endPoint.type.value %in%
                  c("ENDPOINT_TYPE.primary", "ENDPOINT_TYPE.secondary", NA)),
            info = "ctrdata_euctr.R#189")

# extractKey(tmp2, "^armReportingGroups.armReportingGroup.tendencyValues.tendencyValue.value")
# extractKey(tmp2, "armReportingGroups.armReportingGroup.subjects")
# extractKey(tmp2, "armReportingGroups.armReportingGroup.@attributes.id")
# extractKey(tmp2, "armReportingGroups.armReportingGroup.comment")

# test
tmp_test <- getSublistKey(
  fulllist = result,
  keyssublists =
    list(
      c("endPoints.endPoint", "armReportingGroups.armReportingGroup.subjects[0-9]*$"),
      c("endPoints.endPoint", "armReportingGroups.armReportingGroup.@attributes.id"),
      c("endPoints.endPoint", "armReportingGroups.armReportingGroup.@attributes.armId")
    )
)
expect_true(
  sum(
    as.numeric(
      tmp_test[["endPoints.endPoint.armReportingGroups.armReportingGroup.subjects"]]),
    na.rm = TRUE) > 1250L,
  info = "ctrdata_euctr.R#211")

#### dbFindFields #####

# test
expect_error(
  dbFindFields(
    namepart = c("onestring", "twostring"),
    con = dbc),
  "Name part should have only one element.",
  info = "ctrdata_euctr.R#221")

# test
expect_error(
  dbFindFields(
    namepart = list("onestring", "twostring"),
    con = dbc),
  "Name part should be atomic.",
  info = "ctrdata_euctr.R#229")

# test
expect_error(
  dbFindFields(namepart = "",
               con = dbc),
  "Empty name part string.",
  info = "ctrdata_euctr.R#236")

# test
tmp_test <- suppressMessages(suppressWarnings(
  dbFindFields(
    namepart = "date",
    con = dbc)))
expect_true("character" %in% class(tmp_test),
            info = "ctrdata_euctr.R#243")
expect_true(length(tmp_test) >= 5L,
            info = "ctrdata_euctr.R#246")

#### dbFindIdsUniqueTrials #####

# test
expect_message(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "EUCTR")),
  "Searching for duplicates",
  info = "ctrdata_euctr.R#257")

# test
expect_message(
  suppressWarnings(
    dbFindIdsUniqueTrials(
      con = dbc,
      preferregister = "CTGOV")),
  "Returning keys",
  info = "ctrdata_euctr.R#266")

# test
expect_warning(
  suppressMessages(
    tmp_test <- dbFindIdsUniqueTrials(
      con = dbc,
      prefermemberstate = "3RD",
      include3rdcountrytrials = FALSE)),
  "Preferred EUCTR version set to 3RD country trials",
  info = "ctrdata_euctr.R#276")

# test
expect_true(all(
  c("2007-000371-42-PL",  "2010-019340-40-3RD", "2011-004742-18-PL",
    "2015-001653-32-3RD", "2018-003180-54-SE",  "2018-003986-33-SK")
  %in% tmp_test),
  info = "ctrdata_euctr.R#283")

#### annotations #####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "NCT01516567",
      register = "CTGOV",
      con = dbc,
      annotation.text = "ANNO",
      annotation.mode = "replace")),
  "Imported or updated 1 trial",
  info = "ctrdata_euctr.R#297")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "EUCTR",
      con = dbc,
      annotation.text = "ANNO",
      annotation.mode = "replace")),
  "Imported or updated",
  info = "ctrdata_euctr.R#308")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "EUCTR",
      con = dbc,
      annotation.text = "EU",
      annotation.mode = "prepend")),
  "Imported or updated",
  info = "ctrdata_euctr.R#320")

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
             sort(c("EU ANNO", "ANNO")),
             info = "ctrdata_euctr.R#339")


#### ctrOpenSearchPagesInBrowser #####

# test
expect_message(
  suppressWarnings(
    ctrOpenSearchPagesInBrowser(
      dbQueryHistory(con = dbc)[1, ])),
  "Opening browser for search:",
  info = "ctrdata_euctr.R#350")
