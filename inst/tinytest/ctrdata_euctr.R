## RH 2019-09-28

# check server
testUrl <- "https://www.clinicaltrialsregister.eu/ctr-search/search"
testGet <- function() try(httr::GET(testUrl, httr::timeout(5)), silent = TRUE)
testOnce <- testGet()

if (inherits(testOnce, "try-error") &&
    grepl("SSL certificate.*local issuer certificate", testOnce)) {
  # message("Switching off certificate verification")
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  testOnce <- testGet()
}

if (httr::status_code(testOnce) != 200L
) exit_file("Reason: EUCTR not working")

rm(testUrl, testGet, testOnce)

#### ctrLoadQueryIntoDb ####

# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "query=",
        register = "EUCTR"))),
  "more than 10,000) trials")

# test
expect_true(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "2005-001267-63+OR+2008-003606-33",
        register = "EUCTR",
        only.count = TRUE)))[["n"]] >= 2L)

# correct _id association

# test
suppressWarnings(
  suppressMessages(
    ctrLoadQueryIntoDb(
      queryterm = "2005-001267-63+OR+2008-003606-33",
      register = "EUCTR",
      euctrresults = TRUE,
      con = dbc
    )))

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
            "neuroblastoma&status=completed&phase=phase-one&country=pl")

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
      con = dbc)),
  "(Imported or updated|First result page empty)")

# test
expect_true(tmpTest$n >= 0L)

# test
expect_true(is.character(tmpTest$success))
rm(tmpTest, q, hist, json, date.from, date.to, date.today)

#### ctrLoadQueryIntoDb results ####

# get trials with results
q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
            "2013-003420-37+OR+2009-011454-17+OR+2006-005357-29")

# temp folder
tempD <- tempdir()

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = q,
      euctrresults = TRUE,
      euctrresultshistory = TRUE,
      euctrresultspdfpath = tempD,
      con = dbc)),
  "Imported or updated results for 3")
rm(tempD, q)


# get results
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(
        "a2_eudract_number",
        "trialInformation.globalEndOfTrialDate",
        "p_date_of_the_global_end_of_the_trial",
        "trialInformation.recruitmentStartDate",
        "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
        "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm",
        "endPoints.endPoint",
        "trialInformation.analysisForPrimaryCompletion",
        "e71_human_pharmacology_phase_i"
      ),
      con = dbc)
  ))

# test
expect_true(nrow(result) > 30L)

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
expect_true(all(as.Date(c("2013-10-28")) %in%
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

#### dfListExtractKey ####

# test
expect_true(
  sum(nchar(
    # note: function
    # is deprecated
    suppressWarnings(
      dfListExtractKey(
        df = result2,
        list.key = list(
          c("endPoints.endPoint", "title"))
      )[["value"]]
    )), na.rm = TRUE)
  > 3000L)

# test
expect_error(
  suppressWarnings(
    dfListExtractKey(
      df = iris)),
  "'df' lacks '_id' column")

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
df2 <- dfName2Value(
  df = df,
  valuename = "subjectDisposition.*postAssignmentPeriod.arms.arm.type.value"
)
# test
expect_true(
  length(unique(df2[["_id"]])) >= 2L
)

# extract
df2 <- dfName2Value(
  df = df,
  valuename = "subjectDisposition.*postAssignmentPeriod.arms.arm.type.value",
  wherename = "endPoints.endPoint.title",
  wherevalue = "percentage"
)

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
df2 <- dfName2Value(
  df = df,
  valuename = "^endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value$",
  wherename = "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value",
  wherevalue = "HYPOTHESIS_METHOD.*"
)

# test
expect_true(
  all(df2[["value"]] < 1)
)

# test
expect_error(
  dfName2Value(
    df = df,
    valuename = "doesnotexist"),
  "No rows found for 'valuename'")

# test
expect_error(
  dfName2Value(
    df = df,
    valuename = "^endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value$",
    wherename = "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value",
    wherevalue = "doesnotexist"),
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
  "Imported or updated 5 trial")

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
expect_false(
  any(c("2010-024264-18-GB") %in% trialsCtgov))

# test
expect_true(
  all(c("NCT00001209", "NCT00001436", "NCT00187109", "NCT01516567") %in%
        trialsEuctr))
# test
expect_false(
  any(c("NCT01471782") %in% trialsEuctr))

# clean up
rm(trialsCtgov, trialsEuctr)

# test
expect_error(
  dbFindIdsUniqueTrials(
    con = dbc,
    preferregister = "WRONG"),
  "not known")

# test
expect_error(
  suppressMessages(
    suppressWarnings(
      dbFindIdsUniqueTrials(
        con = dbc,
        prefermemberstate = "WRONG"))),
  "not known")

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
  "Preferred EUCTR version set to 3RD country trials")

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
    suppressMessages(
      dbGetFieldsIntoDf(
        fields = c(NA, "willNeverBeFound"),
        con = dbc))),
  paste0(
    "No data could be extracted for",
    "|No records with values for any specified field"))

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
      namepart = ".*",
      con = dbc)))
# get all data (takes long with sqlite)
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = tmpf,
      con = dbc,
      verbose = FALSE,
      stopifnodata = FALSE)
  ))
#
tmpr <- names(result)
tmpr <- tmpr[tmpr != "_id"]
# determine all classes
tmpc <- sapply(result, class,
               USE.NAMES = FALSE)
tmpc <- unlist(tmpc)
tmpc <- table(tmpc)

# src_mongo:
# tmpc
# character      Date   integer      list   logical
#       243        15        22       253       138
# src_sqlite:
# tmpc
# character      Date   integer      list   logical
#       411        18        22       538        77
# mongo_remote_rw
# tmpc
# character      Date   integer      list   logical
#        65        10         6         7        57

# tests note tmpr has more columns
# because data frames are expanded
expect_true(length(tmpf) <= length(tmpr))
# expect_identical(tmpf, tmpr)
# adapted to mongo remote server
expect_true(tmpc[["character"]] > 50)
expect_true(tmpc[["integer"]]   >  5)
expect_true(tmpc[["Date"]]      >  5)
expect_true(tmpc[["list"]]      >  5)
expect_true(tmpc[["logical"]]   > 50)

# TODO
# not testing for lists, because
# src_mongo returns only non-object
# fields when searching for all fields
# with dbFindFields(".*") whereas
# src_sqlite returns names of both
# objects and terminal / no more nested
# fields

# clean up
rm(tmpf, tmpr, tmpc, result)


#### ctrOpenSearchPagesInBrowser #####

# test
expect_equal(
  suppressWarnings(
    ctrOpenSearchPagesInBrowser(
      dbQueryHistory(con = dbc)[1, ])),
  # first query into the database
  paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?",
         "query=2005-001267-63+OR+2008-003606-33"))
