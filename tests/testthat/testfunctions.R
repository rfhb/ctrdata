# testfunctions.R
# ralf.herold@gmx.net
# 2016-01-23

# run tests manually with:
# devtools::test()
# library(testthat)

# check code coverage:
# https://codecov.io/gh/rfhb/ctrdata/

# Mac OS X:
# brew services {start|stop} mongodb

library(ctrdata)
context("ctrdata functions")

# helper function to check if there
# is a useful internect connection
has_internet <- function(){
  if (is.null(curl::nslookup("r-project.org", error = FALSE))) {
    skip("No internet connection available. ")
  }
  if (!RCurl::url.exists(url = "https://www.clinicaltrialsregister.eu/",
                         .opts = list(connecttimeout = 3, ssl.verifypeer = FALSE))
      ||
      !RCurl::url.exists(url = "https://clinicaltrials.gov/",
                         .opts = list(connecttimeout = 3, ssl.verifypeer = FALSE))
      ) {
    skip("One or more registers not available. ")
  }
}

# helper function to check mongodb
has_mongo <- function(){
  # check server
  tmp <- getOption("warn")
  options("warn" = 2)
  mongo_ok <- try({
    capture.output(ctrMongo())
  },
  silent = TRUE)
  # use test result
  options("warn" = tmp)
  if (class(mongo_ok) == "try-error") {
    skip("No suitable version password-free localhost mongodb accessible.")
  }
}

# helper function to check proxy access
has_proxy <- function(){
  # get initial options
  old_options <- options()$RCurlOptions
  # this is a local proxy using jap
  opts <- list(proxy = "127.0.0.1", proxyport = 4001)
  # set proxy
  options(RCurlOptions = opts)
  # test for working proxy connection
  proxy_ok <- try({
    is.character(RCurl::getURL("http://www.google.com/"))
  },
  silent = TRUE)
  #
  # reset to initial options
  options(RCurlOptions = old_options)
  #
  if (class(proxy_ok) == "try-error") {
    skip("No proxied internet connection available.")
  }
}


#### mongodb local password free access ####
test_that("access to mongo db from R package", {

  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # initialise = drop collections from mongodb
  try(mongolite::mongo(collection = coll,                 db = "users")$drop(), silent = TRUE)
  try(mongolite::mongo(collection = paste0(coll, "Keys"), db = "users")$drop(), silent = TRUE)

  # test 1
  expect_message(dbQueryHistory(collection = coll),
                 "No history found in expected format.")

})


#### mongodb access ####
test_that("access to mongo db from command line", {

  has_mongo()

  # test 2
  expect_message(installMongoFindBinaries(debug = TRUE),
                 "mongo / mongoimport ")

})


#### empty downloads ####
test_that("retrieve data from registers", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # test 3
  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "query=NonExistingConditionGoesInHere",
    register = "EUCTR",
    collection = coll)),
    "First result page empty")

  # test 4
  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "cond=NonExistingConditionGoesInHere",
    register = "CTGOV",
    collection = coll)),
    "No trials or number of trials could not be determined")

  # clean up is the end of script = drop collection from mongodb

})


#### ctgov new, update ####
test_that("retrieve data from register ctgov", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # test 5
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "CTGOV",
    collection = coll),
    "Imported or updated 1 trial")


  ## create and test updatable query

  q <- paste0("https://clinicaltrials.gov/ct2/results?term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

  # test 6
  expect_message(ctrLoadQueryIntoDb(paste0(q, "12%2F31%2F2008"), collection = coll),
                 "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(collection = coll)
  # manipulate query
  hist[nrow(hist), "query-term"] <- sub("(.*&lup_e=).*", "\\112%2F31%2F2009", hist[nrow(hist), "query-term"])
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  mongolite::mongo(collection = coll, db = "users")$update(query = '{"_id":{"$eq":"meta-info"}}',
                                                           update = paste0('{ "$set" :', json, "}"),
                                                           upsert = TRUE)

  # test 7
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(querytoupdate = "last", collection = coll)),
                 "Imported or updated")

  remove("hist", "json", "q")

})


#### euctr new, fast, slow, update ####
test_that("retrieve data from register euctr", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # test 8
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "EUCTR",
    collection = coll),
    "Updated history")

  # test 9
  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    querytoupdate = "last",
    collection = coll)),
    "First result page empty")

  ## github issue 8 bug
  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?",
              "query=&dateFrom=2017-09-15&dateTo=2017-09-15&age=under-18")

  # test 10
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(queryterm = q,
                                                     collection = coll,
                                                     euctrresults = TRUE)),
                 "Updated history")

  ## forced slow import
  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "neuroblastoma&status=completed&phase=phase-one&country=pl")

  # test 11
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                                                     collection = coll,
                                                     debug = TRUE, verbose = FALSE)),
                 "Imported or updated")


  ## download without details
  # test 12
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                                                     collection = coll,
                                                     details = FALSE)),
                 "Imported or updated")


  ## create and test updatable query

  date.today <- format(Sys.time(),                          "%Y-%m-%d")
  date.temp  <- format(Sys.time() - (60 * 60 * 24 * 6),     "%Y-%m-%d")
  date.old   <- format(Sys.time() - (60 * 60 * 24 * 6 * 9), "%Y-%m-%d")

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "&dateFrom=", date.old, "&dateTo=", date.temp)

  # test 13
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                                                     collection = coll,
                                                     details = FALSE)),
                 "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(collection = coll)
  # manipulate query
  hist[nrow(hist), "query-term"]      <- sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
  hist[nrow(hist), "query-timestamp"] <- paste0(date.temp, " 23:59:59")
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  mongolite::mongo(collection = coll,
                   db = "users")$update(query = '{"_id":{"$eq":"meta-info"}}',
                                        update = paste0('{ "$set" :', json, "}"),
                                        upsert = TRUE)

  # test 14
  expect_message(ctrLoadQueryIntoDb(querytoupdate = "last",
                                    collection = coll,
                                    details = FALSE),
                 "Imported or updated")

  remove("hist", "json", "q", "date.old", "date.today", "date.temp")

})


#### euctr results ####
test_that("retrieve results from register euctr", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "2004-000015-25+OR+2007-000371-42+OR+XYZ")

  # test 15
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                                                     euctrresults = TRUE,
                                                     collection = coll,
                                                     debug = TRUE)),
                 "Imported or updated results for")

  tmp <- dbGetFieldsIntoDf(c("a2_eudract_number",
                             "endPoints.endPoint.title",
                             "firstreceived_results_date",
                             "e71_human_pharmacology_phase_i",
                             "version_results_history"),
                           collection = coll)

  # test 16
  expect_true(!any(tmp[tmp$a2_eudract_number == "2007-000371-42", c(1, 2, 3)] == ""))

  # test 16a
  expect_true(class(tmp$firstreceived_results_date)     == "Date")
  expect_true(class(tmp$e71_human_pharmacology_phase_i) == "logical")

})

#### browser show query ####
test_that("browser interaction", {

  # test 17
  expect_equal(suppressWarnings(ctrGetQueryUrlFromBrowser("something_insensible")), NULL)

  # ctgov

  q <- "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

  tmp <- ctrGetQueryUrlFromBrowser(content = q)

  # test 18
  expect_is(tmp, "data.frame")

  has_internet()

  # test 19
  expect_warning(ctrGetQueryUrlFromBrowser(content = "ThisDoesNotExist"),
                 "no clinical trial register search URL found")

  # test 20
  expect_message(ctrOpenSearchPagesInBrowser(q),
                 "Opening browser for search:")

  # test 21
  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening browser for search:")

  # euctr

  q <- "https://www.clinicaltrialsregister.eu/ctr-search/search?query=&age=under-18&status=completed"

  tmp <- ctrGetQueryUrlFromBrowser(content = q)

  # test 22
  expect_is(tmp, "data.frame")

  # test 23
  expect_message(ctrOpenSearchPagesInBrowser(q),
                 "Opening browser for search:")

  # test 24
  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening browser for search:")

  # both registers

  # test 25
  expect_equal(ctrOpenSearchPagesInBrowser(register = c("EUCTR", "CTGOV"), copyright = TRUE), TRUE)

  # test with database

  has_mongo()

  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # test 26
  expect_message(ctrOpenSearchPagesInBrowser(dbQueryHistory(collection = coll)[1, ]),
                 "Opening browser for search:")

  tmp <-  data.frame(lapply(dbQueryHistory(collection = coll),
                            tail, 1L), stringsAsFactors = FALSE)
  names(tmp) <- sub("[.]", "-", names(tmp))

  # test 27
  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening browser for search:")

})


#### db fields and records ####
test_that("operations on database after download from register", {

  has_mongo()
  has_internet()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # dbFindFields

  # test 28a (temporary)
  expect_warning(dbFindVariable(namepart = "date", collection = coll),
                 "'dbFindVariable' is deprecated.")

  # test 28
  expect_error(dbFindFields(namepart = c("onestring", "twostring"), collection = coll),
               "Name part should have only one element.")

  # test 29
  expect_error(dbFindFields(namepart = list("onestring", "twostring"), collection = coll),
               "Name part should be atomic.")

  # test 30
  expect_error(dbFindFields(namepart = "", collection = coll),
               "Empty name part string.")

  # test 31
  expect_message(dbFindFields(namepart = "date", collection = coll),
                 "Returning first of ")

  # test 32
  expect_equal(is.na(dbFindFields(namepart = "ThisNameShouldNotExistAnywhere", collection = coll)),
               TRUE)

  # dbFindIdsUniqueTrials

  # test 33
  expect_message(dbFindIdsUniqueTrials(collection = coll, preferregister = "EUCTR"),
                 "Searching multiple country records")

  # test 34
  expect_message(dbFindIdsUniqueTrials(collection = coll, preferregister = "CTGOV"),
                 "Returning keys")

  # test 35
  expect_warning(dbFindIdsUniqueTrials(collection = coll, prefermemberstate = "3RD", include3rdcountrytrials = FALSE),
                 "Preferred EUCTR version set to 3RD country trials, but include3rdcountrytrials was FALSE")


  # dbGetFieldsIntoDf

  # test 36a (temporary)
  expect_warning(dbGetVariablesIntoDb(fields = "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
                                      collection = coll)[1, ],
                 "'dbGetVariablesIntoDb' is deprecated.")

  # test 36
  expect_error(dbGetFieldsIntoDf(fields = "ThisDoesNotExist", collection = coll),
               "For field: ThisDoesNotExist no data could be extracted")

  # test 37
  expect_error(dbGetFieldsIntoDf(fields = "", collection = coll),
               "'fields' contains empty elements")

  # test 38
  expect_error(dbGetFieldsIntoDf(fields = list("ThisDoesNotExist"), collection = coll),
               "Input should be a vector of strings of field names.")


  # clean up = drop collections from mongodb

  # test 38
  expect_equivalent (mongolite::mongo(collection = coll,                 db = "users")$drop(), TRUE)

  # test 40
  expect_equivalent (mongolite::mongo(collection = paste0(coll, "Keys"), db = "users")$drop(), TRUE)

})


#### deduplication ####
test_that("operations on database for deduplication", {

  has_mongo()
  has_internet()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # get some trials with corresponding numbers
  ctrLoadQueryIntoDb(queryterm = "NCT00134030", register = "CTGOV", collection = coll) # EUDRACT-2004-000242-20
  ctrLoadQueryIntoDb(queryterm = "NCT01516580", register = "CTGOV", collection = coll) # 2010-019224-31
  ctrLoadQueryIntoDb(queryterm = "NCT00025597", register = "CTGOV", collection = coll) # this is not in euctr
  ctrLoadQueryIntoDb(queryterm = "2010-019224-31", register = "EUCTR", collection = coll)
  ctrLoadQueryIntoDb(queryterm = "2004-000242-20", register = "EUCTR", collection = coll)
  ctrLoadQueryIntoDb(queryterm = "2005-000915-80", register = "EUCTR", collection = coll) # this is not in ctgov
  ctrLoadQueryIntoDb(queryterm = "2014-005674-11", register = "EUCTR", collection = coll) # this is 3rd country only
  ctrLoadQueryIntoDb(queryterm = "2016-002347-41", register = "EUCTR", collection = coll) # in eu and 3rd country

  # test combinations of parameters

  # test 41
  tmp <- dbFindIdsUniqueTrials(collection = coll)
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB", "2014-005674-11-3RD",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  # test 42
  tmp <- dbFindIdsUniqueTrials(collection = coll, include3rdcountrytrials = FALSE) # removes 2014-005674-11
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  # test 43
  tmp <- dbFindIdsUniqueTrials(collection = coll, prefermemberstate = "3RD") # changes 2016-002347-41
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB", "2014-005674-11-3RD",
                               "2016-002347-41-3RD", "NCT00025597"),
                        check.attributes = FALSE))

  # test 44
  tmp <- dbFindIdsUniqueTrials(collection = coll, prefermemberstate = "IT")
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-IT", "2010-019224-31-IT", "2014-005674-11-3RD",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  # test 45
  tmp <- dbFindIdsUniqueTrials(collection = coll, preferregister = "CTGOV")
  expect_true(all.equal(tmp, c("NCT00025597", "NCT00134030", "NCT01516580", "2005-000915-80-GB",
                              "2014-005674-11-3RD", "2016-002347-41-GB"),
                        check.attributes = FALSE))

  # test 46
  tmp <- dbFindIdsUniqueTrials(collection = coll, preferregister = "CTGOV", prefermemberstate = "IT")
  expect_true(all.equal(tmp, c("NCT00025597", "NCT00134030", "NCT01516580", "2005-000915-80-IT",
                               "2014-005674-11-3RD", "2016-002347-41-GB"),
                        check.attributes = FALSE))


  # clean up = drop collections from mongodb

  # test 47
  expect_equivalent (mongolite::mongo(collection = coll,                 db = "users")$drop(), TRUE)

  # test 48
  expect_equivalent (mongolite::mongo(collection = paste0(coll, "Keys"), db = "users")$drop(), TRUE)

})



#### annotations ####
test_that("annotate queries", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # test 49
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18 OR NCT01516567",
    register = "CTGOV",
    collection = coll,
    annotation.text = "ANNO",
    annotation.mode = "replace"),
    "Imported or updated 2 trial")

  # test 50
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "NCT01516567",
    register = "CTGOV",
    collection = coll,
    annotation.text = "TEST",
    annotation.mode = "prepend"),
    "Imported or updated 1 trial")

  # test 51
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "EUCTR",
    collection = coll,
    annotation.text = "TEST",
    annotation.mode = "prepend"),
    "Imported or updated 6 records on 1 trial")

  # test 52
  expect_equal(sort(suppressMessages(dbGetFieldsIntoDf(fields = "annotation",
                                                          collection = coll))[, "annotation"]),
               sort(c("ANNO", "TEST", "TEST", "TEST", "TEST", "TEST", "TEST", "TEST ANNO")))

})


#### df operations ####
test_that("operations on data frame", {

  df <- data.frame("var1" = 1:3, "var2" = 2:4, stringsAsFactors = FALSE)

  statusvalues <- list("Firstvalues" = c("12", "23"),
                       "Lastvalue"   = c("34"))

  # dfMergeTwoVariablesRelevel

  # test 53
  expect_error(dfMergeTwoVariablesRelevel(list("var1", "var2")),
               "Need a data frame as input.")

  # test 54
  expect_message(dfMergeTwoVariablesRelevel(df = df, colnames = c("var1", "var2")),
                 "Unique values returned: 12, 23, 34")

  # test 55
  expect_is     (dfMergeTwoVariablesRelevel(df = df, colnames = c("var1", "var2")),
                 "character")

  # test 56
  expect_message(dfMergeTwoVariablesRelevel(df = df, colnames = c("var1", "var2"),
                                            levelslist = statusvalues),
                 "Unique values returned: Firstvalues, Lastvalue")

  # test 57
  expect_error(dfMergeTwoVariablesRelevel(df = df, colnames = 1:3),
                 "Please provide exactly two column names.")


})


#### active substance ####
test_that("operations on data frame", {

  has_internet()

  # test 57
  expect_equal(sort(ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")[1:4]),
               sort(c("gleevec", "glivec", "imatinib", "sti 571")))


})
