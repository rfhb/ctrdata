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
}

# helper function to check mongodb
has_mongo <- function(){
  # check server
  tmp <- getOption("warn")
  options("warn" = 2)
  mongo_ok <- try({
    capture.output(ctrMongo())
  }, silent = TRUE)
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
  }, silent = TRUE)
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

  expect_message(dbQueryHistory(collection = coll),
                 "No history found in expected format.")

})


#### mongodb access from command line ####
test_that("access to mongo db from command line", {

  has_mongo()

  expect_message(installMongoFindBinaries(debug = TRUE),
                 "mongo / mongoimport ")

})


#### empty download from both registers ####
test_that("retrieve data from registers", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "query=NonExistingConditionGoesInHere",
    register = "EUCTR",
    collection = coll)),
    "First result page empty")

  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "cond=NonExistingConditionGoesInHere",
    register = "CTGOV",
    collection = coll)),
    "No studies downloaded")

  # clean up is the end of script = drop collection from mongodb

})


#### ctgov download new and update ####
test_that("retrieve data from register ctgov", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "CTGOV",
    collection = coll),
    "Imported or updated 1 trial")


  ## create and test updatable query

  q <- paste0("https://clinicaltrials.gov/ct2/results?term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

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

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(querytoupdate = "last", collection = coll)),
                 "Imported or updated")

  remove("hist", "json", "q")

})


#### euctr download new, fast, slow, update ####
test_that("retrieve data from register euctr", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "EUCTR",
    collection = coll),
    "Updated history")

  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    querytoupdate = "last",
    collection = coll)),
    "First result page empty")

  ## github issue 8 bug
  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?",
              "query=&dateFrom=2017-09-15&dateTo=2017-09-17")

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(queryterm = q,
                                                     collection = coll,
                                                     euctrresults = TRUE)),
                 "Updated history")

  ## forced slow import
  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "neuroblastoma&status=completed&phase=phase-one")

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                                                     collection = coll,
                                                     debug = TRUE, verbose = FALSE)),
                 "Imported or updated")


  ## create and test updatable query

  date.today <- format(Sys.time(),                "%Y-%m-%d")
  date.temp  <- format(Sys.time() - 60*60*24*6,   "%Y-%m-%d")
  date.old   <- format(Sys.time() - 60*60*24*6*2, "%Y-%m-%d")

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "&dateFrom=", date.old, "&dateTo=", date.temp)

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q, collection = coll)),
                 "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")
  # manipulate query
  hist[nrow(hist), "query-term"]      <- sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
  hist[nrow(hist), "query-timestamp"] <- paste0(date.temp, "-23-59-59")
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                   db = "users")$update(query = '{"_id":{"$eq":"meta-info"}}',
                                        update = paste0('{ "$set" :', json, "}"),
                                        upsert = TRUE)

  expect_message(ctrLoadQueryIntoDb(querytoupdate = "last", collection = coll),
                 "Imported or updated")

  remove("hist", "json", "q", "date.old", "date.today", "date.temp")

})


#### euctr download results ####
test_that("retrieve results from register euctr", {

  has_internet()
  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "2004-000015-25+OR+2004-000518-37+OR+2004-004386-15+OR+2007-000371-42+OR+XYZ")

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q, euctrresults = TRUE, collection = coll)),
                 "Imported or updated results for")

  tmp <- dbGetVariablesIntoDf(c("a2_eudract_number",
                                "endPoints.endPoint.title",
                                "firstreceived_results_date",
                                "version_results_history"),
                       collection = coll)

  expect_true(!any(tmp[tmp$a2_eudract_number == "2007-000371-42", ] == ""))

})

#### browser open show get query ####
test_that("browser interaction", {

  expect_equal(suppressWarnings(ctrGetQueryUrlFromBrowser("something_insensible")), NULL)

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  q <- "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

  tmp <- ctrGetQueryUrlFromBrowser(content = q)

  expect_is(tmp, "data.frame")

  has_internet()

  expect_warning(ctrGetQueryUrlFromBrowser(content = "ThisDoesNotExist"),
                 "Content is not a clinical trial register search URL.")

  expect_message(ctrOpenSearchPagesInBrowser(q),
                 "Opening in browser previous search:")

  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening in browser previous search:")

  expect_equal(ctrOpenSearchPagesInBrowser(register = c("EUCTR", "CTGOV"), copyright = TRUE), TRUE)

  has_mongo()

  expect_message(ctrOpenSearchPagesInBrowser(dbQueryHistory(collection = coll)[1, ]),
                 "Opening in browser previous search: ")

  tmp <-  data.frame(lapply(dbQueryHistory(collection = coll),
                            tail, 1L), stringsAsFactors = FALSE)
  names(tmp) <- sub("[.]", "-", names(tmp))

  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening in browser previous search: ")

})

# testing downloading from both registers using a proxy
# a query retrieving a small number of trials
# test_that("retrieve via proxy data from register euctr", {
#
#   has_proxy()
#   has_mongo()
#
#   skip_on_travis()
#
#   # get initial options
#   old_options <- options()$RCurlOptions
#
#   # this is a local proxy using jap
#   opts <- list(proxy = "127.0.0.1", proxyport = 4001)
#
#   # set proxy
#   options(RCurlOptions = opts)
#
#   queryeuctr <- list(queryterm = "2010-024264-18",      register = "EUCTR")
#
#   expect_message(ctrLoadQueryIntoDb(queryeuctr,
#                                     collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
#                                     debug = TRUE),
#                  "Updated history")
#
#   # reset to initial options
#   options(RCurlOptions = old_options)
#
# })


#### functions for database records ####
test_that("operations on database after download from register", {

  has_mongo()
  has_internet()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # dbFindVariable
  expect_error(dbFindVariable(namepart = c("onestring", "twostring"), collection = coll),
               "Name part should have only one element.")

  expect_error(dbFindVariable(namepart = list("onestring", "twostring"), collection = coll),
               "Name part should be atomic.")

  expect_error(dbFindVariable(namepart = "", collection = coll),
               "Empty name part string.")

  expect_message(dbFindVariable(namepart = "date", collection = coll),
                 "Returning first of ")

  expect_equal(is.na(dbFindVariable(namepart = "ThisNameShouldNotExistAnywhere", collection = coll)),
               TRUE)


  # dbFindIdsUniqueTrials
  expect_message(dbFindIdsUniqueTrials(collection = coll, preferregister = "EUCTR"),
                 "Searching multiple country records")

  expect_message(dbFindIdsUniqueTrials(collection = coll, preferregister = "CTGOV"),
                 "Returning keys")

  expect_warning(dbFindIdsUniqueTrials(collection = coll, prefermemberstate = "3RD", include3rdcountrytrials = FALSE),
                 "Preferred EUCTR version set to 3RD country trials, but include3rdcountrytrials was FALSE")


  # dbGetVariablesIntoDf
  expect_error(dbGetVariablesIntoDf(fields = "ThisDoesNotExist", collection = coll),
               "For variable / field: ThisDoesNotExist no data could be extracted")

  expect_error(dbGetVariablesIntoDf(fields = "", collection = coll),
               "'fields' contains empty elements")

  expect_error(dbGetVariablesIntoDf(fields = list("ThisDoesNotExist"), collection = coll),
               "Input should be a vector of strings of field names.")


  # clean up = drop collections from mongodb
  expect_equivalent (mongolite::mongo(collection = coll,                 db = "users")$drop(), TRUE)
  expect_equivalent (mongolite::mongo(collection = paste0(coll, "Keys"), db = "users")$drop(), TRUE)

})


#### functions for deduplication ####
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

  tmp <- dbFindIdsUniqueTrials(collection = coll)
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB", "2014-005674-11-3RD",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  tmp <- dbFindIdsUniqueTrials(collection = coll, include3rdcountrytrials = FALSE) # removes 2014-005674-11
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  tmp <- dbFindIdsUniqueTrials(collection = coll, prefermemberstate = "3RD") # changes 2016-002347-41
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB", "2014-005674-11-3RD",
                               "2016-002347-41-3RD", "NCT00025597"),
                        check.attributes = FALSE))

  tmp <- dbFindIdsUniqueTrials(collection = coll, prefermemberstate = "IT")
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-IT", "2010-019224-31-IT", "2014-005674-11-3RD",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  tmp <- dbFindIdsUniqueTrials(collection = coll, preferregister = "CTGOV")
  expect_true(all.equal(tmp, c("NCT00025597", "NCT00134030", "NCT01516580", "2005-000915-80-GB",
                              "2014-005674-11-3RD", "2016-002347-41-GB"),
                        check.attributes = FALSE))

  tmp <- dbFindIdsUniqueTrials(collection = coll, preferregister = "CTGOV", prefermemberstate = "IT")
  expect_true(all.equal(tmp, c("NCT00025597", "NCT00134030", "NCT01516580", "2005-000915-80-IT",
                               "2014-005674-11-3RD", "2016-002347-41-GB"),
                        check.attributes = FALSE))


  # clean up = drop collections from mongodb
  expect_equivalent (mongolite::mongo(collection = coll,                 db = "users")$drop(), TRUE)
  expect_equivalent (mongolite::mongo(collection = paste0(coll, "Keys"), db = "users")$drop(), TRUE)

})


#### functions for dataframes ####
test_that("operations on data frame", {

  df <- data.frame("var1" = 1:3, "var2" = 2:4, stringsAsFactors = FALSE)

  statusvalues <- list("Firstvalues" = c("12", "23"),
                       "Lastvalue"   = c("34"))

  # dfMergeTwoVariablesRelevel
  expect_error(dfMergeTwoVariablesRelevel(list("var1", "var2")),
               "Need a data frame as input.")

  expect_message(dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2")),
                 "Unique values returned: 12, 23, 34")

  expect_is     (dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2")),
                 "character")

  expect_message(dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2"),
                                            levelslist = statusvalues),
                 "Unique values returned: Firstvalues, Lastvalue")

  expect_error(dfMergeTwoVariablesRelevel(df = df, varnames = 1:3),
                 "Please provide exactly two variable names.")


})
