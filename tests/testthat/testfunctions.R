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
  mongo_ok <- try({
    capture.output(ctrdata:::ctrMongo())
  }, silent = TRUE)
  # use test result
  if (class(mongo_ok) == "try-error" ||
      mongo_ok [1] == "Unable to connect to 127.0.0.1:27017, error code = 2") {
    skip("No password-free localhost mongodb connection available.")
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


#### mongodb local password free access to a standard ####
test_that("access to mongo db from R package", {

  has_mongo()

  # initialise = drop collections from mongodb
  try(mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                       db = "users")$drop(),
      silent = TRUE)

  try(mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDBKeys",
                       db = "users")$drop(),
      silent = TRUE)

  expect_message(dbQueryHistory(
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
                 "No history found in expected format.")

})


#### mongodb access from command line ####
test_that("access to mongo db from command line", {

  has_mongo()

  expect_message(installMongoFindBinaries(debug = TRUE), "mongoimport / mongo ")

})


#### empty download from both registers ####
test_that("retrieve data from registers", {

  has_internet()
  has_mongo()

  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "query=NonExistingConditionGoesInHere",
    register = "EUCTR",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
    "First result page empty")

  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "cond=NonExistingConditionGoesInHere",
    register = "CTGOV",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
    "No studies downloaded")

  # at the end of srcipt, clean up occurs = drop collection from mongodb

})


#### ctgov download new and update ####
test_that("retrieve data from register ctgov", {

  has_internet()
  has_mongo()

  expect_message(ctrLoadQueryIntoDb(
    queryterm = "term=2010-024264-18",
    register = "CTGOV",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
    "Imported or updated 1 trial")

  ## create and test updatable query

  q <- paste0("https://clinicaltrials.gov/ct2/results?term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=12%2F31%2F2014")

  expect_message(ctrLoadQueryIntoDb(q,
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
    "Imported or updated 3")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")
  # manipulate query
  hist[nrow(hist), "query-term"]      <- sub(".*(term=.*)&lup_e=.*", "\\1", q)
  hist[nrow(hist), "query-timestamp"] <- "2014-12-31-23-59-59"
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                   db = "users")$update(query = '{"_id":{"$eq":"meta-info"}}',
                                        update = paste0('{ "$set" :', json, "}"),
                                        upsert = TRUE)

  expect_message(ctrLoadQueryIntoDb(
    querytoupdate = "last",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
    "Imported or updated")

  remove("hist", "json", "q")

})


#### euctr download new fast slow, update ####
test_that("retrieve data from register euctr", {

  has_internet()
  has_mongo()

  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "EUCTR",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
    "Updated history")

  expect_error(suppressWarnings(ctrLoadQueryIntoDb(
    querytoupdate = "last",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
    "First result page empty")


  ## slow import
  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "neuroblastoma&status=completed&phase=phase-one")
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                                  collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                  debug = TRUE, verbose = FALSE)),
                 "Imported or updated")


  ## create and test updatable query

  date.today <- format(Sys.time(),                "%Y-%m-%d")
  date.temp  <- format(Sys.time() - 60*60*24*6,   "%Y-%m-%d")
  date.old   <- format(Sys.time() - 60*60*24*6*2, "%Y-%m-%d")

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "&dateFrom=", date.old, "&dateTo=", date.temp)

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
                 collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
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

  expect_message(ctrLoadQueryIntoDb(
    querytoupdate = "last",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
    "Imported or updated")

  remove("hist", "json", "q", "date.old", "date.today", "date.temp")

})

#### browser open show get query ####
test_that("browser interaction", {

  expect_equal(ctrGetQueryUrlFromBrowser("something_insensible"), NULL)

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

  expect_message(ctrOpenSearchPagesInBrowser(dbQueryHistory(
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")[1, ]),
    "Opening in browser previous search: ")

  tmp <-  data.frame(lapply(dbQueryHistory(
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
    tail, 1))
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

  # dbFindVariable
  expect_error(dbFindVariable(namepart = c("onestring", "twostring"),
                              collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "Name part should have only one element.")

  expect_error(dbFindVariable(namepart = list("onestring", "twostring"),
                              collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "Name part should be atomic.")

  expect_error(dbFindVariable(namepart = "",
                              collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "Empty name part string.")

  expect_message(dbFindVariable(namepart = "date",
                                collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
                 "Returning first of ")

  expect_equal(is.na(dbFindVariable(namepart = "ThisNameShouldNotExistAnywhere",
                                    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
               TRUE)


  # dbFindIdsUniqueTrials
  expect_message(dbFindIdsUniqueTrials(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                       preferregister = "EUCTR"),
                 "Searching multiple country records")

  expect_message(dbFindIdsUniqueTrials(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                       preferregister = "CTGOV"),
                 "Returning keys")


  dbFindIdsUniqueTrials <- function(preferregister = "EUCTR", prefermemberstate = "GB", include3rdcountrytrials = TRUE,
                                    collection = "ctrdata", db = "users", url = "mongodb://localhost",
                                    username = "", password = "", verbose = TRUE)

  expect_warning(dbFindIdsUniqueTrials(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                       prefermemberstate = "3RD", include3rdcountrytrials = FALSE),
                 "Preferred EUCTR version set to 3RD country trials, but include3rdcountrytrials was FALSE")


  # dbGetVariablesIntoDf
  expect_error(dbGetVariablesIntoDf(fields = "ThisDoesNotExist",
                                    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "For variable / field: ThisDoesNotExist no data could be extracted")

  expect_error(dbGetVariablesIntoDf(fields = "",
                                    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "'fields' contains empty elements")

  expect_error(dbGetVariablesIntoDf(fields = list("ThisDoesNotExist"),
                                    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "Input should be a vector of strings of field names.")



  # clean up = drop collections from mongodb
  expect_equivalent (mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                      db = "users")$drop(), TRUE)

  expect_equivalent (mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDBKeys",
                                      db = "users")$drop(), TRUE)

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
