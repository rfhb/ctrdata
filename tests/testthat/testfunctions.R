# testfunctions.R
# ralf.herold@gmx.net
# 2016-01-23

# run tests manually with:
# devtools::test()

# Mac OS X:
# brew services {start|stop} mongodb

library(ctrdata)
context("ctrdata functions")

# Name                           | Testing?
# ------------------------------ | -------------
# ctrOpenSearchPagesInBrowser	   | some
# ctrQueryHistoryInDb            | some
# ctrGetQueryUrlFromBrowser      | some
# ctrLoadQueryIntoDb             | some
# dbFindIdsUniqueTrials          | some
# dbFindVariable                 | some
# dbGetVariablesIntoDf           | some
# dfMergeTwoVariablesRelevel     | some
# dfFindUniqueEuctrRecord        | some
# installMongoCheckVersion       | (implicit)
# installMongoFindBinaries       | (implicit)
# installCygwinWindowsDoInstall  | not planned
# installCygwinWindowsTest       | not planned


# helper function to check if there
# is a useful internect connection
has_internet <- function(){
  if(is.null(curl::nslookup("r-project.org", error = FALSE))) {
    skip("No internet connection available. ")
  }
}

# helper function to check mongodb
has_mongo <- function(){
  mongo_ok <- try({
    #library(rmongodb)
    capture.output(rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"))
  }, silent = TRUE)
  # use test result
  if (class(mongo_ok) == "try-error" || mongo_ok [1] == "Unable to connect to 127.0.0.1:27017, error code = 2") {
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
  if(class(proxy_ok) == "try-error") {
    skip("No proxied internet connection available.")
  }
}

# testing local password free access to a standard
# mongodb installation which may fail if this is
# configured otherwise
test_that("access to mongo db from R package", {

  has_mongo()

  expect_message(ctrQueryHistoryInDb(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "Total number of records")

})

# test access to mongo db from command line
test_that("access to mongo db from command line", {

  skip_on_os("windows")

  expect_message(installMongoFindBinaries(), "mongoimport / mongo found in the path")

})

# testing options for user to
# search in desktop browser
test_that("browser interaction", {

  expect_is(ctrGetQueryUrlFromBrowser(content = "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"), "list")

  has_internet()

  expect_error(ctrGetQueryUrlFromBrowser(content = "ThisDoesNotExist"), "Content is not a clinical trial register search URL.")

  expect_equal(ctrOpenSearchPagesInBrowser(register = "EUCTR", queryterm = "cancer&age=under-18"), TRUE)

})

# testing downloading from both registers
# a query with a no trials as result
test_that("retrieve data from registers", {

  has_internet()
  has_mongo()

  queryeuctr <- list(queryterm = "query=NonExistingConditionGoesInHere", register = "EUCTR")
  queryctgov <- list(queryterm = "cond=NonExistingConditionGoesInHere",  register = "CTGOV")

  expect_error(ctrLoadQueryIntoDb(queryeuctr, ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "First result page empty - no trials found")
  expect_error(ctrLoadQueryIntoDb(queryctgov, ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "No studies downloaded")

  # at the end of srcipt, clean up occurs = drop collection from mongodb

})

# testing downloading from both registers
# a query retrieving a small number of trials
test_that("retrieve data from register ctgov", {

  has_internet()
  has_mongo()

  queryctgov <- list(queryterm = "term=2010-024264-18", register = "CTGOV")

  expect_message(ctrLoadQueryIntoDb(queryctgov, ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "Imported or updated 1 trial")

})

# testing downloading from both registers
# a query retrieving a small number of trials
test_that("retrieve data from register euctr", {

  has_internet()
  has_mongo()

  queryeuctr <- list(queryterm = "2010-024264-18",      register = "EUCTR")

  expect_message(ctrLoadQueryIntoDb(queryeuctr, ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB", debug = TRUE), "Updated history")

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
#   expect_message(ctrLoadQueryIntoDb(queryeuctr, ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB", debug = TRUE), "Updated history")
#
#   # reset to initial options
#   options(RCurlOptions = old_options)
#
# })

# testing functions seeking
# record contents in database
test_that("operations on database", {

  has_mongo()

  expect_message(dbFindIdsUniqueTrials(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "Searched for EUCTR identifiers")
  expect_is     (dbFindIdsUniqueTrials(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "character")

  expect_error(dbGetVariablesIntoDf(fields = "ThisDoesNotExist",
                                    ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "For variable / field: ThisDoesNotExist no data could be extracted")

  # clean up = drop collections from mongodb
  expect_equivalent (rmongodb::mongo.drop(mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "users"),
                     ns= "users.ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), TRUE)

  expect_equivalent (rmongodb::mongo.drop(mongo = rmongodb::mongo.create(host = "127.0.0.1:27017", db = "varietyResults"),
                     ns= "varietyResults.ThisNameSpaceShouldNotExistAnywhereInAMongoDBKeys"), TRUE)

})

# testing operations on minimalistic
# dataframes to simulate deduplication
# function use
test_that("operations on data frame", {

  df <- data.frame("var1" = 1, "var2" = 1)

  expect_message(dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2")), "Unique values returned:")
  expect_is     (dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2")), "character")

  expect_error(dfFindUniqueEuctrRecord(df = df), "Data frame does not include")

  df <- data.frame(1, 2); names (df) <- c("_id", "a2_eudract_number")

  expect_message  (dfFindUniqueEuctrRecord(df = df), "0 EUCTR records dropped")
  expect_identical(dfFindUniqueEuctrRecord(df = df), df)

})

