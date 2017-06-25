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
    capture.output(ctrMongo())
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

# testing local password free access to a standard
# mongodb installation which may fail if this is
# configured otherwise
test_that("access to mongo db from R package", {

  has_mongo()

  # initialise = drop collections from mongodb
  try(mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                       db = "users")$drop(),
      silent = TRUE)

  try(mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDBKeys",
                       db = "users")$drop(),
      silent = TRUE)

  expect_warning(suppressMessages(dbQueryHistory(
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
                 "No history found in expected format.")

})

# test access to mongo db from command line
test_that("access to mongo db from command line", {

  has_mongo()

  expect_message(installMongoFindBinaries(debug = TRUE), "mongoimport / mongo ")

})

# testing downloading from both registers
# a query with a no trials as result
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

# testing downloading from both registers
# a query retrieving a small number of trials
test_that("retrieve data from register ctgov", {

  has_internet()
  has_mongo()

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(
    queryterm = "term=2010-024264-18",
    register = "CTGOV",
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
    "Imported or updated 1 trial")

  ## create and test updatable query

  q <- paste0("https://clinicaltrials.gov/ct2/results?term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=12%2F31%2F2014")

  expect_message(suppressWarnings(ctrLoadQueryIntoDb(q,
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB")),
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

# testing downloading from both registers
# a query retrieving a small number of trials
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
    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB", debug = TRUE)),
    "First result page empty")


  ## slow import
  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "neuroblastoma&status=completed&phase=phase-one")
  expect_message(ctrLoadQueryIntoDb(q,
                                    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                    debug = TRUE, verbose = FALSE),
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

# testing options for user to
# search in desktop browser
test_that("browser interaction", {

  tmp <- ctrGetQueryUrlFromBrowser(content = "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0")

  expect_is(tmp, "data.frame")

  has_internet()

  expect_warning(ctrGetQueryUrlFromBrowser(content = "ThisDoesNotExist"),
                 "Content is not a clinical trial register search URL.")

  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening in browser previous search:")

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

# testing functions seeking
# record contents in database
test_that("operations on database after download from register", {

  has_mongo()
  has_internet()

  expect_message(dbFindIdsUniqueTrials(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
                 "Searching multiple country records")

  expect_error(dbGetVariablesIntoDf(fields = "ThisDoesNotExist",
                                    collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"),
               "For variable / field: ThisDoesNotExist no data could be extracted")

  # clean up = drop collections from mongodb
  expect_equivalent (mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB",
                                      db = "users")$drop(), TRUE)

  expect_equivalent (mongolite::mongo(collection = "ThisNameSpaceShouldNotExistAnywhereInAMongoDBKeys",
                                      db = "users")$drop(), TRUE)

})

# testing operations on minimalistic dataframes
test_that("operations on data frame", {

  df <- data.frame("var1" = 1, "var2" = 1)

  expect_message(dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2")), "Unique values returned:")
  expect_is     (dfMergeTwoVariablesRelevel(df = df, varnames = c("var1", "var2")), "character")

})
