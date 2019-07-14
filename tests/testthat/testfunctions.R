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

# ensure warnings are not turned into errors
# getOption("warn")
# options(warn = 0)

#### initialisation ####

# specify base uri for remote mongodb server, trailing slash
mdburi <- "mongodb+srv://DWbJ7Wh:bdTHh5cS@cluster0-b9wpw.mongodb.net"
# permissions are restricted to "find" in "dbperm" in "dbperm"
# no other functions can be executed, no login possible


# helper function to check if there
# is a useful internect connection
has_internet <- function(){
  if (is.null(curl::nslookup("r-project.org", error = FALSE))) {
    skip("No internet connection available. ")
  }
  if ("try-error" %in% c(
    class(try(httr::headers(httr::HEAD(
      url = utils::URLencode("https://clinicaltrials.gov"))),
      silent = TRUE)),
    class(try(httr::headers(httr::HEAD(
      url = utils::URLencode("https://www.clinicaltrialsregister.eu/"),
      config = httr::config(ssl_verifypeer = FALSE)
    )),
    silent = TRUE))
  )
  ) {
    skip("One or more registers not available. ")
  }
}

# helper function to check mongodb
has_mongo <- function(){
  # # check server
  # tmp <- getOption("warn")
  # options("warn" = 2)
  # test
  # mongo_ok <- try(ctrdata:::ctrMongo(), silent = TRUE)
  mongo_ok <- try(nodbi::src_mongo(), silent = TRUE)
  # use test result
  # options("warn" = tmp)
  if ("try-error" %in% class(mongo_ok)) {
    #skip("No password-free localhost mongodb accessible.")
    skip("No access using nodbi::src_mongo()")
  }
}

# helper function to check sqlite
has_sqlite <- function(){
  # # check server
  # tmp <- getOption("warn")
  # options("warn" = 2)
  # test
  sqlite_ok <- try(nodbi::src_sqlite(), silent = TRUE)
  # use test result
  # options("warn" = tmp)
  if ("try-error" %in% class(sqlite_ok)) {
    skip("No access using nodbi::src_sqlite()")
  }
}

# helper function to check tool chain
has_toolchain <- function(){

  tc_ok <- try({
    any(

      # the tests are similar to those in onload.R
      !suppressWarnings(installFindBinary("php --version")),
      !suppressWarnings(installFindBinary("php -r 'simplexml_load_string(\"\");'")),
      !suppressWarnings(installFindBinary("echo x | sed s/x/y/")),
      !suppressWarnings(installFindBinary("perl -V:osname")),
      !suppressMessages({
        tmp <- installCygwinWindowsTest()
        ifelse(is.null(tmp), TRUE, tmp)
      }),

      na.rm = TRUE)
  }, silent = TRUE)

  if ((class(tc_ok) == "try-error") || (tc_ok == TRUE)) {
    skip("One or more tool chain applications are not available.")
  }
}


# helper function to check mongodb
has_mongo_remote <- function(mdburi, db, collection) {

  # test
  mongo_ok <- try(
    nodbi::src_mongo(collection = collection,
                     db = db,
                     url = mdburi),
    silent = TRUE)

  # use test result
  if ("try-error" %in% class(mongo_ok)) {
    skip("No access using nodbi::src_mongo() to remote database.")
  }
}


#### local mongodb ####
test_that("local mongodb", {

  has_mongo()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # initialise = drop collections from mongodb
  try(nodbi::docdb_delete(dbc, key = dbc$collection), silent = TRUE)
  # try(mongolite::mongo(con = dbc,
  #                      url = "mongodb://localhost/users")$drop(),
  #     silent = TRUE)

  # test 1
  expect_message(dbQueryHistory(con = dbc),
                 "No history found in expected format.")

})


#### remote mongodb read only ####
test_that("remote mongodb read only", {

  ## brief testing of main functions

  has_toolchain()
  has_internet()

  # # specify base uri for remote mongodb server, trailing slash
  mdburi <- "mongodb+srv://DWbJ7Wh:bdTHh5cS@cluster0-b9wpw.mongodb.net/"
  # permissions are restricted to "find" in "dbperm" in "dbperm"
  # no other functions can be executed, no login possible
  #
  # skip if no access despite internet
  has_mongo_remote(mdburi, "dbperm", "dbperm")

  ## read-only tests

  # initialise - this collection has been filled with
  # documents from test "remote mongodb read write"
  dbc <- nodbi::src_mongo(collection = "dbperm",
                          db = "dbperm",
                          url = mdburi)

  # field get test
  expect_message(dbFindFields(namepart = "date", con = dbc),
                 "Finding fields")

  # read test
  tmp <- dbGetFieldsIntoDf(fields = c("a2_eudract_number",
                                      "overall_status",
                                      "record_last_import",
                                      "primary_completion_date",
                                      "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
                                      "study_design_info.intervention_model",
                                      "e71_human_pharmacology_phase_i"),
                           con = dbc)

  # output tests
  expect_equal(dim(tmp)[2], 8)
  expect_true("POSIXct"   %in% class(tmp[["record_last_import"]]))
  expect_true("character" %in% class(tmp[["study_design_info.intervention_model"]]))

})


#### remote mongodb write read ####
test_that("remote mongodb write read", {

  ## brief testing of main functions

  has_toolchain()
  has_internet()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"

  # test remote mongodb server
  # expected to only work locally and on CI Travis,
  # where ctrdatamongouri is set as environment variable
  has_mongo_remote(Sys.getenv(x = "ctrdatamongouri"), "dbtemp", coll)

  # continue
  dbc <- nodbi::src_mongo(collection = coll,
                          db = "dbtemp",
                          url = Sys.getenv(x = "ctrdatamongouri"))

  # test 2a
  expect_equivalent(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "CTGOV",
    con = dbc)$n,
    1L)

  # test 2b
  expect_equivalent(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "EUCTR",
    con = dbc)$n,
    6L)

  # test 2c
  expect_true(
    length(
      dbFindFields(namepart = "date",
                   con = dbc)) > 7L)

  # clean up
  nodbi::docdb_delete(src = dbc,
                      key = dbc$collection)

})


#### empty downloads ####
test_that("retrieve data from registers", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # test 3
  expect_equal(suppressMessages(ctrLoadQueryIntoDb(
    queryterm = "query=NonExistingConditionGoesInHere",
    register = "EUCTR",
    con = dbc)$n),
    0L)

  # test 4
  expect_equal(suppressMessages(ctrLoadQueryIntoDb(
    queryterm = "cond=NonExistingConditionGoesInHere",
    register = "CTGOV",
    con = dbc)$n),
    0L)

  # clean up is the end of script

})


#### ctgov mongo new, update ####
test_that("retrieve data from register ctgov", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # test 5
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "CTGOV",
    con = dbc),
    "Imported or updated 1 trial")

  ## create and test updatable query

  q <- paste0("https://clinicaltrials.gov/ct2/results?term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

  # test 6
  expect_message(
    ctrLoadQueryIntoDb(
      paste0(q, "12%2F31%2F2008"),
      con = dbc),
    "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(con = dbc)
  # manipulate query
  hist[nrow(hist), "query-term"] <- sub("(.*&lup_e=).*", "\\112%2F31%2F2009", hist[nrow(hist), "query-term"])
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  nodbi::docdb_update(src = dbc,
                      key = dbc$collection,
                      value = data.frame("_id" = "meta-info",
                                         "content" = as.character(json),
                                         stringsAsFactors = FALSE,
                                         check.names = FALSE)
  )

  # mongolite::mongo(con = dbc,
  #                  url = "mongodb://localhost/users")$update(query = '{"_id":{"$eq":"meta-info"}}',
  #                                                            update = paste0('{ "$set" :', json, "}"),
  #                                                            upsert = TRUE)

  # test 7
  expect_message(suppressWarnings(ctrLoadQueryIntoDb(
    querytoupdate = "last", con = dbc)),
    "Imported or updated")

  remove("hist", "json", "q")

})



#### ctgov sqlite new, update ####
test_that("retrieve data from register ctgov into sqlite", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_sqlite(collection = coll)

  # test 5
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "2010-024264-18",
        register = "CTGOV",
        con = dbc)),
    "Imported or updated 1 trial")

  ## create and test updatable query

  q <- paste0("https://clinicaltrials.gov/ct2/results?term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

  # test 6
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        paste0(q, "12%2F31%2F2008"),
        con = dbc)),
    "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- suppressWarnings(dbQueryHistory(con = dbc))
  # manipulate query
  hist[nrow(hist), "query-term"] <- sub("(.*&lup_e=).*", "\\112%2F31%2F2009", hist[nrow(hist), "query-term"])
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  nodbi::docdb_update(src = dbc,
                      key = dbc$collection,
                      value = data.frame("_id" = "meta-info",
                                         "content" = as.character(json),
                                         stringsAsFactors = FALSE,
                                         check.names = FALSE)
  )

  # mongolite::mongo(con = dbc,
  #                  url = "mongodb://localhost/users")$update(query = '{"_id":{"$eq":"meta-info"}}',
  #                                                            update = paste0('{ "$set" :', json, "}"),
  #                                                            upsert = TRUE)

  # test 7
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        querytoupdate = "last", con = dbc)),
    "Imported or updated")

  remove("hist", "json", "q")

})


#### euctr mongo new, update ####
test_that("retrieve data from register euctr", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "neuroblastoma&status=completed&phase=phase-one&country=pl")
  # ctrGetQueryUrlFromBrowser(content = q)
  # ctrOpenSearchPagesInBrowser(q)

  # test 11
  expect_message(
    ctrLoadQueryIntoDb(q,
                       con = dbc),
    "Imported or updated")

  ## create and test updatable query

  # only works for last 7 days with rss mechanism
  # query based on date is used since this avoids no trials are found

  date.today <- Sys.time()
  date.from  <- format(date.today - (60 * 60 * 24 * 12), "%Y-%m-%d")
  date.to    <- format(date.today - (60 * 60 * 24 *  6), "%Y-%m-%d")

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "&dateFrom=", date.from, "&dateTo=", date.to)
  # ctrOpenSearchPagesInBrowser(q)

  # test 13
  expect_message(
    ctrLoadQueryIntoDb(q,
                       con = dbc,
                       verbose = TRUE),
    "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(con = dbc)
  # manipulate query
  hist[nrow(hist), "query-term"]      <- sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
  hist[nrow(hist), "query-timestamp"] <- paste0(date.to, " 23:59:59")
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  nodbi::docdb_update(src = dbc,
                      key = dbc$collection,
                      value = data.frame("_id" = "meta-info",
                                         "content" = as.character(json),
                                         stringsAsFactors = FALSE,
                                         check.names = FALSE)
  )

  # mongolite::mongo(con = dbc,
  #                  url = "mongodb://localhost/users")$update(query = '{"_id":{"$eq":"meta-info"}}',
  #                                                            update = paste0('{ "$set" :', json, "}"),
  #                                                            upsert = TRUE)

  # test 14
  expect_message(
    ctrLoadQueryIntoDb(querytoupdate = "last",
                       con = dbc),
    "(Imported or updated|First result page empty)")

  remove("hist", "json", "q", "date.from", "date.today", "date.to")

})


#### euctr sqlite new, update ####
test_that("retrieve data from register euctr into sqlite", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_sqlite(collection = coll)

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "neuroblastoma&status=completed&phase=phase-one&country=pl")
  # ctrGetQueryUrlFromBrowser(content = q)
  # ctrOpenSearchPagesInBrowser(q)

  # test 11
  expect_message(
    ctrLoadQueryIntoDb(q,
                       con = dbc),
    "Imported or updated")

  ## create and test updatable query

  # only works for last 7 days with rss mechanism
  # query based on date is used since this avoids no trials are found

  date.today <- Sys.time()
  date.from  <- format(date.today - (60 * 60 * 24 * 12), "%Y-%m-%d")
  date.to    <- format(date.today - (60 * 60 * 24 *  6), "%Y-%m-%d")

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "&dateFrom=", date.from, "&dateTo=", date.to)
  # ctrOpenSearchPagesInBrowser(q)

  # test 13
  expect_message(
    ctrLoadQueryIntoDb(q,
                       con = dbc,
                       verbose = TRUE),
    "Imported or updated ")

  # manipulate history to force testing updating
  # based on code in dbCTRUpdateQueryHistory
  hist <- dbQueryHistory(con = dbc)
  # manipulate query
  hist[nrow(hist), "query-term"]      <- sub(".*(&dateFrom=.*)&dateTo=.*", "\\1", q)
  hist[nrow(hist), "query-timestamp"] <- paste0(date.to, " 23:59:59")
  # convert into json object
  json <- jsonlite::toJSON(list("queries" = hist))
  # update database
  nodbi::docdb_update(src = dbc,
                      key = dbc$collection,
                      value = data.frame("_id" = "meta-info",
                                         "content" = as.character(json),
                                         stringsAsFactors = FALSE,
                                         check.names = FALSE)
  )

  # mongolite::mongo(con = dbc,
  #                  url = "mongodb://localhost/users")$update(query = '{"_id":{"$eq":"meta-info"}}',
  #                                                            update = paste0('{ "$set" :', json, "}"),
  #                                                            upsert = TRUE)

  # test 14
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(querytoupdate = "last",
                         con = dbc)),
    "(Imported or updated|First result page empty)")

  remove("hist", "json", "q", "date.from", "date.today", "date.to")

})


#### euctr results ####
test_that("retrieve results from register euctr", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # clean up
  nodbi::docdb_delete(src = dbc,
                      key = dbc$collection)

  q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
              "2007-000371-42+OR+2011-004742-18")
  # ctrGetQueryUrlFromBrowser(content = q)
  # ctrOpenSearchPagesInBrowser(input = q)

  expect_message(
    ctrLoadQueryIntoDb(q,
                       euctrresults = TRUE,
                       con = dbc),
    "Imported or updated results for")

  tmp <- dbGetFieldsIntoDf(fields = c("a2_eudract_number",
                                      "endPoints.endPoint.title",
                                      "firstreceived_results_date",
                                      "e71_human_pharmacology_phase_i",
                                      "version_results_history"),
                           con = dbc,
                           stopifnodata = FALSE)

  # test 16
  expect_true(!any(is.na(tmp[tmp$a2_eudract_number == "2007-000371-42", c(4,5,6)])))
  expect_true(all(c(tmp$firstreceived_results_date[tmp$a2_eudract_number == "2007-000371-42"][1] == as.Date("2015-07-29"),
                    tmp$firstreceived_results_date[tmp$a2_eudract_number == "2011-004742-18"][1] == as.Date("2016-07-28"))))

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

  # test 19
  expect_warning(ctrGetQueryUrlFromBrowser(content = "ThisDoesNotExist"),
                 "no clinical trial register search URL found")

  has_internet()

  # test 20
  expect_message(ctrOpenSearchPagesInBrowser(input = q),
                 "Opening browser for search:")

  # test 21
  expect_message(ctrOpenSearchPagesInBrowser(input = tmp),
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
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # test 26
  expect_message(ctrOpenSearchPagesInBrowser(dbQueryHistory(con = dbc)[1, ]),
                 "Opening browser for search:")

  tmp <-  data.frame(lapply(dbQueryHistory(con = dbc),
                            tail, 1L),
                     stringsAsFactors = FALSE,
                     check.names = FALSE)

  # test 27
  expect_message(ctrOpenSearchPagesInBrowser(tmp),
                 "Opening browser for search:")

})


#### db fields and records ####
test_that("operations on database after download from register", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # test 28
  expect_error(dbFindFields(
    namepart = c("onestring", "twostring"),
    con = dbc),
    "Name part should have only one element.")
  #
  expect_error(dbFindFields(
    namepart = list("onestring", "twostring"),
    con = dbc),
    "Name part should be atomic.")
  #
  expect_error(dbFindFields(namepart = "",
                            con = dbc),
               "Empty name part string.")

  # test 31
  tmp <- dbFindFields(
    namepart = "date",
    con = dbc)
  expect_type(tmp, "character")
  expect_true(length(tmp) > 5L)
  #

  # dbFindIdsUniqueTrials

  # test 33
  expect_message(dbFindIdsUniqueTrials(
    con = dbc,
    preferregister = "EUCTR"),
    "Searching multiple country records")

  # test 34
  expect_message(dbFindIdsUniqueTrials(
    con = dbc,
    preferregister = "CTGOV"),
    "Returning keys")

  # test 35
  expect_warning(dbFindIdsUniqueTrials(
    con = dbc,
    prefermemberstate = "3RD",
    include3rdcountrytrials = FALSE),
    "Preferred EUCTR version set to 3RD country trials, but include3rdcountrytrials was FALSE")


  # dbGetFieldsIntoDf

  # test 36
  expect_error(dbGetFieldsIntoDf(
    fields = "ThisDoesNotExist",
    con = dbc),
    "For field: ThisDoesNotExist no data could be extracted")

  # test 37
  expect_error(dbGetFieldsIntoDf(
    fields = "",
    con = dbc),
    "'fields' contains empty elements")

  # test 38
  expect_error(dbGetFieldsIntoDf(
    fields = list("ThisDoesNotExist"),
    con = dbc),
    "Input should be a vector of strings of field names.")


  # clean up = drop collections from mongodb

  # test 38
  expect_equivalent(nodbi::docdb_delete(src = dbc, key = dbc$collection), TRUE)

})


#### deduplication ####
test_that("operations on database for deduplication", {

  has_mongo()
  has_internet()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # get some trials with corresponding numbers
  # ctrLoadQueryIntoDb(queryterm = "NCT00134030", register = "CTGOV", con = dbc) # EUDRACT-2004-000242-20
  # ctrLoadQueryIntoDb(queryterm = "NCT01516580", register = "CTGOV", con = dbc) # 2010-019224-31
  # ctrLoadQueryIntoDb(queryterm = "NCT00025597", register = "CTGOV", con = dbc) # this is not in euctr
  # ctrLoadQueryIntoDb(queryterm = "2010-019224-31", register = "EUCTR", con = dbc)
  # ctrLoadQueryIntoDb(queryterm = "2004-000242-20", register = "EUCTR", con = dbc)
  # ctrLoadQueryIntoDb(queryterm = "2005-000915-80", register = "EUCTR", con = dbc) # this is not in ctgov
  # ctrLoadQueryIntoDb(queryterm = "2014-005674-11", register = "EUCTR", con = dbc) # this is 3rd country only
  # ctrLoadQueryIntoDb(queryterm = "2016-002347-41", register = "EUCTR", con = dbc) # in eu and 3rd country

  # loading into empty database / collection

  ctrLoadQueryIntoDb(
    queryterm = "NCT00134030 OR NCT01516580 OR NCT00025597",
    register = "CTGOV",
    con = dbc)

  ctrLoadQueryIntoDb(
    queryterm = "2010-019224-31 OR 2004-000242-20 OR 2005-000915-80 OR 2014-005674-11 OR 2016-002347-41",
    register = "EUCTR",
    con = dbc)

  # test combinations of parameters

  # test 41
  tmp <- dbFindIdsUniqueTrials(con = dbc)
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB",
                               "2014-005674-11-3RD", "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  # test 42
  tmp <- dbFindIdsUniqueTrials(con = dbc, include3rdcountrytrials = FALSE) # removes 2014-005674-11
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  # test 43
  tmp <- dbFindIdsUniqueTrials(con = dbc, prefermemberstate = "3RD") # changes 2016-002347-41
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-GB", "2010-019224-31-GB", "2014-005674-11-3RD",
                               "2016-002347-41-3RD", "NCT00025597"),
                        check.attributes = FALSE))

  # test 44
  tmp <- dbFindIdsUniqueTrials(con = dbc, prefermemberstate = "IT")
  expect_true(all.equal(tmp, c("2004-000242-20-GB", "2005-000915-80-IT", "2010-019224-31-IT", "2014-005674-11-3RD",
                               "2016-002347-41-GB", "NCT00025597"),
                        check.attributes = FALSE))

  # test 45
  tmp <- dbFindIdsUniqueTrials(con = dbc, preferregister = "CTGOV")
  expect_true(all.equal(tmp, c("NCT00025597", "NCT00134030", "NCT01516580", "2005-000915-80-GB",
                               "2014-005674-11-3RD", "2016-002347-41-GB"),
                        check.attributes = FALSE))

  # test 46
  tmp <- dbFindIdsUniqueTrials(con = dbc, preferregister = "CTGOV", prefermemberstate = "IT")
  expect_true(all.equal(tmp, c("NCT00025597", "NCT00134030", "NCT01516580", "2005-000915-80-IT",
                               "2014-005674-11-3RD", "2016-002347-41-GB"),
                        check.attributes = FALSE))


  # clean up = drop collections from mongodb

  # test 47
  expect_equivalent(nodbi::docdb_delete(src = dbc, key = dbc$collection), TRUE)

})



#### annotations ####
test_that("annotate queries", {

  has_internet()
  has_mongo()
  has_toolchain()

  # initialise
  coll <- "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"
  dbc <- nodbi::src_mongo(collection = coll)

  # test 49
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "NCT01516567",
    register = "CTGOV",
    con = dbc,
    annotation.text = "ANNO",
    annotation.mode = "replace"),
    "Imported or updated 1 trial")

  # test 50
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "NCT01516567",
    register = "CTGOV",
    con = dbc,
    annotation.text = "APPEND",
    annotation.mode = "append"),
    "Imported or updated 1 trial")

  expect_message(ctrLoadQueryIntoDb(
    queryterm = "NCT01516567",
    register = "CTGOV",
    con = dbc,
    annotation.text = "PREPEND",
    annotation.mode = "prepend"),
    "Imported or updated 1 trial")

  # test 51
  expect_message(ctrLoadQueryIntoDb(
    queryterm = "2010-024264-18",
    register = "EUCTR",
    con = dbc,
    annotation.text = "EUANNO",
    annotation.mode = "replace"),
    "Imported or updated")

  # test 52
  tmp <- dbGetFieldsIntoDf(
    fields = "annotation",
    con = dbc)
  tmp <- tmp[tmp[["_id"]] %in%
               dbFindIdsUniqueTrials(
                 con = dbc) , ]
  expect_equal(sort(tmp[["annotation"]]),
               sort(c("EUANNO", "PREPEND ANNO APPEND")))

})


#### df operations ####
test_that("operations on data frame", {

  df <- data.frame("var1" = 1:3,
                   "var2" = 2:4,
                   stringsAsFactors = FALSE)

  statusvalues <- list("Firstvalues" = c("12", "23"),
                       "Lastvalue"   = c("34"))

  # dfMergeTwoVariablesRelevel

  # test 53
  expect_error(dfMergeTwoVariablesRelevel(list("var1", "var2")),
               "Need a data frame as input.")

  # test 54
  expect_message(dfMergeTwoVariablesRelevel(df = df,
                                            colnames = c("var1", "var2")),
                 "Unique values returned: 12, 23, 34")

  # test 55
  expect_is(dfMergeTwoVariablesRelevel(df = df,
                                       colnames = c("var1", "var2")),
            "character")

  # test 56
  expect_message(dfMergeTwoVariablesRelevel(df = df,
                                            colnames = c("var1", "var2"),
                                            levelslist = statusvalues),
                 "Unique values returned: Firstvalues, Lastvalue")

  # test 57
  expect_error(dfMergeTwoVariablesRelevel(df = df,
                                          colnames = 1:3),
               "Please provide exactly two column names.")


})


#### active substance ####
test_that("operations on data frame", {

  has_internet()

  # test 57
  expect_equal(sort(ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")[1:4]),
               sort(c("gleevec", "glivec", "imatinib", "sti 571")))


})
