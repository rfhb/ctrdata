# testfunctions.R
# ralf.herold@gmx.net
# 2016-01-23

# run tests manually with:
# devtools::test()

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
# installCygwinWindowsDoInstall  |
# installCygwinWindowsTest       |


test_that("access to mongo db", {

  expect_message(ctrQueryHistoryInDb(), "Total number of records")
  expect_message(ctrQueryHistoryInDb(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "Total number of records")
  expect_equal(ctrQueryHistoryInDb(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), NULL)

})


test_that("browser interaction", {

  expect_error(ctrGetQueryUrlFromBrowser(content = "ThisDoesNotExist"), "Content is not a clinical trial register search URL.")
  expect_is(ctrGetQueryUrlFromBrowser(content = "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"), "list")

  expect_equal(ctrOpenSearchPagesInBrowser(register = "EUCTR", queryterm = "cancer&age=under-18"), TRUE)

})


test_that("retrieve data from registers", {

  queryeuctr <- list(queryterm = "query=NonExistingConditionGoesInHere", register = "EUCTR")
  queryctgov <- list(queryterm =  "cond=NonExistingConditionGoesInHere", register = "CTGOV")

  expect_error(ctrLoadQueryIntoDb(queryeuctr, ns = "ns_for_test_that"), "First result page empty - no trials found?")
  expect_error(ctrLoadQueryIntoDb(queryctgov, ns = "ns_for_test_that"), "No studies downloaded.")

})


test_that("operations on database", {

  expect_message(dbFindIdsUniqueTrials(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "Searched for EUCTR identifiers")
  expect_is(dbFindIdsUniqueTrials(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "list")

  expect_is(dbFindVariable(namepart = "ThisDoesNotExist", ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "NULL")
  expect_error(dbGetVariablesIntoDf(fields = "ThisDoesNotExist", ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "No records found")

})


test_that("operations on data frame", {

  expect_message(dfMergeTwoVariablesRelevel(df = data.frame("var1" = 1, "var2" = 1), varnames = c("var1", "var2")), "Unique values found")
  expect_is(dfMergeTwoVariablesRelevel(df = data.frame("var1" = 1, "var2" = 1), varnames = c("var1", "var2")), "character")

  expect_error(dfFindUniqueEuctrRecord(df = data.frame("var1" = 1)), "Data frame does not include")

  df <- data.frame(1, 2); names (df) <- c("_id", "a2_eudract_number")

  expect_message(dfFindUniqueEuctrRecord(df = df), "0 EUCTR records dropped")
  expect_identical(dfFindUniqueEuctrRecord(df = df), df)

})

