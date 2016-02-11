# testfunctions.R
# ralf.herold@gmx.net
# 2016-01-23

library(ctrdata)
context("Background functions")

# as of 2016-01-23:
#
# Name                           | Testing?
# ------------------------------ | -------------
# ctrOpenSearchPagesInBrowser	   | no (would require graphical environment)
# ctrQueryHistoryInDb            | yes (todo: check afterr ctrLoadQueryIntoDb)
# ctrGetQueryUrlFromBrowser      |
# ctrLoadQueryIntoDb             |
# dbFindIdsUniqueTrials          |
# dbFindVariable                 |
# dbFindUniqueEuctrRecord        |
# dbGetVariablesIntoDf           |
# dfMergeTwoVariablesRelevel     |
# installMongoCheckVersion       | (implicit)
# installMongoFindBinaries       | (implicit)
# installCygwinWindowsDoInstall  |
# installCygwinWindowsTest       |
#


test_that("access to mongo db", {

  expect_message(ctrQueryHistoryInDb(), "Total number of records")
  expect_message(ctrQueryHistoryInDb(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), "Total number of records")
  expect_equal(ctrQueryHistoryInDb(ns = "ThisNameSpaceShouldNotExistAnywhereInAMongoDB"), NULL)

})











