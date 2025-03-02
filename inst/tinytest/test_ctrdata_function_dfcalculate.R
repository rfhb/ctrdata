## RH 2025-01-26

# setup
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

# tests
tf <- function() {

  # test
  expect_true(length(dfCalculateConcept()) >= 15L)

  # test
  expect_true(length(unique(unlist(
    suppressMessages(sapply(dfCalculateConcept(),
                            dfCalculateConcept))))) >= 190L)

  # get data
  if (!checkSqlite()) exit_file("Reason: no SQLite")

  dbc <- nodbi::src_sqlite(
    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
    collection = "my_trials", RSQLite::SQLITE_RO)

  # only SQLite triggers message
  expect_message(
    df <- dbGetFieldsIntoDf(
      fields = "",
      calculate = dfCalculateConcept(),
      con = dbc,
      verbose = FALSE
    ),
    "iterating over fields"
  )

  # test
  expect_true(all(grepl("_id|^[.][a-z]+[A-Z]", names(df))))
  expect_true(ncol(df) >= 21L)
  expect_true(nrow(df) >= 29L)

  # factors
  expect_length(table(df$.controlType, exclude = NULL), 6L)
  expect_length(table(df$.isMedIntervTrial, exclude = NULL), 2L)
  expect_length(table(df$.isPlatformTrial, exclude = NULL), 1L)
  expect_length(table(df$.isUniqueTrial, exclude = NULL), 2L)
  expect_length(table(df$.statusRecruitment, exclude = NULL), 3L)
  expect_length(table(df$.trialPopulationAgeGroup, exclude = NULL), 4L)
  expect_length(table(df$.sponsorType, exclude = NULL), 3L)
  expect_length(table(df$.trialPhase, exclude = NULL), 10L)
  expect_length(table(df$.trialObjectives, exclude = NULL), 16L)

  # integers
  expect_true(sum(df$.numSites, na.rm = TRUE) > 250L)
  expect_true(sum(df$.numTestArmsSubstances, na.rm = TRUE) > 25L)
  expect_true(sum(df$.sampleSize, na.rm = TRUE) > 8400L)
  expect_true(sum(df$.primaryEndpointFirstPsize, na.rm = TRUE) > 70L)

  # double
  expect_true(max(df$.primaryEndpointFirstPvalue, na.rm = TRUE) < 1.0)

  # dates
  expect_true(all(df$.resultsDate > as.Date("2000-01-01"), na.rm = TRUE))
  expect_true(all(df$.startDate > as.Date("2000-01-01"), na.rm = TRUE))

  # strings
  expect_true(sum(nchar(unlist(df$.primaryEndpointDescription))) > 19000L)
  expect_true(sum(nchar(unlist(df$.primaryEndpointFirstPmethod)), na.rm = TRUE) >= 10L)
  expect_true(sum(nchar(unlist(df$.trialPopulationInclusion))) > 26900L)
  expect_true(sum(nchar(unlist(df$.trialPopulationExclusion))) > 23800L)

}
tf()
