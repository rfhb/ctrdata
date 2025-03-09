## RH 2025-01-26

# setup
if (!at_home()) exit_file("Reason: not at_home")
source("setup_ctrdata.R")

# tests
tf <- function() {

  # get all functions
  fcts <- as.character(utils::ls.str(
    getNamespace("ctrdata"),
    all.names = TRUE,
    pattern = "^f[.][a-z]"))

  # get all unique fields needed for fcts
  fctFields <- sapply(
    fcts, function(i) do.call(i, list()),
    simplify = FALSE)

  # test
  expect_true(length(fcts) >= 16L)

  # test
  expect_true(length(unique(unlist(fctFields))) >= 175L)

  # get data
  if (!checkSqlite()) exit_file("Reason: no SQLite")

  dbc <- nodbi::src_sqlite(
    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
    collection = "my_trials", flags = RSQLite::SQLITE_RO)

  # only SQLite triggers message
  expect_message(
    dF <- dbGetFieldsIntoDf(
      fields = "",
      calculate = fcts,
      con = dbc,
      verbose = FALSE
    ),
    "iterating over fields"
  )

  # test
  expect_true(all(grepl("_id|^[.][a-z]+[A-Z]", names(dF))))
  expect_true(ncol(dF) >= 21L)
  expect_true(nrow(dF) >= 29L)

  # factors
  expect_length(table(dF$.controlType, exclude = NULL), 6L)
  expect_length(table(dF$.isMedIntervTrial, exclude = NULL), 2L)
  expect_length(table(dF$.likelyPlatformTrial, exclude = NULL), 1L)
  expect_length(table(dF$.isUniqueTrial, exclude = NULL), 2L)
  expect_length(table(dF$.statusRecruitment, exclude = NULL), 4L)
  expect_length(table(dF$.trialPopulationAgeGroup, exclude = NULL), 4L)
  expect_length(table(dF$.sponsorType, exclude = NULL), 3L)
  expect_length(table(dF$.trialPhase, exclude = NULL), 10L)
  expect_length(table(dF$.trialObjectives, exclude = NULL), 16L)

  # integers
  expect_true(sum(dF$.numSites, na.rm = TRUE) > 250L)
  expect_true(sum(dF$.numTestArmsSubstances, na.rm = TRUE) > 25L)
  expect_true(sum(dF$.sampleSize, na.rm = TRUE) > 8400L)
  expect_true(sum(dF$.primaryEndpointFirstPsize, na.rm = TRUE) > 70L)

  # double
  expect_true(max(dF$.primaryEndpointFirstPvalue, na.rm = TRUE) < 1.0)

  # dates
  expect_true(all(dF$.resultsDate > as.Date("2000-01-01"), na.rm = TRUE))
  expect_true(all(dF$.startDate > as.Date("2000-01-01"), na.rm = TRUE))

  # strings
  expect_true(sum(nchar(unlist(dF$.primaryEndpointDescription))) > 19000L)
  expect_true(sum(nchar(unlist(dF$.primaryEndpointFirstPmethod)), na.rm = TRUE) >= 10L)
  expect_true(sum(nchar(unlist(dF$.trialPopulationInclusion))) > 25000L)
  expect_true(sum(nchar(unlist(dF$.trialPopulationExclusion))) > 22000L)

}
tf()
