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
  expect_true(length(fcts) >= 20L)

  # test
  expect_true(length(unique(unlist(fctFields))) >= 207L)

  # get data
  if (!checkSqlite()) exit_file("Reason: no SQLite")

  dbc <- nodbi::src_sqlite(
    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
    collection = "my_trials", flags = RSQLite::SQLITE_RO)

  # SQLite before 3.48.0 triggers message
  expect_message(
    dF <- dbGetFieldsIntoDf(
      fields = "ctrname",
      calculate = fcts,
      con = dbc,
      verbose = FALSE
    ),
    ifelse(
      utils::packageVersion("RSQLite") >= package_version("2.3.10"),
      "Querying ", "iterating over fields")
  )

  # helper
  btw <- function(x, l, u = l) isTRUE(((0.9 * l) <= x) && (x <= (u * 1.1)))

  # test
  expect_true(all(grepl("_id|ctrname|^[.][a-z]+[A-Z]", names(dF))))
  expect_true(ncol(dF) == 28L)
  expect_true(nrow(dF) == 24L)

  # factors
  expect_length(table(dF$.assignmentType, exclude = NULL), 2L)
  expect_length(table(dF$.controlType, exclude = NULL), 6L)
  expect_length(table(dF$.hasResults, exclude = NULL), 2L)
  expect_length(table(dF$.isMedIntervTrial, exclude = NULL), 2L)
  expect_length(table(dF$.likelyPlatformTrial, exclude = NULL), 1L)
  expect_length(table(dF$.isUniqueTrial, exclude = NULL), 1L)
  expect_length(table(dF$.statusRecruitment, exclude = NULL), 4L)
  expect_length(table(dF$.trialPopulationAgeGroup, exclude = NULL), 3L)
  expect_length(table(dF$.sponsorType, exclude = NULL), 4L)
  expect_length(table(dF$.trialPhase, exclude = NULL), 10L)
  expect_length(table(dF$.trialObjectives, exclude = NULL), 17L)
  expect_length(table(dF$ctrname, exclude = NULL), 5L)

  # integers
  expect_true(sum(dF$.numSites, na.rm = TRUE) == 1017L)
  expect_true(sum(dF$.numTestArmsSubstances, na.rm = TRUE) == 26L)
  expect_true(sum(dF$.sampleSize, na.rm = TRUE) == 18226L)
  expect_true(sum(dF$.primaryEndpointFirstPsize, na.rm = TRUE) == 8660L)

  # double
  expect_true(btw(mean(dF$.primaryEndpointFirstPvalue, na.rm = TRUE), 0.0024, 0.0025))

  # dates
  expect_true(all(dF$.resultsDate >= as.Date("2017-05-09"), na.rm = TRUE))
  expect_true(all(dF$.startDate >= as.Date("2007-01-10"), na.rm = TRUE))

  # strings
  expect_true(btw(sum(nchar(dF$.externalLinks), na.rm = TRUE), 4818L))
  expect_true(btw(sum(nchar(dF$.primaryEndpointFirstPmethod), na.rm = TRUE), 64L))
  expect_true(btw(sum(nchar(unlist(dF$.primaryEndpointDescription))), 13994L))
  expect_true(btw(sum(nchar(dF$.trialPopulationInclusion)), 24730L))
  expect_true(btw(sum(nchar(dF$.trialPopulationExclusion)), 26312L))
  expect_true(btw(sum(nchar(dF$.trialTitle)), 4299L))

  # lists
  expect_length(na.omit(unique(unlist(dF$.likelyRelatedTrials))), 19L)
  expect_length(na.omit(unique(unlist(dF$.maybeRelatedTrials))), 0L) # since no platform trial or similar

  # test robustness against NAs

  # local test helper function
  ltf <- function(ifn, idf, con) do.call(ifn, list(idf))

  # iterate
  for (f in fcts) {

    # info
    message(f)

    # get regular data
    suppressMessages(
      dF <- dbGetFieldsIntoDf(
        fields = unlist(fctFields[f]),
        con = dbc,
        verbose = FALSE
      ))

    # generate empty df
    for (c in seq_len(ncol(dF))[-1]) {
      v <- is.na(dF[[c]])
      # replace all values with first NA value
      dF[[c]][!v] <- dF[[c]][v][1]
    }

    # test 1 - check condition that all are NA
    expect_true(all(unlist(sapply(dF[, -1], is.na))))

    # run function f, include in helper
    # so that f.isUniqueTrial can access
    # con in the parent environment
    res <- try(ltf(f, dF, dbc), silent = TRUE)

    if (inherits(res, "try-error")) message(">>> failed: ", f)

    # test 2
    expect_false(inherits(res, "try-error"))

    # test 3
    expect_equal(nrow(dF), nrow(res))

  }

}

# run function
tf()
