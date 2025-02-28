## RH 2025-01-26

# setup
source("setup_ctrdata.R")
if (!at_home()) exit_file("Reason: not at_home")

# tests
tf <- function() {

  # test
  expect_true(length(dfCalculateConcept()) >= 15L)

  # test
  expect_true(length(unique(unlist(sapply(dfCalculateConcept(), dfCalculateConcept)))) >= 160L)

  # get data
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
  expect_true(ncol(df) >= 18L)
  expect_true(nrow(df) >= 29L)


  # online tests

  # check


  # load with start / end


  # matrix / list
  # - register
  # - function
  # - result


  # all data




  # end tests

}
tf()
