## RH 2025-01-26

# setup
source("setup_ctrdata.R")
if (!at_home()) exit_file("Reason: not at_home")

# tests
tf <- function() {

  # test
  expect_true(length(dfCalculate()) >= 12L)

  # test
  expect_true(length(unique(unlist(sapply(dfCalculate(), dfCalculate)))) >= 130L)

  # get data
  dbc <- nodbi::src_sqlite(
    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
    collection = "my_trials", RSQLite::SQLITE_RO)

  # only SQLite triggers message
  expect_message(
    df <- dbGetFieldsIntoDf(
      fields = "",
      calculate = dfCalculate(),
      con = dbc,
      verbose = FALSE
    ),
    "iterating over fields"
  )

  # test
  expect_equivalent(names(df), c("_id", dfCalculate()))
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
