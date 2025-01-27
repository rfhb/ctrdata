## RH 2025-01-26

# setup
source("setup_ctrdata.R")
if (!at_home()) exit_file("Reason: not at_home")

# tests
tf <- function() {

  # test
  expect_true(length(dfCalculate()) >= 5L)

  # test
  expect_true(length(unlist(sapply(dfCalculate(), dfCalculate))) > 70L)

  # get data
  dbc <- nodbi::src_sqlite(
    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
    collection = "my_trials", RSQLite::SQLITE_RO)

  df <- ctrdata::dbGetFieldsIntoDf(
    fields = "",
    calculate = dfCalculate(),
    con = dbc,
    verbose = TRUE
  )

  # test
  expect_equivalent(names(df), c("_id", dfCalculate()))
  expect_true(nrow(df) >= 29L)

  # end tests

}
tf()
