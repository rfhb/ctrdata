## RH 2020-07-01

#### SETUP ####
source("setup_ctrdata.R")

## database in memory
dbc <- nodbi::src_sqlite(
  collection = "inmemory"
)

## test function
tf <- function() {

  # register clean-up
  on.exit(expr = {
    try({
      RSQLite::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RSQLite::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  })

  # do tests

  #### ctrLoadQueryIntoDb ####

  tmpdf <- iris[1:5, ]
  names(tmpdf) <- paste0("query-", names(tmpdf))
  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf,
        con = dbc)),
    "'queryterm' has to be a non-empty string")
  # test
  expect_error(
    suppressWarnings(
      suppressMessages(
        ctrLoadQueryIntoDb(
          queryterm = tmpdf,
          querytoupdate = 1L,
          con = dbc))),
    "and 'querytoupdate' specified, which is inconsistent")
  tmpdf["query-term"] <- as.character(tmpdf[["query-Species"]])
  # test
  expect_error(
    suppressWarnings(
      suppressMessages(
        ctrLoadQueryIntoDb(
          queryterm = tmpdf,
          con = dbc))),
    "'register' has to be a non-empty string")

  # test
  expect_error(
    suppressWarnings(
      suppressMessages(
        ctrLoadQueryIntoDb(
          queryterm = iris,
          con = dbc))),
    "'queryterm' does not seem to result from ctr")

  # test
  expect_error(
    suppressWarnings(
      suppressMessages(
        ctrLoadQueryIntoDb(
          queryterm = "https\\#@",
          con = dbc))),
    "'queryterm'.*has unexpected characters")

  # test no history or no table with
  # the name specified in dbc
  expect_error(
    suppressWarnings(
      suppressMessages(
        ctrLoadQueryIntoDb(
          querytoupdate = 1L,
          con = dbc))))

  # test clipr - was not able to get
  # testing to work on linux containers
  if (.Platform$OS.type == "windows" ||
      grepl("darwin", sessionInfo()$platform,
            ignore.case = TRUE)) {

    tmpcb <- suppressWarnings(
      clipr::read_clip(
        allow_non_interactive = TRUE)
    )
    # no testing if some content is
    # found in the system clipboard
    if (is.null(tmpcb) || tmpcb == "") {
      expect_error(
        suppressWarnings(
          suppressMessages(
            ctrLoadQueryIntoDb(
              queryterm = "",
              con = dbc))),
        "'queryterm' is not an non-empty string")
    }
  }

  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        querytoupdate = pi,
        con = dbc)))

  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        querytoupdate = "notlast",
        con = dbc)))

  # this also checks only.count
  # and that no records were found
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "someQueryForErrorTriggering",
        querytoupdate = 1L,
        only.count = TRUE,
        con = dbc)),
    "'queryterm' and 'querytoupdate' specified.*cannot continue")

  #### database ####

  # test
  expect_error(
    ctrdata:::ctrDb(
      con = NULL
    ), "specify in parameter 'con' a database connection")

  RSQLite::dbDisconnect(conn = dbc$con)
  dbc <- nodbi::src_sqlite()
  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "someQueryForErrorTriggering",
        only.count = TRUE,
        con = dbc)),
    "parameter 'collection' needs")

  # test if database connection
  # is opened by ctrDb
  dbx <- nodbi::src_sqlite(
    collection = "otherinmemory")
  RSQLite::dbDisconnect(conn = dbx$con)
  # test
  expect_error(
    suppressWarnings(
      dbFindIdsUniqueTrials(
        con = dbx)),
    "No records found, check collection 'otherinmemory'")
  # test
  expect_true(dbx$collection == "otherinmemory")

} # tf test function
tf()
