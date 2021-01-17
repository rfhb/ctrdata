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

  #### queryterm and ctrLoadQueryIntoDb ####

  tmpdf <- iris[1:5, ]
  names(tmpdf) <- paste0("query-", names(tmpdf))
  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf,
        con = dbc)),
    "'url' and / or 'register' is not")
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
    "'url' and / or 'register' is not")

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
        "no clinical trial register search URL found")
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

  dbc <- nodbi::src_sqlite()
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "someQueryForErrorTriggering",
        only.count = TRUE,
        con = dbc)),
      "parameter 'collection' needs")

  RSQLite::dbDisconnect(conn = dbc$con)
  dbc <- nodbi::src_sqlite(
    collection = "inmemory")
  RSQLite::dbDisconnect(conn = dbc$con)
  expect_warning(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "someQueryForErrorTriggering",
        only.count = TRUE,
        con = dbc)),
      "Database connection was closed, trying to reopen")

} # tf test function
tf()
