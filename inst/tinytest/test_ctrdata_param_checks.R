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
    "'queryterm' does not seem to result from")

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
        "Cannot use 'queryterm' ")
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

  # test if database connection
  # is opened by ctrDb
  # dbx <- nodbi::src_sqlite(
  #   collection = "otherinmemory")
  RSQLite::dbDisconnect(conn = dbc$con)
  # test
  expect_error(
    suppressWarnings(
      dbFindIdsUniqueTrials(
        con = dbc)),
    "No records found, check collection")
  # test
  expect_true(grepl("inmemory", dbc$collection))

  #### ctrGetQueryUrl ####

  # EUCTR mangling: list of c(input, expected output)
  queryterms <- list(
    c("cancer&age=adult", # add query=
      "query=cancer&age=adult"),
    c("cancer", # add query=
      "query=cancer"),
    c("cancer+AND breast&age=adult&phase=0", # add query=
      "query=cancer+AND breast&age=adult&phase=0"),
    c("cancer&age=adult&phase=0", # add query=
      "query=cancer&age=adult&phase=0"),
    c("cancer&age=adult&phase=1&results=true", # add query=
      "query=cancer&age=adult&phase=1&results=true"),
    c("&age=adult&phase=1&abc=xyz&cancer&results=true", # insert query=
      "&age=adult&phase=1&abc=xyz&query=cancer&results=true"),
    c("age=adult&cancer", # insert query=
      "age=adult&query=cancer"),
    c("2010-024264-18", # add query=
      "query=2010-024264-18"),
    c("NCT1234567890", # add query=
      "query=NCT1234567890"),
    c("teratoid&country=dk", # add query=
      "query=teratoid&country=dk"),
    c("term=cancer&age=adult", # keep
      "term=cancer&age=adult"),
    c("age=adult&term=cancer", # keep
      "age=adult&term=cancer")
  )

  # test
  expect_true(all(vapply(queryterms, function(qt) {
    suppressMessages(ctrGetQueryUrl(
      url = qt[[1]],
      register = "EUCTR"))[[1]] == qt[[2]]},
    logical(1L))))

  # CTGOV
  queryterms <- list(
    c("cancer&age=adult",  # add term=
      "term=cancer&age=adult"),
    c("cancer", # add term=
      "term=cancer"),
    c("cancer&age=adult&phase=0", # add term=
      "term=cancer&age=adult&phase=0"),
    c("cancer&age=adult&phase=1&results=true", # add term=
      "term=cancer&age=adult&phase=1&results=true"),
    c("&age=adult&phase=1&abc=xyz&cancer&results=true", # add term=
      "&age=adult&phase=1&abc=xyz&term=cancer&results=true"),
    c("age=adult&cancer", # add term=
      "age=adult&term=cancer"),
    c("2010-024264-18", # add term=
      "term=2010-024264-18"),
    c("NCT1234567890", # add term=
      "term=NCT1234567890"),
    c("NCT1234567890+OR+NCT1234567890+AND+SOMETHING", # add term=
      "term=NCT1234567890+OR+NCT1234567890+AND+SOMETHING"),
    c("term=cancer&age=adult", # no change
      "term=cancer&age=adult"),
    c("age=adult&term=cancer", # no change
      "age=adult&term=cancer"))

  # test
  expect_true(all(vapply(queryterms, function(qt) {
    suppressMessages(ctrGetQueryUrl(
      url = qt[[1]],
      register = "CTGOV"))[[1]] == qt[[2]]},
    logical(1L))))

} # tf test function
tf()
