## RH 2020-07-01

#### SETUP ####
source("setup_ctrdata.R")
if (!checkSqlite()) exit_file("Reason: no SQLite")

## test function
tf <- function() {

  if (Sys.info()[["sysname"]] != "Linux") {

    try(clipr::clear_clip(allow_non_interactive = TRUE), silent = TRUE)

  }

  ## database in memory
  dbc <- nodbi::src_sqlite(
    collection = "inmemory"
  )

  # register clean-up
  on.exit(expr = {
    try({
      RSQLite::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RSQLite::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  }, add = TRUE)

  # do tests

  #### ctrLoadQueryIntoDb ####

  # test
  expect_error(
    ctrLoadQueryIntoDb(),
    "does not seem to result")

  tmpdf <- iris[1:5, ]
  names(tmpdf) <- paste0("query-", names(tmpdf))
  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf)),
    "has to be a non-empty string")
  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf,
        querytoupdate = 1L,
        con = dbc)),
    "only one of 'queryterm' and 'querytoupdate'")
  tmpdf["query-term"] <- as.character(tmpdf[["query-Species"]])

  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf)),
    "'register' has to be a non-empty string")

  # test
  expect_error(
    ctrLoadQueryIntoDb(
      queryterm = iris),
    "'queryterm' does not seem to result from ctr")

  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "https\\#@")),
    "'queryterm' does not seem to result from")

  # test
  expect_error(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "https://classic.clinicaltrials.gov/this*")),
    "Not a search query")

  # test
  expect_error(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "something",
        register = "unknown")),
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
    if (!is.null(tmpcb) && length(tmpcb) == 1L && tmpcb == "") {
      expect_error(
        suppressWarnings(
          suppressMessages(
            ctrLoadQueryIntoDb(
              queryterm = "",
              con = dbc))),
        "'queryterm'")
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
    "only one of 'queryterm' and 'querytoupdate'")

  #### database ####

  # test
  expect_error(
    ctrdata:::ctrDb(
      con = NULL
    ), "connection object .+ nodbi")

  # test if database connection
  # is opened by ctrDb
  RSQLite::dbDisconnect(conn = dbc$con)
  # test
  expect_error(
    suppressMessages(
      suppressWarnings(
        dbFindIdsUniqueTrials(
          con = dbc))),
    "No records found, check collection")
  # test
  expect_true(grepl("inmemory", dbc$collection))

  # sqlite but no collection specified
  dbc <- try(nodbi::src_sqlite(), silent = TRUE)
  # register clean-up
  on.exit(expr = {
    try({
      RSQLite::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RSQLite::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  }, add = TRUE)
  out <- inherits(dbc, c("src_sqlite", "docdb_src"))
  if (out) {
    # test
    expect_error(
      suppressMessages(
        suppressWarnings(
          dbFindIdsUniqueTrials(
            con = dbc))),
      "parameter .* table")
    RSQLite::dbDisconnect(conn = dbc$con)
  }
  rm(dbc, out)

  # postgres but no collection specified
  dbc <- try(nodbi::src_postgres(), silent = TRUE)
  # register clean-up
  on.exit(expr = {
    try({
      RPostgres::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RPostgres::dbDisconnect(conn = dbc$con)
    },
    silent = TRUE)
  }, add = TRUE)
  out <- inherits(dbc, c("src_postgres", "docdb_src"))
  if (out) {
    # test
    expect_error(
      suppressMessages(
        suppressWarnings(
          dbFindIdsUniqueTrials(
            con = dbc))),
      "pecify .* table")
    RPostgres::dbDisconnect(conn = dbc$con)
  }
  rm(dbc, out)

  #### ctrGetQueryUrl ####

  # see also test_ctrdata_other_functions.R

  # EUCTR mangling: list of c(input, expected output)
  queryterms <- list(
    c("cancer&age=adult", # add query=
      "query=cancer&age=adult"),
    c("cancer", # add query=
      "query=cancer"),
    c("(cancer OR pneumonia)", # add query=
      "query=(cancer OR pneumonia)"),
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

  # for debugging
  ctrGetQueryUrl(url = queryterms[[9]][1], register = "CTGOV")
  sapply(sapply(queryterms, "[[", 1),
         function(i) ctrGetQueryUrl(url = i, register = "EUCTR")[["query-term"]],
         USE.NAMES = FALSE, simplify = TRUE)

  # test
  expect_true(all(
    vapply(queryterms, function(qt) {
      suppressMessages(ctrGetQueryUrl(
        url = qt[[1]],
        register = "EUCTR"))[[1]] == qt[[2]]},
      logical(1L))
  ))

  # CTGOV to CTGOV2
  queryterms <- list(
    c("cancer&age=1",
      "term=cancer&aggFilters=ages:adult"),
    c("cancer",
      "term=cancer"),
    c("cancer&age=1&phase=0",
      "term=cancer&aggFilters=ages:adult,phase:1"),
    c("cancer&age=1&phase=1&rslt=With",
      "term=cancer&aggFilters=ages:adult,phase:2,results:with"),
    c("&age=1&phase=1&abc=xyz&cancer&rslt=With",
      "term=cancer&aggFilters=ages:adult,phase:2,results:with"),
    c("age=1&cancer",
      "term=cancer&aggFilters=ages:adult"),
    c("2010-024264-18",
      "term=2010-024264-18"),
    c("NCT1234567890",
      "term=NCT1234567890"),
    c("NCT1234567890+OR+NCT1234567890+AND+SOMETHING",
      "term=NCT1234567890 OR NCT1234567890 AND SOMETHING"),
    c("term=cancer&age=1",
      "term=cancer&aggFilters=ages:adult"),
    c("age=1&term=cancer",
      "term=cancer&aggFilters=ages:adult"))

  # for debugging
  #  ctrGetQueryUrl(url = queryterms[[9]][1], register = "CTGOV")
  sapply(sapply(queryterms, "[[", 1),
         function(i) ctrGetQueryUrl(url = i, register = "CTGOV")[["query-term"]],
         USE.NAMES = FALSE, simplify = TRUE)

  # test
  expect_true(all(
    vapply(queryterms, function(qt) {
      suppressMessages(ctrGetQueryUrl(
        url = qt[[1]],
        register = "CTGOV"))[[1]] == qt[[2]]},
      logical(1L))
  ))

  # URLs
  queryurls <- list(
    # EUCTR
    c("https://www.clinicaltrialsregister.eu/ctr-search/search?query=neuroblastoma",
      "query=neuroblastoma"),
    c("https://www.clinicaltrialsregister.eu/ctr-search/trial/2019-003713-33/NL",
      "query=2019-003713-33"),
    c("https://www.clinicaltrialsregister.eu/ctr-search/trial/2007-000371-42/results",
      "query=2007-000371-42"),
    # CTGOV to CTGOV2
    c("https://classic.clinicaltrials.gov/ct2/results?cond=Neuroblastoma&term=&intr=Investigational+Agent&type=Intr",
      "cond=Neuroblastoma&intr=Investigational Agent&aggFilters=studyType:int"),
    c("https://classic.clinicaltrials.gov/ct2/show/NCT01492673?type=Intr&cond=Neuroblastoma&intr=Investigational+Agent&draw=2&rank=1",
      "term=NCT01492673"),
    c("https://classic.clinicaltrials.gov/ct2/show/NCT01492673",
      "term=NCT01492673"),
    # ISRCTN
    c("https://www.isrctn.com/search?q=neuroblastoma&searchType=advanced-search",
      "q=neuroblastoma"),
    c("https://www.isrctn.com/search?q=&filters=condition:cancer",
      "q=&filters=condition:cancer"),
    c("https://www.isrctn.com/search?q=&filters=condition%3Acancer%2CrecruitmentCountry%3AGermany&searchType=basic-search",
      "q=&filters=condition:cancer,recruitmentCountry:Germany"),
    c("https://www.isrctn.com/ISRCTN70039829",
      "q=ISRCTN70039829")
  )

  # for debugging
  ctrGetQueryUrl(url = queryurls[[4]][1], register = "CTGOV")
  sapply(sapply(queryurls, "[[", 1),
         function(i) ctrGetQueryUrl(url = i)[["query-term"]],
         USE.NAMES = FALSE, simplify = TRUE)

  # test
  expect_true(all(
    vapply(queryurls, function(qt) {
      suppressMessages(ctrGetQueryUrl(
        url = qt[[1]]))[[1]] == qt[[2]]},
      logical(1L))
  ))

  #### clipboard ####

  # change after changing from clipr to readr
  if (interactive()) {

    clipr::write_clip(
      queryurls[[1]][1],
      allow_non_interactive = TRUE)
    expect_message(
      tmp <- ctrGetQueryUrl(),
      "Found search query")
    expect_true(is.data.frame(tmp))
    expect_equal(tmp[["query-term"]], queryurls[[1]][2])
    rm(tmp)

    clipr::write_clip(
      "NotARegisterUrl",
      allow_non_interactive = TRUE)
    expect_error(
      ctrGetQueryUrl(),
      "no clinical trial register")

  }

  # URLs for single studies
  queryurls <- list(
    # EUCTR
    c("https://www.clinicaltrialsregister.eu/ctr-search/trial/2007-000371-42/results",
      "https://www.clinicaltrialsregister.eu/ctr-search/search?query=2007-000371-42#tabs"),
    # CTGOV to CTGOV2
    c("https://classic.clinicaltrials.gov/ct2/show/NCT01492673?cond=neuroblastoma",
      "https://clinicaltrials.gov/search?term=NCT01492673"),
    c("https://clinicaltrials.gov/ct2/show/NCT01492673?cond=neuroblastoma",
      "https://clinicaltrials.gov/search?term=NCT01492673"),
    c("https://www.clinicaltrials.gov/study/NCT01467986?cond=neuroblastoma&intr=Investigational%20drug&aggFilters=ages:child",
      "https://clinicaltrials.gov/study/NCT01467986#main-content"),
    # ISCRTN
    c("https://www.isrctn.com/ISRCTN70039829",
      "https://www.isrctn.com/ISRCTN70039829"),
    c("https://www.isrctn.com/ISRCTN61139514?q=&filters=condition:cancer",
      "https://www.isrctn.com/ISRCTN61139514")
  )

  # https://clinicaltrials.gov/study/NCT04903899?cond=neuroblastoma

  opts <- options()
  options(browser = NULL)

  # for debugging
  ctrOpenSearchPagesInBrowser(url = queryurls[[4]][1])
  sapply(sapply(queryurls, "[[", 1),
         function(i) ctrOpenSearchPagesInBrowser(url = i),
         USE.NAMES = FALSE, simplify = TRUE)

  # test
  expect_true(all(
    vapply(queryurls, function(qt) {
      suppressMessages(ctrOpenSearchPagesInBrowser(
        url = qt[[1]]))[[1]] == qt[[2]]},
      logical(1L))
  ))

  options(opts)

  # clean up
  rm(queryurls)

} # tf test function
tf()
