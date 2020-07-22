## RH 2020-07-01

#### SETUP ####
#source("setup_ctrdata.R")
tmp <-
  suppressWarnings(
    clipr::read_clip()
  )
if (interactive()) {
  try(
    clipr::write_clip(
      content = ""),
    silent = TRUE)
}

## database in memory
dbc <- nodbi::src_sqlite(
  collection = "inmemory"
)


#### queryterm / ctrLoadQueryIntoDb ####

tmpdf <- iris[1:5,]
names(tmpdf) <- paste0("query-", names(tmpdf))
expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = tmpdf,
      con = dbc)))
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf,
        querytoupdate = 1L,
        con = dbc))))
tmpdf["query-term"] <- as.character(tmpdf[["query-Species"]])
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = tmpdf,
        querytoupdate = 1L,
        con = dbc))))

expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "",
        con = dbc))))

expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = pi,
      con = dbc)))

expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "notlast",
      con = dbc)))

# this also checks only.count
# and that no records were found
expect_warning(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "someQueryForErrorTriggering",
      querytoupdate = 1L,
      only.count = TRUE,
      con = dbc),
    "'queryterm' and 'querytoupdate' specified, continuing"))


#### database ####

dbc <- nodbi::src_sqlite()
expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "someQueryForErrorTriggering",
      only.count = TRUE,
      con = dbc),
    "parameter 'collection' needs"))

RSQLite::dbDisconnect(conn = dbc$con)
dbc <- nodbi::src_sqlite(
  collection = "inmemory")
RSQLite::dbDisconnect(conn = dbc$con)
expect_warning(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "someQueryForErrorTriggering",
      only.count = TRUE,
      con = dbc),
    "Database connection was closed, trying to reopen"))

## restore
if (interactive()) {
  try(
    clipr::write_clip(
      content = tmp),
    silent = TRUE)
}
