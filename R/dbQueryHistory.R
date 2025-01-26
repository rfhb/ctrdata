### ctrdata package

#' Show history of queries loaded into a database collection
#'
#' @inheritParams ctrDb
#'
#' @return A data frame (or tibble, if \code{tibble} is loaded)
#'  with columns: `query-timestamp`, `query-register`,
#'  `query-records` (note: this is the number of records loaded when last
#'  executing \link{ctrLoadQueryIntoDb}, not the total record number) and
#'  `query-term`, with one row for each time that
#'  \link{ctrLoadQueryIntoDb} loaded trial records into this collection.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'     dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'     collection = "my_trials",
#'    RSQLite::SQLITE_RO)
#'
#' dbQueryHistory(con = dbc)
#'
dbQueryHistory <- function(con, verbose = FALSE) {
  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # debug
  if (verbose) message("Running dbQueryHistory ...")

  hist <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{"_id": {"$eq": "meta-info"}}',
    fields = '{"queries": 1, "_id": 0}'
  )

  # check if meeting expectations
  if (is.null(hist) ||
      nrow(hist) == 0L) {
    #
    message("No history found in expected format.")
    #
    # return (class data.frame is expected)
    return(invisible(data.frame(NULL)))
    #
  }

  # access data frame of queries
  hist <- hist[["queries"]]
  if (!is.data.frame(hist)) hist <- hist[[1]]

  # inform user
  if (verbose) {
    message(
      "Number of queries in history of \"",
      con$collection, "\": ", nrow(hist)
    )

    # total number of records in collection
    # use fast queries for _id's only
    countall <- length(nodbi::docdb_query(
      src = con,
      key = con$collection,
      query = "{}",
      fields = '{"_id": 1}'
    )[["_id"]])
    countall <- countall[countall != "meta-info"]

    # inform user
    message(
      "Number of records in collection \"",
      con$collection, "\": ", countall
    )
  }

  # return
  return(ctrdata:::dfOrTibble(hist))

}
# end ctrQueryHistoryInDb
