#' ctrdata: get started, connect database and function overview
#'
#' A package for aggregating and analysing
#' information on and results from clinical trials,
#' retrieved from public study registers
#'
#' @section 1 - Database connection:
#' Package `ctrdata` retrieves trial information and stores it in a
#' database collection, which has to be given as a connection object
#' to parameter `con` for several ctrdata functions; this
#' connection object is created in almost identical ways for
#' these supported backends:
#'
#' *Database* | *Connection object*
#' -------- | ---------
#' MongoDB | \code{dbc <- \link[nodbi:src_mongo]{nodbi::src_mongo}(db = "my_db", collection = "my_coll")}
#' SQLite | \code{dbc <- \link[nodbi:src_sqlite]{nodbi::src_sqlite}(dbname = "my_db", collection = "my_coll")}
#' PostgreSQL | \code{dbc <- \link[nodbi:src_postgres]{nodbi::src_postgres}(dbname = "my_db"); dbc[["collection"]] <- "my_coll"}
#' DuckDB | \code{dbc <- \link[nodbi:src_duckdb]{nodbi::src_duckdb}(dbname = "my_db", collection = "my_coll")}
#'
#' Use a connection object with a `ctrdata` function, for example:
#' \code{\link[ctrdata:dbQueryHistory]{ctrdata::dbQueryHistory}(con = dbc)}.
#'
#' Any such connection object can also be used with other packages, for example
#' \code{\link[mongolite:mongo]{mongolite::mongo}()} or:
#' \code{\link[nodbi:docdb_query]{nodbi::docdb_query}(src = dbc, key = dbc$collection, fields = '{"_id": 1}', query = '{"sponsors.lead_sponsor.agency_class": "Industry"}')}
#'
#' A demo database in package `ctrdata` can be used with:
#' \code{dbc <- nodbi::src_sqlite(dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"), collection = "my_trials")}
#'
#' @section 2 - Operate on a clinical trial register:
#'
#' \link{ctrOpenSearchPagesInBrowser},
#' \link{ctrLoadQueryIntoDb} (load trial records into database collection);
#' see
#' \link{ctrdata-registers} for details on registers and how to search.
#'
#' @section 3 - Get a data frame from the database collection:
#'
#' \link{dbFindFields} (find names of fields of interest in trial records in a collection),
#' \link{dbGetFieldsIntoDf} (create a data frame with fields of interest from collection),
#' \link{dbFindIdsUniqueTrials} (get de-duplicated identifiers of
#' clinical trials' records that can be used to subset a data frame).
#'
#' @section 4 - Operate on a data frame with trial information:
#'
#' \link{dfTrials2Long} (convert fields with nested elements into long format),
#' \link{dfName2Value} (get values for variable(s) of interest).
#'
#' @name ctrdata-package
#' @docType package
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords package
#' @md
NULL
#> NULL
