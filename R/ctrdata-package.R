#' ctrdata: get started, database connection, function overview
#'
#' A package for aggregating and analysing information on clinical
#' studies, and for obtaining documents, from public registers
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
#' Use a connection object with a `ctrdata` function, for example
#' \link{dbQueryHistory}, or other packages, for example
#' \link[mongolite:mongo]{mongolite::mongo} or \link[nodbi:docdb_query]{nodbi::docdb_query}.
#' Use a demo database:
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
#' @name ctrdata
#' @docType package
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords package
#' @md
#'
"_PACKAGE"
