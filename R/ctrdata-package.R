#' Getting started, database connection, function overview
#'
#' `ctrdata` is a package for aggregating and analysing information on clinical
#' studies, and for obtaining documents, from public trial registers
#'
#' @section 1 - Database connection:
#' Package `ctrdata` retrieves trial information and stores it in a database
#' collection. Therefore, a database connection object has to be given to
#' parameter `con` for several `ctrdata` functions.
#' The connection object is built using \code{nodbi} which allows to use
#' different database backends.
#' Specifying a \code{collection = "<my collection's name>"} is
#' necessary for package `ctrdata`.
#' A connection object (here called `dbc`) is created in almost identical
#' ways for these supported backends:
#'
#' *Database* | *Connection object*
#' -------- | ---------
#' MongoDB | \code{dbc <- \link[nodbi:src_mongo]{nodbi::src_mongo}(db = "my_db", collection = "my_coll")}
#' DuckDB | \code{dbc <- \link[nodbi:src_duckdb]{nodbi::src_duckdb}(dbname = "my_db", collection = "my_coll")}
#' SQLite | \code{dbc <- \link[nodbi:src_sqlite]{nodbi::src_sqlite}(dbname = "my_db", collection = "my_coll")}
#' PostgreSQL | \code{dbc <- \link[nodbi:src_postgres]{nodbi::src_postgres}(dbname = "my_db"); dbc[["collection"]] <- "my_coll"}
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
#' \link{ctrShowOneTrial} (show widget to explore structure, fields and data of a trial),
#' \link{dbFindFields} (find names of fields of interest in trial records in a collection),
#' \link{dbGetFieldsIntoDf} (create a data frame with fields of interest and calculated trial concepts from collection),
#' \link{dbFindIdsUniqueTrials} (get de-duplicated identifiers of clinical trials' records to subset a data frame).
#'
#' @section 4 - Operate on a data frame with trial information:
#'
#' \link{dfTrials2Long} (convert fields with nested elements into long format),
#' \link{dfName2Value} (get values for variable(s) of interest).
#'
#' @name ctrdata
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords package
#' @md
#'
"_PACKAGE"
