#' Getting started, database connection, function overview
#'
#' `ctrdata` is a package for aggregating and analysing data on clinical
#' studies, and for obtaining documents, from public trial registers
#'
#' @section 1 - Define a database connection:
#' Package `ctrdata` retrieves trial data and stores it in a database
#' collection. The connection is specified using \code{nodbi}, which allows to
#' use different database backends in an identical way.
#' A database connection object is specified once and then can be used as
#' parameter `con` in subsequent calls of `ctrdata` functions.
#' Specifying \code{collection = "<my trial data collection's name>"}
#' indicates the table in the database that package `ctrdata` should use.
#'
#' *Database* | *Connection object*
#' -------- | ---------
#' SQLite | \code{dbc <- \link[nodbi:src_sqlite]{nodbi::src_sqlite}(dbname = "my_db", collection = "my_coll")}
#' DuckDB | \code{dbc <- \link[nodbi:src_duckdb]{nodbi::src_duckdb}(dbname = "my_db", collection = "my_coll")}
#' MongoDB | \code{dbc <- \link[nodbi:src_mongo]{nodbi::src_mongo}(db = "my_db", collection = "my_coll")}
#' PostgreSQL | \code{dbc <- \link[nodbi:src_postgres]{nodbi::src_postgres}(dbname = "my_db"); dbc[["collection"]] <- "my_coll"}
#'
#' @section 2 - Load information from clinical trial registers:
#'
#' \link{ctrGenerateQueries} (generate from simple user input specific queries
#' for registers EUCTR, CTIS, CTGOV2 and ISRCTN),
#' \link{ctrOpenSearchPagesInBrowser} (open queries in browser),
#' see \href{https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js}{script}
#' (automatically copy user search in any register to clipboard),
#' see \link{ctrdata-registers} for details on registers and how to search,
#' \link{ctrLoadQueryIntoDb} (load trial records found with query into database collection).
#'
#' @section 3 - Use database with downloaded trial information:
#'
#' \link{ctrShowOneTrial} (show widget to explore structure, fields and data of a trial),
#' \link{dbFindFields} (find names of fields of interest in trial records in a collection),
#' \link{dbGetFieldsIntoDf} (create a data frame with fields of interest and calculated trial concepts from collection),
#' \link{ctrdata-trial-concepts} (calculate pre-defined trial concepts for every register),
#' \link{dbFindIdsUniqueTrials} (get de-duplicated identifiers of clinical trials' records to subset a data frame).
#'
#' @section 4 - Operate on a trial data frame from dbGetFieldsIntoDf:
#'
#' \link{dfTrials2Long} (convert fields with nested elements into long format),
#' \link{dfName2Value} (get values for variable(s) of interest),
#' \link{ctrdata-trial-concepts} (calculate pre-defined trial concepts for every register).
#'
#' @name ctrdata
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords package
#' @md
#'
"_PACKAGE"
