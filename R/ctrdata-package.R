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
#'
#' *Database* | *Connection object*
#' -------- | ---------
#' SQLite | \code{dbc <- \link[nodbi:src_sqlite]{nodbi::src_sqlite}(dbname = "my_db", collection = "my_coll")}
#' DuckDB* | \code{dbc <- \link[nodbi:src_duckdb]{nodbi::src_duckdb}(dbname = "my_db", collection = "my_coll")}
#' MongoDB | \code{dbc <- \link[nodbi:src_mongo]{nodbi::src_mongo}(db = "my_db", collection = "my_coll")}
#' PostgreSQL | \code{dbc <- \link[nodbi:src_postgres]{nodbi::src_postgres}(dbname = "my_db"); dbc[["collection"]] <- "my_coll"}
#'
#' *For DuckDB, the JSON extension is needed which can be permanently downloaded as follows.
#' User sets a directory of choice for each new R session:
#' \code{options(duckdb.extension_directory = "~/.duckdb_extensions")}
#' Load and store in this directory; needs to be executed only once after a DuckDB install:
#' \code{DBI::dbExecute(duckdb::dbConnect(duckdb::duckdb()), 'INSTALL json;')}
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
#' \link{ctrdata-trial-concepts} (calculate pre-defined trial concepts across registers),
#' \link{dbFindIdsUniqueTrials} (get de-duplicated identifiers of clinical trials' records to subset a data frame).
#'
#' @section 4 - Operate on a trial data frame from dbGetFieldsIntoDf:
#'
#' \link{ctrdata-trial-concepts} (calculate pre-defined trial concepts across registers),
#' \link{dfTrials2Long} (convert fields with nested elements into long format),
#' \link{dfName2Value} (get values for variable(s) of interest).
#'
#' @name ctrdata
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords package
#' @md
#'
"_PACKAGE"
