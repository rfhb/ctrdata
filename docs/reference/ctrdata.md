# Getting started, database connection, function overview

`ctrdata` is a package for aggregating and analysing data on clinical
studies, and for obtaining documents, from public trial registers

## 1 - Define a database connection

Package `ctrdata` retrieves trial data and stores it in a database
collection. The connection is specified using `nodbi`, which allows to
use different database backends in an identical way. A database
connection object is specified once and then can be used as parameter
`con` in subsequent calls of `ctrdata` functions. Specifying
`collection = "<my trial data collection's name>"` indicates the table
in the database that package `ctrdata` should use.

|  |  |
|----|----|
| *Database* | *Connection object* |
| SQLite | `dbc <- `[`nodbi::src_sqlite`](https://docs.ropensci.org/nodbi/reference/src_sqlite.html)`(dbname = "my_db", collection = "my_coll")` |
| DuckDB | `dbc <- `[`nodbi::src_duckdb`](https://docs.ropensci.org/nodbi/reference/src_duckdb.html)`(dbname = "my_db", collection = "my_coll")` |
| MongoDB | `dbc <- `[`nodbi::src_mongo`](https://docs.ropensci.org/nodbi/reference/src_mongo.html)`(db = "my_db", collection = "my_coll")` |
| PostgreSQL | `dbc <- `[`nodbi::src_postgres`](https://docs.ropensci.org/nodbi/reference/src_postgres.html)`(dbname = "my_db"); dbc[["collection"]] <- "my_coll"` |

## 2 - Load information from clinical trial registers

[ctrGenerateQueries](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
(generate from simple user input specific queries for registers EUCTR,
CTIS, CTGOV2 and ISRCTN),
[ctrOpenSearchPagesInBrowser](https://rfhb.github.io/ctrdata/reference/ctrOpenSearchPagesInBrowser.md)
(open queries in browser), see
[script](https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js)
(automatically copy user search in any register to clipboard), see
[ctrdata-registers](https://rfhb.github.io/ctrdata/reference/ctrdata-registers.md)
for details on registers and how to search,
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
(load trial records found with query into database collection).

## 3 - Use database with downloaded trial information

[ctrShowOneTrial](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
(show widget to explore structure, fields and data of a trial),
[dbFindFields](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
(find names of fields of interest in trial records in a collection),
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
(create a data frame with fields of interest and calculated trial
concepts from collection),
[ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
(calculate pre-defined trial concepts across registers),
[dbFindIdsUniqueTrials](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
(get de-duplicated identifiers of clinical trials' records to subset a
data frame).

## 4 - Operate on a trial data frame from dbGetFieldsIntoDf

[dfTrials2Long](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
(convert fields with nested elements into long format),
[dfName2Value](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
(get values for variable(s) of interest),
[ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
(calculate pre-defined trial concepts for every register).

## See also

Useful links:

- <https://cran.r-project.org/package=ctrdata>

- <https://rfhb.github.io/ctrdata/>

- Report bugs at <https://github.com/rfhb/ctrdata/issues>

## Author

Ralf Herold <ralf.herold@mailbox.org>
