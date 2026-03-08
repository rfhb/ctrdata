# Show history of queries loaded into collection

Note that a new entry is added to the history of queries when
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
retrieves at least 1 clinical trial.

## Usage

``` r
dbQueryHistory(con, verbose = FALSE)
```

## Arguments

- con:

  A database connection object, created with `nodbi`. See section \`1 -
  Database connection\` in
  [ctrdata](https://rfhb.github.io/ctrdata/reference/ctrdata.md).

- verbose:

  If `TRUE`, prints additional information (default `FALSE`).

## Value

A data frame (or tibble, if `tibble` is loaded) with columns:
\`query-timestamp\`, \`query-register\`, \`query-records\` (note: this
is the number of records loaded when last executing
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md),
not the total record number) and \`query-term\`, with one row for each
time that
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
loaded trial records into this collection.

## Examples

``` r

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

dbQueryHistory(con = dbc)
#> # A tibble: 6 × 4
#>   `query-timestamp`   `query-register` `query-records` `query-term`             
#>   <chr>               <chr>                      <int> <chr>                    
#> 1 2024-06-17 20:35:31 CTIS                           2 ""                       
#> 2 2026-03-08 17:24:53 EUCTR                          3 "query=&age=newborn&stat…
#> 3 2023-10-23 10:55:45 CTGOV                          3 "cond=MCI+OR+Alzheimer&t…
#> 4 2026-03-08 17:34:31 ISRCTN                         9 "q=&filters=ageRange:Neo…
#> 5 2026-03-08 17:36:40 CTGOV2                         5 "distance=50&cond=neurob…
#> 6 2026-03-08 17:37:07 CTIS                           2 "searchCriteria={\"conta…
```
