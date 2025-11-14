# Show history of queries loaded into collection

Show history of queries loaded into collection

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
#> # A tibble: 5 × 4
#>   `query-timestamp`   `query-register` `query-records` `query-term`             
#>   <chr>               <chr>                      <int> <chr>                    
#> 1 2024-05-16 14:05:23 EUCTR                          5 "query=&age=newborn&phas…
#> 2 2024-05-16 14:06:43 CTGOV                          8 "term=AREA[MaximumAge]+R…
#> 3 2024-05-16 14:07:32 ISRCTN                         8 "q=&filters=ageRange:Neo…
#> 4 2024-05-16 14:07:57 CTGOV2                         5 "distance=50&cond=neurob…
#> 5 2024-06-23 16:14:56 CTIS                           3 "searchCriteria={\"conta…
```
