# Calculate if record is unique for a study

Trial concept calculated: Applies function dbFindIdsUniqueTrials() with
its defaults.

## Usage

``` r
f.isUniqueTrial(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.isUniqueTrial\`, a logical.

## Examples

``` r
# fields needed
f.isUniqueTrial()
#> [1] "ctrname"

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  fields = "ctrname",
  calculate = "f.isUniqueTrial",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (1 fields)...
#> Calculating f.isUniqueTrial...                            

#> Searching for duplicate trials... 
#> - Getting all trial identifiers...
#> , 24 found in collection
#> - Finding duplicates among registers' and sponsor ids...
#> - Unique are 3 / 5 / 4 / 3 / 9 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
#> = Returning keys (_id) of 24 records in collection "my_trials"
trialsDf
#> # A tibble: 24 × 3
#>    `_id`             ctrname .isUniqueTrial
#>    <chr>             <chr>   <lgl>         
#>  1 12949496          ISRCTN  TRUE          
#>  2 13281214          ISRCTN  TRUE          
#>  3 17473621          ISRCTN  TRUE          
#>  4 2016-004489-24-DE EUCTR   TRUE          
#>  5 2019-002663-10-ES EUCTR   TRUE          
#>  6 2022-000099-20-DE EUCTR   TRUE          
#>  7 2022-500244-37-00 CTIS    TRUE          
#>  8 2023-505613-24-00 CTIS    TRUE          
#>  9 2023-508143-51-01 CTIS    TRUE          
#> 10 2024-510663-34-00 CTIS    TRUE          
#> # ℹ 14 more rows
```
