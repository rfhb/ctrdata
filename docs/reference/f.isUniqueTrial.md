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
#> , 31 found in collection
#> - Finding duplicates among registers' and sponsor ids...
#> - 2 EUCTR _id were not preferred EU Member State record for 11 trials
#> - Unique are 8 / 5 / 5 / 3 / 8 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
#> = Returning keys (_id) of 29 records in collection "my_trials"
trialsDf
#> # A tibble: 31 × 3
#>    `_id`             ctrname .isUniqueTrial
#>    <chr>             <chr>   <lgl>         
#>  1 12949496          ISRCTN  TRUE          
#>  2 13281214          ISRCTN  TRUE          
#>  3 17473621          ISRCTN  TRUE          
#>  4 2012-003632-23-CZ EUCTR   TRUE          
#>  5 2012-003632-23-SE EUCTR   FALSE         
#>  6 2014-002606-20-PT EUCTR   TRUE          
#>  7 2014-003556-31-GB EUCTR   FALSE         
#>  8 2014-003556-31-SE EUCTR   TRUE          
#>  9 2022-500244-37-00 CTIS    TRUE          
#> 10 2022-501142-30-00 CTIS    TRUE          
#> # ℹ 21 more rows
```
