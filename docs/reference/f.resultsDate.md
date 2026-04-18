# Calculate date of results of a study

Trial concept calculated: earliest date of results as recorded in the
register. At that date, results may have been incomplete and may have
been changed later. For EUCTR, requires that results and preferrably
also their history of publication have been included in the collection,
using ctrLoadQueryIntoDb(queryterm = ..., euctrresultshistory = TRUE,
con = ...). Cannot be calculated for ISRCTN, which does not have a
corresponding field.

## Usage

``` r
f.resultsDate(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.resultsDate\`, a date.

## Examples

``` r
# fields needed
f.resultsDate()
#> $euctr
#> [1] "firstreceived_results_date"         "trialInformation.analysisStageDate"
#> 
#> $ctgov
#> [1] "results_first_posted"
#> 
#> $ctgov2
#> [1] "protocolSection.statusModule.resultsFirstPostDateStruct.date"
#> 
#> $isrctn
#> [1] "results.intentToPublish"
#> 
#> $ctis
#> [1] "results.summaryResults.submissionDate"  
#> [2] "results.clinicalStudyReports.submitDate"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.resultsDate",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (7 fields)...
#> , typing fields...
#> Calculating f.resultsDate...                            

trialsDf
#> # A tibble: 11 × 2
#>    `_id`             .resultsDate
#>    <chr>             <date>      
#>  1 2016-004489-24-DE 2022-05-20  
#>  2 2019-002663-10-ES 2024-05-30  
#>  3 2022-000099-20-DE 2025-06-18  
#>  4 NCT00567567       2017-06-27  
#>  5 NCT00716976       2017-06-01  
#>  6 NCT01305200       2017-05-09  
#>  7 NCT01987596       2020-10-29  
#>  8 NCT03275402       2024-02-13  
#>  9 NCT03325556       2021-06-21  
#> 10 NCT03443973       2024-01-17  
#> 11 NCT03548584       2023-09-18  
```
