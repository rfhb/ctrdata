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
#> Calculating f.resultsDate...                            
trialsDf
#> # A tibble: 14 × 2
#>    `_id`             .resultsDate
#>    <chr>             <date>      
#>  1 13281214          2020-11-01  
#>  2 17473621          2024-08-01  
#>  3 2012-003632-23-CZ 2019-01-31  
#>  4 2012-003632-23-SE 2019-01-31  
#>  5 2014-003556-31-GB 2021-09-28  
#>  6 2014-003556-31-SE 2021-09-28  
#>  7 76463425          2019-05-10  
#>  8 NCT00617929       2017-01-20  
#>  9 NCT01125800       2016-03-02  
#> 10 NCT01483820       2016-10-28  
#> 11 NCT01505608       2016-10-28  
#> 12 NCT01592045       2015-09-23  
#> 13 NCT02620761       2022-09-16  
#> 14 NCT03325439       2024-01-16  
```
