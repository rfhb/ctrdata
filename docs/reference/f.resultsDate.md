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
#> # A tibble: 9 × 2
#>   `_id`             .resultsDate
#>   <chr>             <date>      
#> 1 2016-003884-20-DE 2023-07-06  
#> 2 2019-000338-20-ES 2024-05-07  
#> 3 2019-002663-10-ES 2024-05-30  
#> 4 NCT00506441       2014-07-25  
#> 5 NCT00567567       2017-06-27  
#> 6 NCT00716976       2017-06-01  
#> 7 NCT01035138       2014-09-25  
#> 8 NCT01305200       2017-05-09  
#> 9 NCT01955161       2017-09-19  
```
