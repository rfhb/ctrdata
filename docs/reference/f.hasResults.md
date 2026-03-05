# Calculate if a study's results are available

Trial concept calculated: Calculates if results have been recorded in
the register, as structured data, reports or publications, for example.
Requires loading results-related information for EUCTR.

## Usage

``` r
f.hasResults(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and new column \`.hasResults\`
(logical).

## Examples

``` r
# fields needed
f.hasResults()
#> $euctr
#> [1] "endPoints.endPoint.readyForValues"
#> 
#> $ctgov
#> [1] "results_reference.citation"                
#> [2] "clinical_results.outcome_list.outcome.type"
#> 
#> $ctgov2
#> [1] "hasResults"                                               
#> [2] "protocolSection.referencesModule.references.type"         
#> [3] "protocolSection.statusModule.resultsFirstSubmitDate"      
#> [4] "resultsSection.outcomeMeasuresModule.outcomeMeasures.type"
#> 
#> $isrctn
#> [1] "results.publicationStage"
#> 
#> $ctis
#> [1] "results.clinicalStudyReports.id" "results.laypersonResults.id"    
#> [3] "results.summaryResults.id"       "resultsFirstReceived"           
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.hasResults",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (12 fields)...
#> Calculating f.hasResults...                            

trialsDf
#> # A tibble: 17 × 2
#>    `_id`             .hasResults
#>    <chr>             <lgl>      
#>  1 12949496          TRUE       
#>  2 13281214          TRUE       
#>  3 17473621          FALSE      
#>  4 2016-003884-20-DE TRUE       
#>  5 2019-000338-20-ES TRUE       
#>  6 2019-002663-10-ES TRUE       
#>  7 2023-505613-24-00 FALSE      
#>  8 2024-510663-34-00 FALSE      
#>  9 76463425          TRUE       
#> 10 80181452          TRUE       
#> 11 88261002          TRUE       
#> 12 NCT00506441       TRUE       
#> 13 NCT00567567       TRUE       
#> 14 NCT00716976       TRUE       
#> 15 NCT01035138       TRUE       
#> 16 NCT01305200       TRUE       
#> 17 NCT01955161       TRUE       
```
