# Calculate details of a study's primary endpoint analysis and testing

Trial concept calculated: Calculates several results-related elements of
the primary analysis of the primary endpoint. Requires loading
results-related information. For CTIS and ISRCTN, such information is
not available in structured format. Recommended to be combined with
.controlType, .sampleSize etc. for analyses.

## Usage

``` r
f.primaryEndpointResults(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and new columns:
\`.primaryEndpointFirstPvalue\` (discarding any inequality indicator,
e.g. \<=), \`.primaryEndpointFirstPmethod\` (normalised string, e.g.
chisquared), \`.primaryEndpointFirstPsize\` (number included in test,
across assignment groups).

## Examples

``` r
# fields needed
f.primaryEndpointResults()
#> $euctr
#> [1] "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value"       
#> [2] "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value"
#> [3] "endPoints.endPoint.type.value"                                                                    
#> [4] "endPoints.endPoint"                                                                               
#> 
#> $ctgov
#> [1] "clinical_results.outcome_list.outcome.analysis_list.analysis.method" 
#> [2] "clinical_results.outcome_list.outcome.analysis_list.analysis.p_value"
#> [3] "clinical_results.outcome_list.outcome.type"                          
#> [4] "clinical_results.outcome_list.outcome"                               
#> 
#> $ctgov2
#> [1] "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValue"           
#> [2] "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.statisticalMethod"
#> [3] "resultsSection.outcomeMeasuresModule.outcomeMeasures.type"                      
#> [4] "resultsSection.outcomeMeasuresModule.outcomeMeasures"                           
#> 
#> $isrctn
#> NULL
#> 
#> $ctis
#> NULL
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.primaryEndpointResults",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (12 fields)...
#> Calculating f.primaryEndpointResults...                            
trialsDf
#> # A tibble: 11 × 4
#>    `_id`    .primaryEndpointFirs…¹ .primaryEndpointFirs…² .primaryEndpointFirs…³
#>    <chr>                     <dbl> <chr>                                   <dbl>
#>  1 2012-00…                 0.0134 other                                     185
#>  2 2012-00…                 0.0134 other                                     185
#>  3 2014-00…                NA      NA                                         61
#>  4 2014-00…                NA      NA                                         61
#>  5 NCT0061…                NA      NA                                         11
#>  6 NCT0112…                NA      NA                                         60
#>  7 NCT0148…                NA      NA                                          8
#>  8 NCT0150…                NA      NA                                         14
#>  9 NCT0159…                NA      NA                                         54
#> 10 NCT0262…                NA      NA                                         NA
#> 11 NCT0332…                NA      NA                                         NA
#> # ℹ abbreviated names: ¹​.primaryEndpointFirstPvalue,
#> #   ²​.primaryEndpointFirstPmethod, ³​.primaryEndpointFirstPsize
```
