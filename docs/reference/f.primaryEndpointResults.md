# Calculate details of a study's primary endpoint statistical testing

Trial concept calculated: Calculates several results-related elements of
the primary statistical analysis of the primary endpoint. Requires
loading results-related information. For CTIS and ISRCTN, such
information is not available in structured format. Recommended to be
combined with
[f.controlType](https://rfhb.github.io/ctrdata/reference/f.controlType.md),
[f.sampleSize](https://rfhb.github.io/ctrdata/reference/f.sampleSize.md),
[f.assignmentType](https://rfhb.github.io/ctrdata/reference/f.assignmentType.md)
and other
[ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
for analyses.

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
#> [1] "endPoints.endPoint.type.value" "endPoints.endPoint"           
#> 
#> $ctgov
#> [1] "clinical_results.outcome_list.outcome.type"
#> [2] "clinical_results.outcome_list.outcome"     
#> 
#> $ctgov2
#> [1] "resultsSection.outcomeMeasuresModule.outcomeMeasures.type"
#> [2] "resultsSection.outcomeMeasuresModule.outcomeMeasures"     
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
#> Querying database (6 fields)...
#> Calculating f.primaryEndpointResults...                            

trialsDf
#> # A tibble: 9 × 4
#>   `_id`     .primaryEndpointFirs…¹ .primaryEndpointFirs…² .primaryEndpointFirs…³
#>   <chr>                      <dbl> <chr>                                   <dbl>
#> 1 2016-003…                NA      NA                                         NA
#> 2 2019-000…                NA      NA                                         NA
#> 3 2019-002…                 0.0001 ancova                                     70
#> 4 NCT00506…                NA      NA                                         NA
#> 5 NCT00567…                 0.0082 logrank                                   355
#> 6 NCT00716…                NA      NA                                         NA
#> 7 NCT01035…                NA      NA                                         NA
#> 8 NCT01305…                NA      NA                                         NA
#> 9 NCT01955…                 0.959  mixedmodelsanalysis                       922
#> # ℹ abbreviated names: ¹​.primaryEndpointFirstPvalue,
#> #   ²​.primaryEndpointFirstPmethod, ³​.primaryEndpointFirstPsize
```
