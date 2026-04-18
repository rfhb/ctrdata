# Calculate the external references from a study's register record

Trial concept calculated: Calculates the links e.g. to publications or
other external files referenced from a study record. Requires loading
results-related information for EUCTR. Note that documents stored in
registers can be downloaded directly, see
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md).

## Usage

``` r
f.externalLinks(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and new column \`.externalLinks\`
(character).

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
  calculate = "f.externalLinks",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (11 fields)...
#> , typing fields...
#> Calculating f.externalLinks...                            

trialsDf
#> # A tibble: 10 × 2
#>    `_id`       .externalLinks                                                   
#>    <chr>       <chr>                                                            
#>  1 12949496    "2022 Results article in https://pubmed.ncbi.nlm.nih.gov/3638564…
#>  2 13281214    "2021 Results article in https://pubmed.ncbi.nlm.nih.gov/3306966…
#>  3 76463425    "2019 Results article in https://www.ncbi.nlm.nih.gov/pubmed/315…
#>  4 80181452    "2015 Results article in https://www.nejm.org/doi/full/10.1056/N…
#>  5 88261002    "2018 Results article in https://www.ncbi.nlm.nih.gov/pubmed/305…
#>  6 NCT00567567 "Data Available: Select individual patient-level data from this …
#>  7 NCT00716976 "Freyer DR, Chen L, Krailo MD, Knight K, Villaluna D, Bliss B, P…
#>  8 NCT01305200 "Data Available: Select individual patient-level data from this …
#>  9 NCT03275402 "Prasad K, Serencsits BE, Chu BP, Dauer LT, Donzelli M, Basu E, …
#> 10 NCT03325556 "Tariot P, et al. HARMONY study: pimavanserin significantly prol…
```
