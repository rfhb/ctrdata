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
#> Querying database (9 fields)...
#> Calculating f.externalLinks...                            

trialsDf
#> # A tibble: 10 × 2
#>    `_id`       .externalLinks                                                   
#>    <chr>       <chr>                                                            
#>  1 12949496    2018 Protocol article in https://www.ncbi.nlm.nih.gov/pubmed/305…
#>  2 13281214    2021 Results article in https://pubmed.ncbi.nlm.nih.gov/33069668…
#>  3 76463425    2017 Protocol article in https://www.ncbi.nlm.nih.gov/pubmed/281…
#>  4 80181452    2010 Protocol article in http://www.ncbi.nlm.nih.gov/pubmed/1959…
#>  5 88261002    2018 Results article in https://www.ncbi.nlm.nih.gov/pubmed/3057…
#>  6 NCT01483820 Beat Childhood Cancer Consortium http://www.beatcc.org           
#>  7 NCT01505608 Beat Childhood Cancer https://beatcc.org/                        
#>  8 NCT03325439 Pressler R, Boylan G, Dempsey E, Klotz KA, Krauwinkel W, Will E,…
#>  9 NCT03431558 http://data.worldbank.org/indicator/SH.DYN.NMRT http://www.un.or…
#> 10 NCT03876704 Fares S, Sethom MM, Khouaja-Mokrani C, Jabnoun S, Feki M, Kaabac…
```
