# Calculate phase of a clinical trial

Trial concept calculated: phase of a clinical trial as per ICH E8(R1).

## Usage

``` r
f.trialPhase(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.trialPhase\`, which is an
ordered factor with levels \`phase 1\`, \`phase 1+2\`, \`phase 2\`,
\`phase 2+3\`, \`phase 2+4\`, \`phase 3\`, \`phase 3+4\`, \`phase
1+2+3\`, \`phase 4\`, \`phase 1+2+3+4\`.

## Examples

``` r
# fields needed
f.trialPhase()
#> $euctr
#> [1] "e71_human_pharmacology_phase_i"        
#> [2] "e72_therapeutic_exploratory_phase_ii"  
#> [3] "e73_therapeutic_confirmatory_phase_iii"
#> [4] "e74_therapeutic_use_phase_iv"          
#> 
#> $ctgov
#> [1] "phase"
#> 
#> $ctgov2
#> [1] "protocolSection.designModule.phases"
#> 
#> $isrctn
#> [1] "interventions.intervention.phase"
#> 
#> $ctis
#> [1] "authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase"                      
#> [2] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.trialPhase",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (9 fields)...
#> Calculating f.trialPhase...                            

trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .trialPhase
#>    <chr>             <ord>      
#>  1 12949496          phase 3    
#>  2 13281214          phase 3    
#>  3 17473621          phase 3    
#>  4 2012-003632-23-CZ phase 2    
#>  5 2012-003632-23-SE phase 2+3  
#>  6 2014-002606-20-PT phase 3    
#>  7 2014-003556-31-GB phase 2    
#>  8 2014-003556-31-SE phase 2    
#>  9 2022-500244-37-00 phase 2    
#> 10 2022-501142-30-00 phase 4    
#> # ℹ 21 more rows
```
