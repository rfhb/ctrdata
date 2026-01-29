# Calculate if study is a medicine-interventional study

Trial concept calculated: Calculates if record is a
medicine-interventional trial, investigating one or more medicine,
whether biological or not. For EUCTR and CTIS, this corresponds to all
records as per the definition of the EU Clinical Trial Regulation. For
CTGOV and CTGOV2, this is based on drug or biological as type of
intervention, and interventional as type of study. For ISRCTN, this is
based on drug or biological as type of intervention, and interventional
as type of study.

## Usage

``` r
f.isMedIntervTrial(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with colums \`\_id\` and \`.isMedIntervTrial\`, a logical.

## Examples

``` r
# fields needed
f.isMedIntervTrial()
#> $euctr
#> [1] "ctrname"
#> 
#> $ctgov
#> [1] "intervention.intervention_type" "study_type"                    
#> 
#> $ctgov2
#> [1] "protocolSection.armsInterventionsModule.interventions.type"
#> [2] "protocolSection.designModule.studyType"                    
#> 
#> $isrctn
#> [1] "interventions.intervention.interventionType"
#> [2] "trialDesign.primaryStudyDesign"             
#> 
#> $ctis
#> [1] "ctrname"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.isMedIntervTrial",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (7 fields)...
#> Calculating f.isMedIntervTrial...                            
trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .isMedIntervTrial
#>    <chr>             <lgl>            
#>  1 12949496          TRUE             
#>  2 13281214          TRUE             
#>  3 17473621          FALSE            
#>  4 2012-003632-23-CZ TRUE             
#>  5 2012-003632-23-SE TRUE             
#>  6 2014-002606-20-PT TRUE             
#>  7 2014-003556-31-GB TRUE             
#>  8 2014-003556-31-SE TRUE             
#>  9 2022-500244-37-00 TRUE             
#> 10 2022-501142-30-00 TRUE             
#> # ℹ 21 more rows
```
