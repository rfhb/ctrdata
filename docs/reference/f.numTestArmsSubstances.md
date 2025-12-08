# Calculate number of arms or groups with investigational medicines in a study

Trial concept calculated: number of active arms with different
investigational medicines, after excluding non-active comparator,
auxiliary and placebo arms / medicines. For ISRCTN, this is imprecise
because arms are not identified in a field. Most registers provide no or
only limited information on phase 1 trials, so that this number
typically cannot be calculated for these trials. Requires packages
stringdist to be installed; stringdist is used for evaluating names of
active substances, which are considered similar when the similarity is
0.8 or higher.

## Usage

``` r
f.numTestArmsSubstances(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.numTestArmsSubstances\`, an
integer

## Examples

``` r
# fields needed
f.numTestArmsSubstances()
#> $euctr
#> [1] "e822_placebo"                                                          
#> [2] "e823_other"                                                            
#> [3] "e824_number_of_treatment_arms_in_the_trial"                            
#> [4] "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm"
#> 
#> $ctgov
#> [1] "arm_group"
#> 
#> $ctgov2
#> [1] "protocolSection.armsInterventionsModule.armGroups"
#> 
#> $isrctn
#> [1] "trialDesign.secondaryStudyDesign"           
#> [2] "interventions.intervention.drugNames"       
#> [3] "interventions.intervention.interventionType"
#> 
#> $ctis
#> [1] "authorizedPartI.productRoleGroupInfos"                      
#> [2] "authorizedApplication.authorizedPartI.productRoleGroupInfos"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.numTestArmsSubstances",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (11 fields)...
#> Calculating f.numTestArmsSubstances...                            

trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .numTestArmsSubstances
#>    <chr>                              <int>
#>  1 12949496                               1
#>  2 13281214                               1
#>  3 17473621                              NA
#>  4 2012-003632-23-CZ                      1
#>  5 2012-003632-23-SE                      1
#>  6 2014-002606-20-PT                      2
#>  7 2014-003556-31-GB                      1
#>  8 2014-003556-31-SE                      1
#>  9 2022-500244-37-00                      1
#> 10 2022-501142-30-00                      1
#> # ℹ 21 more rows
```
