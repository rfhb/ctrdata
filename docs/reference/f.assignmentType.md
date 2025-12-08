# Calculate type of assignment to intervention in a study

Calculate type of assignment to intervention in a study

## Usage

``` r
f.assignmentType(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.assignmentType\`, which is a
factor with levels \`R\` (randomised assignment) and \`NR\` (all other
types of assignment).

## Examples

``` r
# fields needed
f.assignmentType()
#> $euctr
#> [1] "e811_randomised"                                                               
#> [2] "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.allocation.value"
#> 
#> $ctgov
#> [1] "study_design_info.allocation"
#> 
#> $ctgov2
#> [1] "protocolSection.designModule.designInfo.allocation"
#> 
#> $isrctn
#> [1] "trialDesign.studyDesign"
#> 
#> $ctis
#> [1] "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod"                      
#> [2] "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod"
#> 
#> [[6]]
#> [1] "ctrname"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  field = "ctrname",
  calculate = "f.assignmentType",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (8 fields)...
#> Calculating f.assignmentType...                            

trialsDf
#> # A tibble: 31 × 3
#>    `_id`             ctrname .assignmentType
#>    <chr>             <chr>   <fct>          
#>  1 12949496          ISRCTN  R              
#>  2 13281214          ISRCTN  R              
#>  3 17473621          ISRCTN  R              
#>  4 2012-003632-23-CZ EUCTR   NR             
#>  5 2012-003632-23-SE EUCTR   NR             
#>  6 2014-002606-20-PT EUCTR   R              
#>  7 2014-003556-31-GB EUCTR   NR             
#>  8 2014-003556-31-SE EUCTR   NR             
#>  9 2022-500244-37-00 CTIS    NR             
#> 10 2022-501142-30-00 CTIS    R              
#> # ℹ 21 more rows
```
