# Calculate type of control data collected in a study

Trial concept calculated: type of internal control. ICH E10 lists as
types of control: placebo concurrent control, no-treatment concurrent
control, dose-response concurrent control, active (positive) concurrent
control, external (including historical) control, multiple control
groups. Dose-controlled trials are currently not identified. External
(including historical) controls are so far not identified in specific
register fields. Cross-over designs, where identifiable, have active
controls.

## Usage

``` r
f.controlType(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.controlType\`, which is a factor
with levels \`none\`, \`no-treatment\`, \`placebo\`, \`active\`,
\`placebo+active\` and \`other\`.

## Examples

``` r
# fields needed
f.controlType()
#> $euctr
#> [1] "e81_controlled"                                                                   
#> [2] "e816_cross_over"                                                                  
#> [3] "e822_placebo"                                                                     
#> [4] "e823_other"                                                                       
#> [5] "e8231_comparator_description"                                                     
#> [6] "e824_number_of_treatment_arms_in_the_trial"                                       
#> [7] "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value"
#> 
#> $ctgov
#> [1] "arm_group.arm_group_type"
#> 
#> $ctgov2
#> [1] "protocolSection.armsInterventionsModule.armGroups.type"
#> 
#> $isrctn
#> [1] "trialDesign.studyDesign"        "trialDesign.primaryStudyDesign"
#> 
#> $ctis
#> [1] "authorizedPartI.productRoleGroupInfos.productRoleName"                      
#> [2] "authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  field = "ctrname",
  calculate = "f.controlType",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (14 fields)...
#> Calculating f.controlType...                            
trialsDf
#> # A tibble: 31 × 3
#>    `_id`             ctrname .controlType
#>    <chr>             <chr>   <fct>       
#>  1 12949496          ISRCTN  none        
#>  2 13281214          ISRCTN  none        
#>  3 17473621          ISRCTN  other       
#>  4 2012-003632-23-CZ EUCTR   active      
#>  5 2012-003632-23-SE EUCTR   active      
#>  6 2014-002606-20-PT EUCTR   none        
#>  7 2014-003556-31-GB EUCTR   active      
#>  8 2014-003556-31-SE EUCTR   active      
#>  9 2022-500244-37-00 CTIS    placebo     
#> 10 2022-501142-30-00 CTIS    active      
#> # ℹ 21 more rows
```
