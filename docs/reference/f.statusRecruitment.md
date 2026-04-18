# Calculate status of recruitment of a study

Trial concept calculated: status of recruitment at the time of loading
the trial records. Maps the categories that are in fields which specify
the state of recruitment. Simplifies the status into three categories.

## Usage

``` r
f.statusRecruitment(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.statusRecruitment\`, which is a
factor with levels \`ongoing\` (includes active, not yet recruiting;
temporarily halted; suspended; authorised, not started and similar),
\`completed\` (includes ended; ongoing, recruitment ended), \`ended
early\` (includes prematurely ended, terminated early) and \`other\`
(includes revoked, withdrawn, planned, stopped).

## Details

Note that for EUCTR, \`NA\` is returned for "Trial now transitioned"
(into CTIS, from which the status can be obtained) and for "GB - no
longer in EU/EEA" (no data source known).

## Examples

``` r
# fields needed
f.statusRecruitment()
#> $euctr
#> [1] "trialInformation.globalEndOfTrialPremature"
#> [2] "trialInformation.isGlobalEndOfTrialReached"
#> [3] "p_end_of_trial_status"                     
#> 
#> $ctgov
#> [1] "last_known_status" "overall_status"   
#> 
#> $ctgov2
#> [1] "protocolSection.statusModule.overallStatus"
#> 
#> $isrctn
#> [1] "participants.recruitmentEnd"           
#> [2] "participants.recruitmentStart"         
#> [3] "participants.recruitmentStatusOverride"
#> 
#> $ctis
#> [1] "authorizedApplication.memberStatesConcerned.mscName"                    
#> [2] "mscTrialNotificationsInfoList.mscNotificationsListInfo.notificationType"
#> [3] "events.trialEvents.events.notificationType"                             
#> [4] "ctPublicStatusCode"                                                     
#> [5] "ctStatus"                                                               
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.statusRecruitment",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (14 fields)...
#> , typing fields...
#> Calculating f.statusRecruitment...                            

trialsDf
#> # A tibble: 24 × 2
#>    `_id`             .statusRecruitment
#>    <chr>             <fct>             
#>  1 12949496          other             
#>  2 13281214          completed         
#>  3 17473621          completed         
#>  4 2016-004489-24-DE completed         
#>  5 2019-002663-10-ES completed         
#>  6 2022-000099-20-DE completed         
#>  7 2022-500244-37-00 ongoing           
#>  8 2023-505613-24-00 ongoing           
#>  9 2023-508143-51-01 ongoing           
#> 10 2024-510663-34-00 ongoing           
#> # ℹ 14 more rows
```
