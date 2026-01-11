# Calculate sample size of a study

Trial concept calculated: sample size of the trial, preferring
results-related (achieved recruitment) over protocol-related information
(planned sample size). Thus, the calculated number depends on the status
of the recruitment (see
[f.statusRecruitment](https://rfhb.github.io/ctrdata/reference/f.statusRecruitment.md))
and on the availability of results data; *for "CTIS", the number always
corresponds to the planned sample size*.

## Usage

``` r
f.sampleSize(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.sampleSize\`, an integer.

## Examples

``` r
# fields needed
f.sampleSize()
#> $euctr
#>  [1] "trialInformation.countrySubjectCounts.countrySubjectCount.subjects"
#>  [2] "f11_number_of_subjects_for_this_age_range"                         
#>  [3] "f1111_number_of_subjects_for_this_age_range"                       
#>  [4] "f1121_number_of_subjects_for_this_age_range"                       
#>  [5] "f1131_number_of_subjects_for_this_age_range"                       
#>  [6] "f1141_number_of_subjects_for_this_age_range"                       
#>  [7] "f1151_number_of_subjects_for_this_age_range"                       
#>  [8] "f1161_number_of_subjects_for_this_age_range"                       
#>  [9] "f121_number_of_subjects_for_this_age_range"                        
#> [10] "f131_number_of_subjects_for_this_age_range"                        
#> 
#> $ctgov
#> [1] "enrollment"
#> 
#> $ctgov2
#> [1] "protocolSection.designModule.enrollmentInfo.count"
#> 
#> $isrctn
#> [1] "participants.targetEnrolment"     "participants.totalFinalEnrolment"
#> 
#> $ctis
#> [1] "authorizedPartsII.recruitmentSubjectCount"                      
#> [2] "authorizedPartI.rowSubjectCount"                                
#> [3] "authorizedApplication.authorizedPartsII.recruitmentSubjectCount"
#> [4] "authorizedApplication.authorizedPartI.rowSubjectCount"          
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.sampleSize",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (18 fields)...
#> Calculating f.sampleSize...                            

trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .sampleSize
#>    <chr>                   <int>
#>  1 12949496                    9
#>  2 13281214                   60
#>  3 17473621                  100
#>  4 2012-003632-23-CZ          45
#>  5 2012-003632-23-SE          45
#>  6 2014-002606-20-PT         300
#>  7 2014-003556-31-GB          76
#>  8 2014-003556-31-SE          76
#>  9 2022-500244-37-00         440
#> 10 2022-501142-30-00         520
#> # ℹ 21 more rows
```
