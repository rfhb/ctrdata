# Calculate start date of a study

Trial concept calculated: start of the trial, based on the documented or
planned start of recruitment, or on the date of opinion of the competent
authority.

## Usage

``` r
f.startDate(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.startDate\`, a date.

## Examples

``` r
# fields needed
f.startDate()
#> $euctr
#> [1] "n_date_of_competent_authority_decision"
#> [2] "n_date_of_ethics_committee_opinion"    
#> [3] "trialInformation.recruitmentStartDate" 
#> 
#> $ctgov
#> [1] "start_date"
#> 
#> $ctgov2
#> [1] "protocolSection.statusModule.startDateStruct.date"
#> 
#> $isrctn
#> [1] "participants.recruitmentStart" "trialDesign.overallStartDate" 
#> 
#> $ctis
#> [1] "startDateEU"                                                                                                    
#> [2] "authorizationDate"                                                                                              
#> [3] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  field = "ctrname",
  calculate = "f.startDate",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (11 fields)...
#> , typing fields...
#> Calculating f.startDate...                            

trialsDf
#> # A tibble: 24 × 3
#>    `_id`             ctrname .startDate
#>    <chr>             <chr>   <date>    
#>  1 12949496          ISRCTN  2015-11-01
#>  2 13281214          ISRCTN  2016-09-01
#>  3 17473621          ISRCTN  2023-05-01
#>  4 2016-004489-24-DE EUCTR   2019-12-16
#>  5 2019-002663-10-ES EUCTR   2020-05-06
#>  6 2022-000099-20-DE EUCTR   2022-08-08
#>  7 2022-500244-37-00 CTIS    2022-08-03
#>  8 2023-505613-24-00 CTIS    2021-04-28
#>  9 2023-508143-51-01 CTIS    2024-05-06
#> 10 2024-510663-34-00 CTIS    2023-01-16
#> # ℹ 14 more rows
```
