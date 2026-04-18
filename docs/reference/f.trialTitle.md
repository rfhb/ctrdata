# Calculate the title of a study

Trial concept calculated: scientific or full title of the study.

## Usage

``` r
f.trialTitle(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.trialTitle\`, a string.

## Examples

``` r
# fields needed
f.trialTitle()
#> $euctr
#> [1] "a3_full_title_of_the_trial"
#> 
#> $ctgov
#> [1] "official_title"
#> 
#> $ctgov2
#> [1] "protocolSection.identificationModule.officialTitle"
#> 
#> $isrctn
#> [1] "trialDescription.scientificTitle"
#> 
#> $ctis
#> [1] "authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle"                      
#> [2] "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.trialTitle",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (6 fields)...
#> , typing fields...
#> Calculating f.trialTitle...                            

trialsDf
#> # A tibble: 24 × 2
#>    `_id`             .trialTitle                                                
#>    <chr>             <chr>                                                      
#>  1 12949496          "The use of milrinone in neonates with persistent pulmonar…
#>  2 13281214          "A randomised controlled trial of early targeted patent du…
#>  3 17473621          "The effectiveness of the “BLUI Blanket” light emitting di…
#>  4 2016-004489-24-DE "A Prospective, Randomised, Active-Controlled, Single-blin…
#>  5 2019-002663-10-ES "Escalating Dose and Randomized, Controlled Study of Nusin…
#>  6 2022-000099-20-DE "A Phase IIIb randomized open-label study of nirsevimab ve…
#>  7 2022-500244-37-00 "A 4-week double-blind, randomized, placebo-controlled, ph…
#>  8 2023-505613-24-00 "A Phase 2, Open-Label, Single-Arm, Sequential-Panel Study…
#>  9 2023-508143-51-01 "Pulmonary function and sleep related disorders during cer…
#> 10 2024-510663-34-00 "An open label, single arm study to evaluate single and mu…
#> # ℹ 14 more rows
```
