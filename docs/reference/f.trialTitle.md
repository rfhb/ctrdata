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
#> Calculating f.trialTitle...                            
trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .trialTitle                                                
#>    <chr>             <chr>                                                      
#>  1 12949496          The use of milrinone in neonates with persistent pulmonary…
#>  2 13281214          A randomised controlled trial of early targeted patent duc…
#>  3 17473621          The effectiveness of the “BLUI Blanket” light emitting dio…
#>  4 2012-003632-23-CZ A Pilot, Open-label, Single Arm, Multicentre Study to Expl…
#>  5 2012-003632-23-SE A Phase 2/3, Open-label, Single Arm, Multicentre Study to …
#>  6 2014-002606-20-PT A RANDOMIZED, OPEN-LABEL, ACTIVE CONTROLLED, SAFETY AND DE…
#>  7 2014-003556-31-GB Long-term Outcome of Children Enrolled in Study ROPP-2008-…
#>  8 2014-003556-31-SE Long-term Outcome of Children Enrolled in Study ROPP-2008-…
#>  9 2022-500244-37-00 A 4-week double-blind, randomized, placebo-controlled, pha…
#> 10 2022-501142-30-00 Balloon + Oxytocin versus Oral Misoprostol to Induce labor…
#> # ℹ 21 more rows
```
