# Calculate type of sponsor of a study

Trial concept calculated: type or class of the sponsor(s) of the study.
No specific field is available in ISRCTN; thus, sponsor type is set to
\`other\`. Note: If several sponsors, sponsor type is deemed \`mixed\`
*if there is both, a commercial and a non-commercial sponsor(s)*.

## Usage

``` r
f.sponsorType(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.sponsorType\`, which is a factor
with levels \`for profit\`, \`not for profit\`, \`mixed\` (not and for
profit sponsors) or \`other\`.

## Examples

``` r
# fields needed
f.sponsorType()
#> $euctr
#> [1] "b1_sponsor.b31_and_b32_status_of_the_sponsor"
#> 
#> $ctgov
#> [1] "sponsors.collaborator.agency_class" "sponsors.lead_sponsor.agency_class"
#> 
#> $ctgov2
#> [1] "protocolSection.sponsorCollaboratorsModule.collaborators.class"
#> [2] "protocolSection.sponsorCollaboratorsModule.leadSponsor.class"  
#> 
#> $isrctn
#> [1] "ctrname"
#> 
#> $ctis
#> [1] "sponsorType"                                                
#> [2] "primarySponsor.commercial"                                  
#> [3] "primarySponsor.isCommercial"                                
#> [4] "coSponsors.commercial"                                      
#> [5] "coSponsors.isCommercial"                                    
#> [6] "authorizedApplication.authorizedPartI.sponsors.commercial"  
#> [7] "authorizedApplication.authorizedPartI.sponsors.isCommercial"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.sponsorType",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (13 fields)...
#> Calculating f.sponsorType...                            
trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .sponsorType  
#>    <chr>             <fct>         
#>  1 12949496          other         
#>  2 13281214          other         
#>  3 17473621          other         
#>  4 2012-003632-23-CZ for profit    
#>  5 2012-003632-23-SE for profit    
#>  6 2014-002606-20-PT for profit    
#>  7 2014-003556-31-GB for profit    
#>  8 2014-003556-31-SE for profit    
#>  9 2022-500244-37-00 mixed         
#> 10 2022-501142-30-00 not for profit
#> # ℹ 21 more rows
```
