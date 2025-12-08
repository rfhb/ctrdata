# Calculate number of sites of a study

Trial concept calculated: number of the sites where the trial is
conducted. EUCTR lacks information on number of sites outside of the
EEA; for each non-EEA country mentioned, at least one site is assumed.

## Usage

``` r
f.numSites(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.numSites\`, an integer.

## Examples

``` r
# fields needed
f.numSites()
#> $euctr
#> [1] "e83single_site_trial"                                      
#> [2] "e841_number_of_sites_anticipated_in_member_state_concerned"
#> [3] "e85_the_trial_involves_multiple_member_states"             
#> [4] "e851_number_of_sites_anticipated_in_the_eea"               
#> [5] "e863_trial_sites_planned_in"                               
#> 
#> $ctgov
#> [1] "location.facility.address.city"
#> 
#> $ctgov2
#> [1] "protocolSection.contactsLocationsModule.locations.city"
#> 
#> $isrctn
#> [1] "participants.trialCentres.trialCentre.name"
#> 
#> $ctis
#> [1] "authorizedPartsII.trialSites.id"                      
#> [2] "authorizedApplication.authorizedPartsII.trialSites.id"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.numSites",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (10 fields)...
#> Calculating f.numSites...                            

trialsDf
#> # A tibble: 30 × 2
#>    `_id`             .numSites
#>    <chr>                 <int>
#>  1 12949496                  2
#>  2 13281214                  1
#>  3 17473621                  1
#>  4 2012-003632-23-CZ        18
#>  5 2012-003632-23-SE        17
#>  6 2014-002606-20-PT        19
#>  7 2014-003556-31-GB        16
#>  8 2014-003556-31-SE        16
#>  9 2022-500244-37-00         2
#> 10 2022-501142-30-00         5
#> # ℹ 20 more rows
```
