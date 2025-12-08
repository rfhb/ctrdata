# Calculate in- and exclusion criteria and age groups

Trial concept calculated: inclusion and exclusion criteria as well as
age groups that can participate in a trial, based on protocol-related
information. Since CTGOV uses single text field for eligibility
criteria, text extraction is used to separate in- and exclusion
criteria. (See
[dfMergeVariablesRelevel](https://rfhb.github.io/ctrdata/reference/dfMergeVariablesRelevel.md)
with an example for healthy volunteers.)

## Usage

``` r
f.trialPopulation(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and new columns:
\`.trialPopulationAgeGroup\` (factor, "P", "A", "P+A", "E", "A+E",
"P+A+E"), \`.trialPopulationInclusion\` (string),
\`.trialPopulationExclusion\` (string).

## Examples

``` r
# fields needed
f.trialPopulation()
#> $euctr
#>  [1] "e3_principal_inclusion_criteria"                             
#>  [2] "e4_principal_exclusion_criteria"                             
#>  [3] "f111_in_utero"                                               
#>  [4] "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks"
#>  [5] "f113_newborns_027_days"                                      
#>  [6] "f114_infants_and_toddlers_28_days23_months"                  
#>  [7] "f115_children_211years"                                      
#>  [8] "f116_adolescents_1217_years"                                 
#>  [9] "f11_trial_has_subjects_under_18"                             
#> [10] "f12_adults_1864_years"                                       
#> [11] "f13_elderly_65_years"                                        
#> 
#> $ctgov
#> [1] "eligibility.criteria.textblock" "eligibility.maximum_age"       
#> [3] "eligibility.minimum_age"       
#> 
#> $ctgov2
#> [1] "protocolSection.eligibilityModule.maximumAge"         
#> [2] "protocolSection.eligibilityModule.minimumAge"         
#> [3] "protocolSection.eligibilityModule.eligibilityCriteria"
#> [4] "protocolSection.eligibilityModule.stdAges"            
#> 
#> $isrctn
#> [1] "participants.ageRange"  "participants.inclusion" "participants.exclusion"
#> 
#> $ctis
#> [1] "ageGroup"                                                                                                                                     
#> [2] "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria"                      
#> [3] "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria"                      
#> [4] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria"
#> [5] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria"
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.trialPopulation",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (26 fields)...
#> Calculating f.trialPopulation...                            

trialsDf
#> # A tibble: 31 × 4
#>    `_id`    .trialPopulationAgeG…¹ .trialPopulationIncl…² .trialPopulationExcl…³
#>    <chr>    <fct>                  <chr>                  <chr>                 
#>  1 12949496 P                      All infants with a ge… 1. Lethal congenital …
#>  2 13281214 P                      All infants aged less… 1. Lack of consent or…
#>  3 17473621 P                      1. Infants with gesta… 1. Genetic, metabolic…
#>  4 2012-00… P+A                    1. Primary HLH patien… 1. Diagnosis of secon…
#>  5 2012-00… P+A                    1. Primary HLH patien… 1. Diagnosis of secon…
#>  6 2014-00… P                      1. Children 3 months … 1. Anticoagulant trea…
#>  7 2014-00… P                      Each subject must mee… Subjects who meet any…
#>  8 2014-00… P                      Each subject must mee… Subjects who meet any…
#>  9 2022-50… A+E                    Diagnosis of symptoma… Patients with an indi…
#> 10 2022-50… A                      Age > 18 years old / … Unable to understand …
#> # ℹ 21 more rows
#> # ℹ abbreviated names: ¹​.trialPopulationAgeGroup, ²​.trialPopulationInclusion,
#> #   ³​.trialPopulationExclusion
```
