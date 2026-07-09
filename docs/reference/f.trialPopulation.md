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
#> Typing fields...
#> Calculating f.trialPopulation...                            

trialsDf
#> # A tibble: 24 × 4
#>    `_id`             .trialPopulationAgeGroup .trialPopulationInclusion         
#>    <chr>             <fct>                    <chr>                             
#>  1 12949496          P                        "All infants with a gestation ≥ 3…
#>  2 13281214          P                        "All infants aged less than 29 we…
#>  3 17473621          P                        "1. Infants with gestational age …
#>  4 2016-004489-24-DE P                        "Inclusion Criteria: A subject mu…
#>  5 2019-002663-10-ES P+A                      "Part A, B and C: - Genetic docum…
#>  6 2022-000099-20-DE P                        "- Born at  29 weeks gestational …
#>  7 2022-500244-37-00 A+E                      "Diagnosis of symptomatic COVID-1…
#>  8 2023-505613-24-00 P                        "Panel A: Is undergoing treatment…
#>  9 2023-508143-51-01 A+E                      "In order to be eligible to parti…
#> 10 2024-510663-34-00 P                        "Age. Neonates and infants who ar…
#> # ℹ 14 more rows
#> # ℹ 1 more variable: .trialPopulationExclusion <chr>
```
