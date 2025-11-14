# Calculate objectives of a study

Trial concept calculated: objectives of the trial, by searching for text
fragments found in fields describing its purpose, objective, background
or hypothesis, after applying .isMedIntervTrial, because the text
fragments are tailored to medicinal product interventional trials. This
is a simplification, and it is expected that the criteria will be
further refined. The text fragments only apply to English.

## Usage

``` r
f.trialObjectives(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.trialObjectives\`, which is a
string with letters separated by a space, such as E (efficacy, including
cure, survival, effectiveness); A (activity, including reponse,
remission, seroconversion); S (safety); PK; PD (including biomarker); D
(dose-finding, determining recommended dose); LT (long-term); and FU
(follow-up).

## Examples

``` r
# fields needed
f.trialObjectives()
#> $euctr
#> [1] "e21_main_objective_of_the_trial" "e64_safety"                     
#> [3] "e65_efficacy"                    "e66_pharmacokinetic"            
#> [5] "e67_pharmacodynamic"             "e69_dose_response"              
#> [7] "ctrname"                        
#> 
#> $ctgov
#> [1] "brief_summary.textblock"           "detailed_description.textblock"   
#> [3] "study_design_info.primary_purpose" "intervention.intervention_type"   
#> [5] "study_type"                       
#> 
#> $ctgov2
#> [1] "protocolSection.descriptionModule.briefSummary"            
#> [2] "protocolSection.descriptionModule.detailedDescription"     
#> [3] "protocolSection.designModule.designInfo.primaryPurpose"    
#> [4] "protocolSection.armsInterventionsModule.interventions.type"
#> [5] "protocolSection.designModule.studyType"                    
#> 
#> $isrctn
#> [1] "trialDescription.studyHypothesis"           
#> [2] "trialDesign.trialType"                      
#> [3] "interventions.intervention.interventionType"
#> [4] "trialDesign.primaryStudyDesign"             
#> 
#> $ctis
#> [1] "authorizedPartI.trialDetails.trialInformation.trialObjective.mainObjective"                                     
#> [2] "authorizedPartI.trialDetails.trialInformation.trialObjective.trialScopes.otherDescription"                      
#> [3] "authorizedPartI.trialDetails.trialInformation.trialObjective.trialScopes.code"                                  
#> [4] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialObjective.mainObjective"               
#> [5] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialObjective.trialScopes.otherDescription"
#> [6] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialObjective.trialScopes.code"            
#> [7] "ctrname"                                                                                                        
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.trialObjectives",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (27 fields)...
#> Calculating f.trialObjectives...                            
trialsDf
#> # A tibble: 31 × 2
#>    `_id`             .trialObjectives
#>    <chr>             <chr>           
#>  1 12949496          S PD A          
#>  2 13281214          E PD A          
#>  3 17473621          NA              
#>  4 2012-003632-23-CZ E S PD PK A D   
#>  5 2012-003632-23-SE E S PD PK A D   
#>  6 2014-002606-20-PT E S             
#>  7 2014-003556-31-GB E S LT          
#>  8 2014-003556-31-SE E S LT          
#>  9 2022-500244-37-00 E S A           
#> 10 2022-501142-30-00 E S A           
#> # ℹ 21 more rows
```
