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
#> # A tibble: 22 × 2
#>    `_id`             .trialObjectives
#>    <chr>             <chr>           
#>  1 12949496          S PD A          
#>  2 13281214          E PD A          
#>  3 17473621          NA              
#>  4 2016-003884-20-DE E S PK          
#>  5 2019-000338-20-ES E S PK          
#>  6 2019-002663-10-ES E S PK A        
#>  7 2022-500244-37-00 E S A           
#>  8 2023-505613-24-00 S PK A          
#>  9 2023-508143-51-01 S PD            
#> 10 2024-510663-34-00 S PK            
#> # ℹ 12 more rows
```
