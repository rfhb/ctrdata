# Calculate if study is likely a platform trial or not

Trial concept calculated: platform trial, research platform. As
operational definition, at least one of these criteria is true: a. trial
has "platform", "basket", "umbrella", "multi.?arm", "multi.?stage" or
"master protocol" in its title or description (for ISRCTN, this is the
only criterion; some trials in EUCTR lack data in English), b. trial has
more than 2 active arms with different investigational medicines, after
excluding comparator, auxiliary and placebo medicines (calculated with
[f.numTestArmsSubstances](https://rfhb.github.io/ctrdata/reference/f.numTestArmsSubstances.md);
not used for ISRCTN because it cannot be calculated precisely), c. trial
has more than 2 periods, after excluding safety run-in, screening,
enrolling, extension and follow-up periods (for CTGOV and CTGOV2, this
criterion requires results-related data).

## Usage

``` r
f.likelyPlatformTrial(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.likelyPlatformTrial\`, a
logical, and two complementary columns, each with lists of identifiers:
\`.likelyRelatedTrials\` (based on other identifiers provided in the
trial record, including \`associatedClinicalTrials\` from CTIS; listing
identifiers whether or not the trial with the other identifier is in the
database collection) and \`.maybeRelatedTrials\` (based on similar short
terms in the first set of brackets or before a colon in the trial title;
only listing identifiers from the database collection).

## Details

For EUCTR, requires that results have been included in the collection,
using \`ctrLoadQueryIntoDb(queryterm = ..., euctrresults = TRUE, con =
...)\`. Requires packages dplyr and stringdist to be installed;
stringdist is used for evaluating terms in brackets in the trial title,
where trials may be related if the term similarity is 0.77 or higher.

Publication references considered: [EU-PEARL WP2
2020](https://web.archive.org/web/20230314024441/https://eu-pearl.eu/wp-content/uploads/2020/06/EU-PEARL_D2.1_Report-on-Terminology-and-Scenarios-for-Platform-Trials-and-Masterprotocols.pdf)
and Williams RJ et al. 2022,
[doi:10.1136/bmj-2021-067745](https://doi.org/10.1136/bmj-2021-067745)

## Examples

``` r
# fields needed
f.likelyPlatformTrial()
#> [[1]]
#> [1] "ctrname"
#> 
#> $euctr
#> [1] "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title"   
#> [2] "e822_placebo"                                                          
#> [3] "e823_other"                                                            
#> [4] "e824_number_of_treatment_arms_in_the_trial"                            
#> [5] "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm"
#> [6] "a3_full_title_of_the_trial"                                            
#> 
#> $ctgov
#> [1] "clinical_results.participant_flow.period_list.period.title"
#> [2] "arm_group"                                                 
#> [3] "official_title"                                            
#> 
#> $ctgov2
#> [1] "resultsSection.participantFlowModule.periods.title"
#> [2] "protocolSection.armsInterventionsModule.armGroups" 
#> [3] "protocolSection.identificationModule.officialTitle"
#> 
#> $isrctn
#> [1] "interventions.intervention.interventionType"
#> [2] "trialDesign.secondaryStudyDesign"           
#> [3] "interventions.intervention.drugNames"       
#> [4] "interventions.intervention.interventionType"
#> [5] "trialDescription.scientificTitle"           
#> 
#> $ctis
#> [1] "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title"                      
#> [2] "authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber"                                        
#> [3] "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title"
#> [4] "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber"                  
#> [5] "authorizedPartI.productRoleGroupInfos"                                                                 
#> [6] "authorizedApplication.authorizedPartI.productRoleGroupInfos"                                           
#> [7] "authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle"                                       
#> [8] "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle"                 
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.likelyPlatformTrial",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (25 fields)...
#> Calculating f.likelyPlatformTrial...                            
#> Searching for duplicate trials... 
#> - Getting all trial identifiers...
#> , 31 found in collection
trialsDf
#> # A tibble: 31 × 4
#>    `_id`           .likelyPlatformTrial .likelyRelatedTrials .maybeRelatedTrials
#>    <chr>           <lgl>                <list>               <list>             
#>  1 12949496        FALSE                <chr [1]>            <chr [1]>          
#>  2 13281214        FALSE                <chr [1]>            <chr [1]>          
#>  3 17473621        FALSE                <lgl [1]>            <chr [1]>          
#>  4 2012-003632-23… FALSE                <chr [2]>            <chr [1]>          
#>  5 2012-003632-23… FALSE                <chr [2]>            <chr [1]>          
#>  6 2014-002606-20… FALSE                <chr [1]>            <chr [1]>          
#>  7 2014-003556-31… FALSE                <chr [2]>            <chr [1]>          
#>  8 2014-003556-31… FALSE                <chr [2]>            <chr [1]>          
#>  9 2022-500244-37… FALSE                <chr [1]>            <chr [1]>          
#> 10 2022-501142-30… FALSE                <lgl [1]>            <chr [1]>          
#> # ℹ 21 more rows
```
