# Generates queries that work across registers

From high-level search terms provided by the user, generate specific
queries for each registers with which ctrdata works, see
[ctrdata-registers](https://rfhb.github.io/ctrdata/reference/ctrdata-registers.md).
Search terms that are expanded to concepts such as from MeSH and MedDRA
by the search implementations in registers include the 'intervention'
and 'condition'. Logical operators only work with 'searchPhrase'.

## Usage

``` r
ctrGenerateQueries(
  searchPhrase = NULL,
  condition = NULL,
  intervention = NULL,
  phase = NULL,
  population = NULL,
  recruitment = NULL,
  startBefore = NULL,
  startAfter = NULL,
  completedBefore = NULL,
  completedAfter = NULL,
  onlyMedIntervTrials = TRUE,
  onlyWithResults = FALSE,
  countries = NULL
)
```

## Arguments

- searchPhrase:

  String with optional logical operators ("AND", "OR") that will be
  searched in selected fields of registers that can handle logical
  operators (general or title fields), should not include quotation
  marks

- condition:

  String with condition / disease

- intervention:

  String with intervention

- phase:

  String, e.g. "phase 2" (note that "phase 2+3" is a specific category,
  not the union set of "phase 2" and "phase 3")

- population:

  String, e.g. "P" (paediatric), "A" (adult), "P+A" (adult and
  paediatric), "E" (elderly), "P+A+E" participants can be recruited. For
  ISRCTN, works only for "P", "A" or "E" but *not mixed* populations.

- recruitment:

  String, one of "ongoing", "completed", "other" ( which includes "ended
  early" but this cannot be searched; use trial concept
  [f.statusRecruitment](https://rfhb.github.io/ctrdata/reference/f.statusRecruitment.md)
  to identify this status)

- startBefore:

  String that can be interpreted as date (for EUCTR, when trial was
  first registered)

- startAfter:

  String that can be interpreted as date (for EUCTR, when trial was
  first registered)

- completedBefore:

  String that can be interpreted as date (does not work with EUCTR)

- completedAfter:

  String that can be interpreted as date (does not work with EUCTR)

- onlyMedIntervTrials:

  Logical, default `TRUE`, which indicates if queries should search only
  for medicine interventional clinical trial

- onlyWithResults:

  Logical

- countries:

  Vector of country names, two- or three-letter ISO 3166 codes

## Value

Named vector of URLs for finding trials in the registers and as input to
functions
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
and
[ctrOpenSearchPagesInBrowser](https://rfhb.github.io/ctrdata/reference/ctrOpenSearchPagesInBrowser.md)

## Examples

``` r

urls <- ctrGenerateQueries(
  intervention = "antibody",
  phase = "phase 3",
  startAfter = "2000-01-01")

# open queries in register web interface
sapply(urls, ctrOpenSearchPagesInBrowser)
#> * Found search query from EUCTR: query=antibody&phase=phase-three&dateFrom=2000-01-01
#> * Found search query from ISRCTN: &q=&filters=intervention:antibody,phase:Phase III,GT+overallStartDate:2000-01-01,primaryStudyDesign:Interventional
#> * Found search query from CTGOV2: intr=(antibody) AND (Drug OR Biological)&start=2000-01-01_&term=AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)&aggFilters=phase:3,studyType:int
#> * Found search query from CTGOV2: term=AREA[InterventionSearch]"antibody" AND (AREA[Phase]"PHASE3") AND AREA[StartDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL))
#> * Found search query from CTIS: searchCriteria={"containAll":"antibody","trialPhaseCode":[5],"eeaStartDateFrom":"2000-01-01"}
#>                                                                                                                                                                                                                                                                                                                    EUCTR 
#>                                                                                                                                                                                                      "https://www.clinicaltrialsregister.eu/ctr-search/search?query=antibody&phase=phase-three&dateFrom=2000-01-01#tabs" 
#>                                                                                                                                                                                                                                                                                                                   ISRCTN 
#>                                                                                                                                                                       "https://www.isrctn.com/search?&q=&filters=intervention:antibody,phase:Phase III,GT+overallStartDate:2000-01-01,primaryStudyDesign:Interventional" 
#>                                                                                                                                                                                                                                                                                                                   CTGOV2 
#>                                                                                                                   "https://clinicaltrials.gov/search?intr=(antibody) AND (Drug OR Biological)&start=2000-01-01_&term=AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)&aggFilters=phase:3,studyType:int" 
#>                                                                                                                                                                                                                                                                                                             CTGOV2expert 
#> "https://clinicaltrials.gov/expert-search?term=AREA[InterventionSearch]\"antibody\" AND (AREA[Phase]\"PHASE3\") AND AREA[StartDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL))" 
#>                                                                                                                                                                                                                                                                                                                     CTIS 
#>                                                                                                                                                                 "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"containAll\":\"antibody\",\"trialPhaseCode\":[5],\"eeaStartDateFrom\":\"2000-01-01\"}" 

# For CTIS to accept such a search URL and show results, consider installing
# the Tampermonkey browser extension from https://www.tampermonkey.net/,
# click on the extension icon, "Create a new script", "Utility" and then
# "Import from this URL":
# https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js

# find potential research platform and platform trials
urls <- ctrGenerateQueries(
  searchPhrase = paste0(
   "basket OR platform OR umbrella OR master protocol OR ",
   "multiarm OR multistage OR subprotocol OR substudy OR ",
   "multi-arm OR multi-stage OR sub-protocol OR sub-study"),
 phase = "phase 3",
 startAfter = "01/31/2010",
 countries = c("DE", "US", "United Kingdom"))

# open queries in register web interface
sapply(urls, ctrOpenSearchPagesInBrowser)
#> * Found search query from EUCTR: query="basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study"&phase=phase-three&dateFrom=2010-01-31&country=de&country=gb&country=3rd
#> * Found search query from ISRCTN: q="basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study"&filters=phase:Phase III,GT+overallStartDate:2010-01-31,primaryStudyDesign:Interventional,recruitmentCountry:Germany,recruitmentCountry:United Kingdom,recruitmentCountry:United States of America
#> * Found search query from CTGOV2: term=("basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study") AND (AREA[Phase]"PHASE3") AND AREA[StartDate]RANGE[2010-01-31,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL)) AND (AREA[LocationCountry]"Germany" OR AREA[LocationCountry]"United Kingdom" OR AREA[LocationCountry]"United States of America")
#> * Found search query from CTGOV2: term=("basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study") AND (AREA[Phase]"PHASE3") AND AREA[StartDate]RANGE[2010-01-31,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL)) AND (AREA[LocationCountry]"Germany" OR AREA[LocationCountry]"United Kingdom" OR AREA[LocationCountry]"United States of America")
#> * Found search query from CTIS: searchCriteria={"containAny":"basket, platform, umbrella, master protocol, multiarm, multistage, subprotocol, substudy, multi-arm, multi-stage, sub-protocol, sub-study","trialPhaseCode":[5],"eeaStartDateFrom":"2010-01-31","msc":[276,826,840]}
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        EUCTR 
#>                                                                                                                                                                                                                                                                                 "https://www.clinicaltrialsregister.eu/ctr-search/search?query=\"basket\" OR \"platform\" OR \"umbrella\" OR \"master protocol\" OR \"multiarm\" OR \"multistage\" OR \"subprotocol\" OR \"substudy\" OR \"multi-arm\" OR \"multi-stage\" OR \"sub-protocol\" OR \"sub-study\"&phase=phase-three&dateFrom=2010-01-31&country=de&country=gb&country=3rd#tabs" 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ISRCTN 
#>                                                                                                                                                                                          "https://www.isrctn.com/search?q=\"basket\" OR \"platform\" OR \"umbrella\" OR \"master protocol\" OR \"multiarm\" OR \"multistage\" OR \"subprotocol\" OR \"substudy\" OR \"multi-arm\" OR \"multi-stage\" OR \"sub-protocol\" OR \"sub-study\"&filters=phase:Phase III,GT+overallStartDate:2010-01-31,primaryStudyDesign:Interventional,recruitmentCountry:Germany,recruitmentCountry:United Kingdom,recruitmentCountry:United States of America" 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       CTGOV2 
#> "https://clinicaltrials.gov/expert-search?term=(\"basket\" OR \"platform\" OR \"umbrella\" OR \"master protocol\" OR \"multiarm\" OR \"multistage\" OR \"subprotocol\" OR \"substudy\" OR \"multi-arm\" OR \"multi-stage\" OR \"sub-protocol\" OR \"sub-study\") AND (AREA[Phase]\"PHASE3\") AND AREA[StartDate]RANGE[2010-01-31,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL)) AND (AREA[LocationCountry]\"Germany\" OR AREA[LocationCountry]\"United Kingdom\" OR AREA[LocationCountry]\"United States of America\")" 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 CTGOV2expert 
#> "https://clinicaltrials.gov/expert-search?term=(\"basket\" OR \"platform\" OR \"umbrella\" OR \"master protocol\" OR \"multiarm\" OR \"multistage\" OR \"subprotocol\" OR \"substudy\" OR \"multi-arm\" OR \"multi-stage\" OR \"sub-protocol\" OR \"sub-study\") AND (AREA[Phase]\"PHASE3\") AND AREA[StartDate]RANGE[2010-01-31,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL)) AND (AREA[LocationCountry]\"Germany\" OR AREA[LocationCountry]\"United Kingdom\" OR AREA[LocationCountry]\"United States of America\")" 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         CTIS 
#>                                                                                                                                                                                                                                                                                                                              "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"containAny\":\"basket, platform, umbrella, master protocol, multiarm, multistage, subprotocol, substudy, multi-arm, multi-stage, sub-protocol, sub-study\",\"trialPhaseCode\":[5],\"eeaStartDateFrom\":\"2010-01-31\",\"msc\":[276,826,840]}" 

urls <- ctrGenerateQueries(
  searchPhrase = "antibody AND covid",
  recruitment = "completed",
  )

if (FALSE) { # \dontrun{
# count trials found
sapply(urls, ctrLoadQueryIntoDb, only.count = TRUE)

# load queries into database collection
dbc <- nodbi::src_sqlite(collection = "my_collection")
sapply(urls, ctrLoadQueryIntoDb, con = dbc, euctrprotocolsall = FALSE)
} # }
```
