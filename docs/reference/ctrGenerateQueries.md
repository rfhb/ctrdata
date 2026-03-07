# Generates queries that work across registers

From high-level search terms provided by the user, generate specific
queries for each registers with which ctrdata works, see
[ctrdata-registers](https://rfhb.github.io/ctrdata/reference/ctrdata-registers.md).

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
  searched in selected fields of registers (general or title fields),
  should not include quotation marks

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

  String, one of "ongoing", "completed", "other" (which includes "ended
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
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md),
[ctrOpenSearchPagesInBrowser](https://rfhb.github.io/ctrdata/reference/ctrOpenSearchPagesInBrowser.md)
and [browseURL](https://rdrr.io/r/utils/browseURL.html)

## Details

Search terms that are expanded to concepts such as from MeSH and MedDRA
by the search implementations in registers include the 'intervention'
and 'condition'. Logical operators only work in parameter
'searchPhrase'.

## Examples

``` r

ctrGenerateQueries(
  searchPhrase = "antibody AND covid",
  recruitment = "ongoing")
#>                                                                                                                                                                                                                                                                                                                                                                                 EUCTR 
#>                                                                                                                                                                             "https://www.clinicaltrialsregister.eu/ctr-search/search?query=\"antibody\" AND \"covid\"&status=ongoing&status=trial-now-transitioned&status=suspended-by-ca&status=temporarily-halted&status=restarted" 
#>                                                                                                                                                                                                                                                                                                                                                                                ISRCTN 
#>                                                                                                                           "https://www.isrctn.com/search?q=\"antibody\" AND \"covid\"&filters=trialStatus:ongoing,primaryStudyDesign:Interventional,phase:Phase 0,phase:Phase I,phase:Phase II,phase:Phase III,phase:Phase IV,phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV" 
#>                                                                                                                                                                                                                                                                                                                                                                                CTGOV2 
#>                                                                                                                                                                         "https://clinicaltrials.gov/search?term=(\"antibody\" AND \"covid\") AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT))&intr=Drug OR Biological&aggFilters=status:act rec,studyType:int" 
#>                                                                                                                                                                                                                                                                                                                                                                          CTGOV2expert 
#> "https://clinicaltrials.gov/expert-search?term=(\"antibody\" AND \"covid\") AND (AREA[OverallStatus]\"ACTIVE_NOT_RECRUITING\" OR AREA[OverallStatus]\"ENROLLING_BY_INVITATION\" OR AREA[OverallStatus]\"RECRUITING\") AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL))" 
#>                                                                                                                                                                                                                                                                                                                                                                                  CTIS 
#>                                                                                                                                                                                                                                                           "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"containAll\":\"antibody, covid\",\"status\":[2,3,4,6,7]}" 

urls <- ctrGenerateQueries(
  intervention = "antibody",
  phase = "phase 3",
  population = "P+A",
  startAfter = "12/31/2020",
  completedBefore = "2025-01-01")
#> Parameter 'population = "P+A"' cannot be used with ISRCTN to indicate either population can be recruited
#> Parameter 'startAfter = "2020-12-31"' in EUCTR refers to the date when the trial was first entered into the EudraCT database
#> Parameter 'completedBefore = "2025-01-01"' cannot be used for queries in EUCTR

# open queries in register web interface
sapply(urls, utils::browseURL)
#> Error in FUN(X[[i]], ...): 'browser' must be a non-empty character string

# For CTIS to accept such a search URL and show results,
# consider the extension, script and documentation at
# https://rfhb.github.io/ctrdata/index.html#
# id_2-script-to-automatically-copy-users-query-from-web-browser

# find potential research platform and platform trials
urls <- ctrGenerateQueries(
  searchPhrase = paste0(
   "basket OR platform OR umbrella OR master protocol OR ",
   "multiarm OR multistage OR subprotocol OR substudy OR ",
   "multi-arm OR multi-stage OR sub-protocol OR sub-study"),
 phase = "phase 3",
 startAfter = "01/31/2010",
 countries = c("DE", "US", "United Kingdom"))
#> Parameter 'startAfter = "2010-01-31"' in EUCTR refers to the date when the trial was first entered into the EudraCT database

if (FALSE) { # \dontrun{
# count trials found
lapply(urls, ctrLoadQueryIntoDb, only.count = TRUE)

# load queries into database collection
dbc <- nodbi::src_sqlite(collection = "my_collection")
sapply(urls, ctrLoadQueryIntoDb, con = dbc, euctrprotocolsall = FALSE)
} # }
```
