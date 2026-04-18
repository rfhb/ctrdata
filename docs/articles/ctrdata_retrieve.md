# Retrieve clinical trial information

## Get started

### Attach package `ctrdata`

``` r

library(ctrdata)
citation("ctrdata")
```

Remember to respect the registers’ terms and conditions (see
`ctrOpenSearchPagesInBrowser(copyright = TRUE)`).

In any publication, please cite this package as follows:

Herold R (2026). “Aggregating and analysing clinical trials data from
multiple public registers using R package ctrdata.” *Research Synthesis
Methods*, *17*(3), 624-656. ISSN 1759-2879, 1759-2887,
<doi:10.1017/rsm.2025.10061> <https://doi.org/10.1017/rsm.2025.10061>.
or\
Herold R (2026). *ctrdata: Retrieve and Analyze Clinical Trials Data
from Public Registers*. R package version 1.26.1.9000,
<https://cran.r-project.org/package=ctrdata>.

### Open register’s advanced search page in browser

These functions open the browser, where the user can start searching for
trials of interest.

``` r

# Please review and respect register copyrights:
ctrOpenSearchPagesInBrowser(
  copyright = TRUE
)

# Open browser with example search:
ctrOpenSearchPagesInBrowser(
  url = "cancer&age=under-18&resultsstatus=trials-with-results",
  register = "EUCTR"
)
```

### Adjust search parameters and execute search in browser

Refine the search until the trials of interest are listed in the
browser. The total number of trials that can be retrieved with package
`ctrdata` is intentionally limited to queries with at most 10,000 result
records.

### Copy address from browser address bar to clipboard

Use functions or keyboard shortcuts according to the operating system.

See
[here](https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser)
for our automation to copy the URLs of a user’s queries in any of the
supported clinical trial registers.

### Get address from clipboard

The next steps are executed in the R environment:

``` r

q <- ctrGetQueryUrl()
# * Using clipboard content as register query URL: https://www.clinicaltrialsregister.eu/
# ctr-search/search?query=cancer&age=under-18&resultsstatus=trials-with-results
# * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results

q
#                                                    query-term  query-register
# 1 query=cancer&age=under-18&resultsstatus=trials-with-results           EUCTR

# To check, this opens a browser with the query
ctrOpenSearchPagesInBrowser(url = q)
```

### Connect database, retrieve, transform, save and check

Note that in addition to protocol- and results-related information, also
all trial documents that made publicly available by the registers,
including any protocols, consent forms and results reports, can be
downloaded by `ctrdata`, by specifying parameter `documents.path`, see
[`help(ctrLoadQueryIntoDb)`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md).

``` r

# Count number of trial records
ctrLoadQueryIntoDb(
  queryterm = q,
  only.count = TRUE
)$n
# * Checking trials in EUCTR, found 409 trials 
# [1] 409

# Connect to a database and chose a collection (table)
db <- nodbi::src_sqlite(
  dbname = "database_name.sql",
  collection = "test"
)

# Retrieve records, load into database
ctrLoadQueryIntoDb(
  queryterm = q,
  con = db
)
# * Checking trials in EUCTR, found 409 trials 
# - Downloading in 21 batch(es) (20 trials each; estimate: 30 s)...
# - Downloading 1628 records of 409 trials (estimate: 80 s)...         
# - Converting to NDJSON (estimate: 4 s)...                                
# - Importing records into database...
# = Imported or updated 1628 records on 409 trial(s) 
# No history found in expected format.
# Updated history ("meta-info" in "test")
# $n
# [1] 1628

# Show which queries have been downloaded into database
dbQueryHistory(con = db)
#       query-timestamp query-register query-records
# 1 2026-03-07 16:51:19          EUCTR          1628
#                                                    query-term
# 1 query=cancer&age=under-18&resultsstatus=trials-with-results
```

With any database, this takes about 100 seconds for 1628 records of 409
trials.

## Loading only a single version of EUCTR protocol-related data

As part of the acceleration of `ctrdata` operations on the EUCTR
(starting version 1.23.0), it is possible as of 2025-07-20 to limit
protocol-related data to loading a *single* version instead of loading
versions from *all* countries in which a trial is conducted, by setting
`euctrprotocolsall = FALSE` when using
[`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md).

The background is that EUCTR has protocol records of a trial separately
for every EU Member State (and a third country, if any), there are often
multiple records of a trial to be loaded. Versions of protocols are
mostly identical across Member States, but each version increases the
time needed for loading. In contrast to protocol-related data which
relate to countries, results-data in EUCTR relate to the trial and only
a single version of results is available and loaded per trial.

``` r

# Retrieve records, load into database
ctrLoadQueryIntoDb(
  queryterm = q, 
  euctrprotocolsall = FALSE,
  con = db
)
# * Checking trials in EUCTR, found 409 trials 
# - Downloading in 21 batch(es) (20 trials each; estimate: 30 s)...
# - Downloading 409 records of 409 trials (estimate: 20 s)...
# - Converting to NDJSON (estimate: 1 s)... 
# - Importing records into database...
# = Imported or updated 409 records on 409 trial(s)
# Updated history ("meta-info" in "test")
# $n
# [1] 409
```

When run as alternative to the preceding section, this takes about 30
seconds for 409 records of 409 trials.

## Repeat and update a previous query

Previously executed queries can be repeated by specifying “last” or an
integer number for parameter `querytoupdate`, where the number
corresponds to the row number of the query shown with
[`dbQueryHistory()`](https://rfhb.github.io/ctrdata/reference/dbQueryHistory.md).
Where possible, the query to update first checks for new records in the
register. Depending on the register and time since running the query
last, an update (differential update) is possible or the original query
is executed fully again.

``` r

# Show all queries
dbQueryHistory(con = db)

# Repeat last query
ctrLoadQueryIntoDb(
  querytoupdate = "last",
  only.count = TRUE,
  con = db
)
# * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results
# * Query last run: 2026-03-07
# * Checking for new or updated trials...
# First result page empty - no (new) trials found?
# Updated history ("meta-info" in "test")
# $n
# [1] 0
```

## Results-related trial information

For CTGOV and CTGOV2, any results are always included in the retrieval.
Only for EUCTR, result-related trial information has to be requested to
be retrieved, because it will take longer to download and store. No
results in structured electronic format are foreseeably available from
ISRCTN and CTIS, thus `ctrdata` cannot load them, see
[`help(ctrLoadQueryIntoDb)`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md).
The download or presence of results is not recorded in
[`dbQueryHistory()`](https://rfhb.github.io/ctrdata/reference/dbQueryHistory.md)
because the availability of results increases over time. The following
is fast since it re-uses previously downloads of trials, if run in the
same R session in which the above commands were run.

``` r

ctrLoadQueryIntoDb(
  querytoupdate = "last",
  euctrprotocolsall = FALSE,
  forcetoupdate = TRUE,
  euctrresults = TRUE,
  con = db
)
# * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results
# * Query last run: 2026-03-07
# * Checking trials in EUCTR, found 409 trials 
# - Downloading in 21 batch(es) (20 trials each; estimate: 30 s)...
# - Downloading 409 records of 409 trials (estimate: 20 s)...
# - Converting to NDJSON (estimate: 1 s)... 
# - Importing records into database...
# = Imported or updated 409 records on 409 trial(s)
# * Checking results if available from EUCTR for 409 trials: 
# - Downloading results...
# - Extracting results (. = data, F = file[s] and data, x = none): F F . . . . F
# . . F . . . F . . F F F . . . . F F F . . . F . . . . F F . F . . . . . F . . 
# . . . . . . . . . F . . . . . . F . . . . . . . . . F . F . . . . . . . . . . 
# . . . . . . . F F . F . . . F . . . F F . . . . . . . . . . . . F F . F . . . 
# . . . . . . . F F . F F . . . . F F . . . F . F . . . F . . . . . . . . . . . 
# F . F . . . . . F . F . . . F F F . F . . F . F . . F . . . . . F F . F . . . 
# . . . . . . . . . F . . . . . . . . . . . . . . . . . . F . . . F . . . . . . 
# . . F . F . . . . . . . F . . . . . . . . . . . . . . . F F . . . . . . F . . 
# . . F F F . . . . . F F . . F . . . . . . . . . . . . F . . . . . F . . . . F 
# . . . F F . . F . . . . . . . . . . . . . F . . . . . F . . . . . . . F . . . 
# F . . F . . . F F . . . . . . F . F . . . . . F . . F . . . . F . . . . F F . 
# . . . F . . . . . . . . 
# - Data found for 409 trials
# - Converting to NDJSON (estimate: 10 s)...
# - Importing 409 results into database (may take some time)...
# - Results history: not retrieved (euctrresultshistory = FALSE)
# = Imported or updated results for 409 trials
# Updated history ("meta-info" in "test")
# $n
# [1] 409
```

## Add trial information from several registers

The same collection can be used to store (and analyse) trial information
from different registers, thus can include different and complementary
sets of trials. The registers currently supported include CTIS, EUCTR,
CTGOV, CTGOV2 and ISRCTN. This can be achieved by loading queries that
the user defines specifically or that function
[`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
provides, as follows:

``` r

# Loading specific query into same collection
ctrLoadQueryIntoDb(
  queryterm = "cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com",
  register = "CTGOV2",
  con = db
)
#  Found search query from CTGOV2: cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# * Checking trials in CTGOV2, found 113 trials
# - Downloading in 1 batch(es) (max. 1000 trials each; estimate: 0.31 s)...
# - Load and convert batch 1...
# - Importing records into database...
# JSON file #: 1 / 1                               
# = Imported or updated 113 trial(s)
# Updated history ("meta-info" in "test")
# $n
# [1] 113

# Use same query details to obtain queries
queries <- ctrGenerateQueries(
  condition = "neuroblastoma",
  recruitment = "completed",
  phase = "phase 2",
  population = "P"
)

# Open queries in registers' web interfaces
sapply(queries, ctrOpenSearchPagesInBrowser)

# Load all queries into database collection
result <- lapply(queries, ctrLoadQueryIntoDb, con = db)

# Show results of loading
sapply(result, "[[", "n")
# EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS 
#   180            0          105          105            2 

# Overview of queries
dbQueryHistory(con = db)
#       query-timestamp query-register query-records
# 1 2026-03-07 16:51:19          EUCTR          1628
# 2 2026-03-07 17:04:08          EUCTR           409
# 3 2026-03-07 17:05:22          EUCTR             0
# 4 2026-03-07 17:08:23          EUCTR           409
# 5 2026-03-07 17:09:27         CTGOV2           113
# 6 2026-03-07 17:10:13          EUCTR           180
# 7 2026-03-07 17:10:14         CTGOV2           105
# 8 2026-03-07 17:10:15         CTGOV2           105
# 9 2026-03-07 17:10:16           CTIS             2
#
# query-term
# 1 query=cancer&age=under-18&resultsstatus=trials-with-results
# 2 query=cancer&age=under-18&resultsstatus=trials-with-results
# 3 query=cancer&age=under-18&resultsstatus=trials-with-results
# 4 query=cancer&age=under-18&resultsstatus=trials-with-results
# 5 cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# 6 query=neuroblastoma&phase=phase-two&age=children&age=adolescent&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18&status=completed
# 7 cond=neuroblastoma&intr=Drug OR Biological&term=AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)&aggFilters=phase:2,ages:child,status:com,studyType:int
# 8 term=AREA[ConditionSearch]"neuroblastoma" AND (AREA[Phase]"PHASE2") AND (AREA[StdAge]"CHILD") AND (AREA[OverallStatus]"COMPLETED") AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL))
# 9 searchCriteria={"medicalCondition":"neuroblastoma","trialPhaseCode":[4],"ageGroupCode":[2],"status":[5,8]}
```

## Add personal annotations

When loading trial information, the user can specify an annotation
string to each of the records that are loaded when calling
[`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md).
By default, new annotations are appended to any existing annotation of
the trial record; alternatively, annotations can be replaced.
Annotations are useful for analyses, for example to specially identify
subsets of records and trials of interest in the collection

``` r

# Annotate a query in CTGOV2 defined above
ctrLoadQueryIntoDb(
  queryterm = queries["CTGOV2"],
  annotation.text = "site_DE ",
  annotation.mode = "append",
  con = db
)
# * Found search query from CTGOV2: cond=neuroblastoma&intr=Drug OR Biological&term=AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)&aggFilters=phase:2,ages:child,status:com,studyType:int
# * Checking trials in CTGOV2, found 105 trials
# - Downloading in 1 batch(es) (max. 1000 trials each; estimate: 0.29 s)...
# - Load and convert batch 1...
# - Importing records into database...
# JSON file #: 1 / 1                               
# = Imported or updated 105 trial(s)
# = Annotated retrieved records (105 records)
# Updated history ("meta-info" in "test")
# $n
# [1] 105
```

## Find synonyms of active substance names

Not all registers automatically expand search terms to include
alternative terms, such as codes and other names of active substances.
The synonymous names can be used in queries in a register that does not
offer search expansion. To obtain a character vector of synonyms for an
active substance name:

``` r

# Search for synonyms
ctrFindActiveSubstanceSynonyms(
  activesubstance = "imatinib"
)
#  [1] "imatinib"          "Bosulif"           "Carcemia"          "CGP 57148"        
#  [5] "CGP 57148B"        "CGP57148"          "CGP57148B"         "Gleevac"          
#  [9] "Gleevec"           "Glevec"            "GLIVEC"            "Imarech"          
# [13] "Imat"              "Imatinib"          "Imatinib Mesylate" "Imkeldi"          
# [17] "Impentri"          "NSC #716051"       "NSC 716051"        "PegIntron"        
# [21] "QTI571"            "Sprycel"           "STI 571"           "STI571"           
# [25] "Tasigna"          
```

## Find platform trials

The interest is increasing to design and use integrated research
platforms, clinical research platforms, platform trials, multi-arm
multi-stage (MAMS) and master protocol research programs (MPRPs).
Additional concepts and terms used include basket and umbrella trials,
and in particular complex trials. Please see the references below for
further information. `ctrdata` can help finding such research and
analysing the study information, as follows:

``` r

# Generate queries to identify trials
queries <- ctrGenerateQueries(
  searchPhrase = paste0(
    "basket OR platform OR umbrella OR master protocol OR ",
    "multiarm OR multistage OR subprotocol OR substudy OR ",
    "multi-arm OR multi-stage OR sub-protocol OR sub-study"), 
  startAfter = "2015-01-01")

# See 
help("ctrGenerateQueries")

# Open queries in register web interface
sapply(queries, ctrOpenSearchPagesInBrowser)

# Count number of studies found in the register
result <- lapply(queries, ctrLoadQueryIntoDb, only.count = TRUE)

sapply(result, "[[", "n")
# EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS 
#  1635          236         2507         2507          302 

# Connect to a database and chose a collection (table)
db <- nodbi::src_sqlite(
  dbname = "database_name.sql",
  collection = "test"
)

# Load studies, include EUCTR results data for analysis
result <- lapply(
  queries, ctrLoadQueryIntoDb, con = db, 
  euctrprotocolsall = FALSE, euctrresults = TRUE)

sapply(result, "[[", "n")
# EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS 
#  1633          236         2507         2507          302  

# See next section for adding related trials
```

References:

- [EU-PEARL. 2023. ‘D2.1. Report on Terminology, References and
  Scenarios for Platform Trials and Master
  Protocols’](https://web.archive.org/web/20230314024441/https://eu-pearl.eu/wp-content/uploads/2020/06/EU-PEARL_D2.1_Report-on-Terminology-and-Scenarios-for-Platform-Trials-and-Masterprotocols.pdf)
- [EC / EMA / HMA. 2022. ‘Complex Clinical Trials – Questions and
  Answers’.
  EMA/298712/2022](https://health.ec.europa.eu/system/files/2022-06/medicinal_qa_complex_clinical-trials_en.pdf)
- [Williams RJ et al. 2022. ‘Approach for Reporting Master Protocol
  Study Designs on ClinicalTrials.Gov: Qualitative Analysis’. The BMJ
  377 (June):e067745](https://doi.org/10.1136/bmj-2021-067745)
- [Clinical Trials Facilitation and Coordination Group (CTFG). 2019.
  ‘Recommendation Paper on the Initiation and Conduct of Complex
  Clinical
  Trials’](https://www.hma.eu/fileadmin/dateien/Human_Medicines/01-About_HMA/Working_Groups/CTFG/2019_02_CTFG_Recommendation_paper_on_Complex_Clinical_Trials.pdf)

## Load information using trial identifiers

When identifiers of clinical trials of interest are already known, this
example shows how they can be processed to import the trial information
into a database collection. This involves constructing a query that
combines the identifiers and then iterating over the sets of
identifiers. Note to combine identifiers into the `queryterm` depends on
the specific register.

``` r

# Use a trial concept to calculate related identifiers
help("ctrdata-trial-concepts")

# Get data from trials loaded above
df <- dbGetFieldsIntoDf(
  fields = "ctrname",
  calculate = c(
    "f.isUniqueTrial",
    "f.likelyPlatformTrial",
    "f.trialTitle"
  ),
  con = db
)
# To review trial concepts details, call 'help("ctrdata-trial-concepts")'
# Querying database (25 fields)...
# Searching for duplicate trials...                         
# - Getting all trial identifiers (may take some time), 4678 found in collection
# - Finding duplicates among registers' and sponsor ids...
# - Unique are 0 / 2507 / 149 / 474 / 202 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
# = Returning keys (_id) of 3332 records in collection "test"
# Searching for duplicate trials... ..                            
# - Getting all trial identifiers, 4678 found in collection
# Calculating f.trialTitle... 

# Show names of calculated columns in the
# data frame with possible platform trials
names(df)
# [1] "_id"                 
# [2] "ctrname"             
# [3] ".isUniqueTrial"      
# [4] ".likelyPlatformTrial"
# [5] ".likelyRelatedTrials"
# [6] ".maybeRelatedTrials" 
# [7] ".trialTitle" 

# Reduce to unique trials
df <- df[df$.isUniqueTrial, ]
nrow(df)
# [1] 3332

# Number of recognised set of trials
length(unique(df$.maybeRelatedTrials))
# 224

# Trials with which _id are missing? 
missingIds <- unique(na.omit(setdiff(
  unlist(df$.maybeRelatedTrials), df$`_id`)))

# Load missing trials by _id
res <- list()
for (i in seq_along(missingIds)) {
  message(i, ": ", missingIds[i])
  res <- c(res, suppressMessages(
    list(ctrLoadQueryIntoDb(
      missingIds[i], euctrresults = TRUE, 
      euctrprotocolsall = FALSE, con = db)
    )))
}

# Trials that could not be loaded are likely phase 1 trials 
# which are not publicly accessible in the in EUCTR register
missingIds[which(sapply(res, "[[", "n") == 0L)]
```

The above loads one trial after the other, just using the `_id` of the
trial, from which `ctrdata` infers the concerned register.
Alternatively, batches of `_id`s can be loaded from some registers (not
CTIS), as follows.

``` r

# ids of trials of interest
ctIds <- c(
  "NCT00001209", "NCT00001436", "NCT00187109", "NCT01516567", "NCT01471782", 
  "NCT00357084", "NCT00357500", "NCT00365755", "NCT00407433", "NCT00410657", 
  "NCT00436852", "NCT00445965", "NCT00450307", "NCT00450827", "NCT00471679", 
  "NCT00492167", "NCT00499616", "NCT00503724")

# split into sets of each 10 trial ids 
# (larger sets e.g. 50 may still work)
idSets <- split(ctIds, ceiling(seq_along(ctIds) / 10))

# variable to collect import results
result <- NULL

# iterate over sets of trial ids
for (idSet in idSets) {
  
  setResult <- ctrLoadQueryIntoDb(
    queryterm = paste0("term=", paste0(idSet, collapse = " ")),
    register = "CTGOV2",
    con = db
  )
  
  # check that queried ids have
  # successfully been loaded
  stopifnot(identical(
    sort(setResult$success), sort(idSet)))
  
  # append result
  result <- c(result, list(setResult))
}

# inspect results 
as.data.frame(do.call(rbind, result))[, c("n", "failed")]
#    n failed
# 1 10   NULL
# 2  8   NULL

# queryterms for other registers for retrieving trials by their identifier: 
#
# CTIS (note the comma separated values): 
# https://euclinicaltrials.eu/ctis-public/search#searchCriteria=
# {"containAny":"2025-521008-22-00, 2024-519446-67-00, 2024-517647-31-00"}
#
# EUCTR (note the country suffix os to be removed, values separated with OR): 
# https://www.clinicaltrialsregister.eu/ctr-search/search?
# query=2008-001606-16+OR+2008-001721-34+OR+2008-002260-33
```
