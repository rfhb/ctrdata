
<!-- README.md is generated from README.Rmd -->
<!-- badges: start -->

[![CRAN](https://badges.cranchecks.info/worst/ctrdata.svg)](https://cran.r-project.org/package=ctrdata)
[![ctrdata status
badge](https://rfhb.r-universe.dev/badges/ctrdata)](https://rfhb.r-universe.dev)
[![codecov](https://codecov.io/gh/rfhb/ctrdata/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rfhb/ctrdata)
[![R-CMD-CHECK-ubuntu-postgresql-duckdb-mongodb-sqlite](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-linux.yaml/badge.svg)](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-linux.yaml)
[![R-CMD-CHECK-win-macos-duckdb-mongodb-sqlite](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-win-macos.yaml/badge.svg)](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-win-macos.yaml)
<!-- badges: end -->

# ctrdata for aggregating and analysing clinical trials

The package `ctrdata` provides functions for retrieving (downloading)
information on clinical trials from public registers, and for
aggregating and analysing this information; it can be used for the

- EU Clinical Trials Register (“EUCTR”,
  <https://www.clinicaltrialsregister.eu/>)
- ClinicalTrials.gov (“CTGOV” classic,
  <https://classic.clinicaltrials.gov/>)
- ISRCTN (<https://www.isrctn.com/>)
- EU Clinical Trials Information System (“CTIS”,
  <https://euclinicaltrials.eu/>) 🔔 see
  [example](#workflow-ctis-example)

The motivation is to investigate and understand trends in design and
conduct of trials, their availability for patients and to facilitate
using their detailed results for research and meta-analyses. `ctrdata`
is a package for the [R](https://www.r-project.org/) system, but other
systems and tools can be used with the databases created with the
package. This README was reviewed on 2023-07-14 for version 1.13.3.9000
(after change of CTGOV website).

## Main features

- Protocol- and results-related trial information is easily downloaded:
  Users define a query in a register’s web interface and then enter the
  URL into `ctrdata` which retrieves in one go all trials found.
  Documents in registers on trials can also be downloaded. Personal
  annotations can be made to trials when downloading a query. Synonyms
  of an active substance can also be found.
- Downloaded trial information is transformed and stored in a collection
  of a document-centric database, for fast and offline access.
  Information from different registers can be accumalated in a single
  collection. Uses `DuckDB`, `PostgreSQL`, `RSQLite` or `MongoDB`, via R
  package `nodbi`: see section
  [Databases](#databases-that-can-be-used-with-ctrdata) below. Re-run
  any previous query in a collection to retrieve and update trial
  records.
- For analyses, convenience functions in `ctrdata` allow to identify
  unique (de-duplicated) trial records across registers, to merge and
  recode fields as well as to easily access deeply-nested fields.
  Analysis can be done with `R` or other systems, by accessing the
  structured information in the database.

URLs of queries in the registers can be automatically copied to the
clipboard (including for “CTIS”, where the URL does not show the query),
see
[here](#3-script-to-automatically-copy-users-query-from-web-browser).

Remember to respect the registers’ terms and conditions (see
`ctrOpenSearchPagesInBrowser(copyright = TRUE)`). Please cite this
package in any publication as follows: “Ralf Herold (2023). ctrdata:
Retrieve and Analyze Clinical Trials in Public Registers. R package
version 1.13.3, <https://cran.r-project.org/package=ctrdata>”.

<!--
&#10;```r
citation("ctrdata")
```
-->

## References

Package `ctrdata` has been used for:

- Lasch et al. (2022) The Impact of COVID‐19 on the Initiation of
  Clinical Trials in Europe and the United States. Clinical Pharmacology
  & Therapeutics, <https://doi.org/10.1002/cpt.2534>
- Blogging on [Innovation coming to paediatric
  research](https://paediatricdata.eu/innovation-coming-to-paediatric-research/)
- Cancer Research UK (2017) [The impact of collaboration: The value of
  UK medical research to EU science and
  health](https://www.cancerresearchuk.org/about-us/we-develop-policy/we-work-with-government/exiting-the-eu/uk-and-eu-research#downloads)

## Installation

### 1. Install package `ctrdata` in R

Package `ctrdata` is [on
CRAN](https://cran.r-project.org/package=ctrdata) and [on
GitHub](https://github.com/rfhb/ctrdata). Within
[R](https://www.r-project.org/), use the following commands to install
package `ctrdata`:

``` r
# Install CRAN version:
install.packages("ctrdata")

# Alternatively, install development version:
install.packages("devtools")
devtools::install_github("rfhb/ctrdata", build_vignettes = TRUE)
```

These commands also install the package’s dependencies (`nodbi`,
`jsonlite`, `httr`, `curl`, `clipr`, `xml2`, `rvest`, `lubridate` and
`stringi`).

### 2. Command line tools `perl`, `sed` and `php` (5.2 or higher)

These are required for `ctrLoadQueryIntoDb()`, the main function of
package `ctrdata` (see [Example workflow](#example-workflow)), to work
with the registers EUCTR, CTGOV, ISRCTN (but are not required to work
with CTIS); the function also checks if the tools can be used.

- For MS Windows, install [`Cygwin`](https://cygwin.org/install.html):
  In `R`, run `ctrdata::installCygwinWindowsDoInstall()` for an
  automated minimal installation. Alternatively, manually install Cygwin
  with packages `perl`, `php-jsonc` and `php-simplexml` into
  `c:\cygwin`. The installation needs about 160 MB disk space.

- In macOS including 11 Big Sur, these are already installed; as
  alternative and 🔔for macOS 12 Monterey and above,
  [`homebrew`](https://brew.sh/) can be used: `brew install php`, which
  seems to include all `php` libraries required for `ctrdata`.

- In Linux, these are usually already installed; tools to install vary
  by distribution (e.g., `sudo apt install php-cli php-xml php-json`).

### 3. Script to automatically copy user’s query from web browser

This is optional; it works with all registers supported by `ctrdata` but
is recommended for CTIS because the URL in the web browser does not
reflect the parameters the user specified for querying this register.

In the web browser, install the [Tampermonkey browser
extension](https://www.tampermonkey.net/), click on “New user script”
and then on “Tools”, then enter into “Import from URL” this URL:
[`https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js`](https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js)
and last click on “Install”.

The browser extension can be disabled and enabled by the user. When
enabled, the URLs to all user’s queries in the registers are
automatically copied to the clipboard and can be pasted into the
`queryterm=...` parameter of function `ctrLoadQueryIntoDb()`.

## Overview of functions in `ctrdata`

The functions are listed in the approximate order of use in a user’s
workflow (in bold, main functions). See also the [package documentation
overview](https://rfhb.github.io/ctrdata/reference/index.html).

| Function name                      | Function purpose                                                                                                               |
|------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| `ctrOpenSearchPagesInBrowser()`    | Open search pages of registers or execute search in web browser                                                                |
| `ctrFindActiveSubstanceSynonyms()` | Find synonyms and alternative names for an active substance                                                                    |
| `ctrGetQueryUrl()`                 | Import from clipboard the URL of a search in one of the registers                                                              |
| `ctrLoadQueryIntoDb()`             | **Retrieve (download) or update, and annotate, information on trials from a register and store in a collection in a database** |
| `dbQueryHistory()`                 | Show the history of queries that were downloaded into the collection                                                           |
| `dbFindIdsUniqueTrials()`          | **Get the identifiers of de-duplicated trials in the collection**                                                              |
| `dbFindFields()`                   | Find names of variables (fields) in the collection                                                                             |
| `dbGetFieldsIntoDf()`              | **Create a data frame (or tibble) from trial records in the database with the specified fields**                               |
| `dfTrials2Long()`                  | Transform the data.frame from `dbGetFieldsIntoDf()` into a long name-value data.frame, including deeply nested fields          |
| `dfName2Value()`                   | From a long name-value data.frame, extract values for variables (fields) of interest (e.g., endpoints)                         |
| `dfMergeVariablesRelevel()`        | Merge variables into a new variable, optionally map values to a new set of levels                                              |
| `installCygwinWindowsDoInstall()`  | Convenience function to install a Cygwin environment (MS Windows only)                                                         |

## Databases that can be used with `ctrdata`

Package `ctrdata` retrieves trial information and stores it in a
database collection, which has to be given as a connection object to
parameter `con` for several `ctrdata` functions; this connection object
is created in slightly different ways for the three supported database
backends that can be used with `ctrdata` as shown in the table. For a
speed comparison, see the [nodbi
documentation](https://github.com/ropensci/nodbi#benchmark).

Besides ctrdata functions below, any such a connection object can
equally be used with functions of other packages, such as `nodbi` (last
row in table) or, in case of MongoDB as database backend, `mongolite`
(see vignettes).

| Purpose                                   | Function call                                                                                                           |
|-------------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| Create **SQLite** database connection     | `dbc <- nodbi::src_sqlite(dbname = "name_of_my_database", collection = "name_of_my_collection")`                        |
| Create **MongoDB** database connection    | `dbc <- nodbi::src_mongo(db = "name_of_my_database", collection = "name_of_my_collection")`                             |
| Create **PostgreSQL** database connection | `dbc <- nodbi::src_postgres(dbname = "name_of_my_database"); dbc[["collection"]] <- "name_of_my_collection"`            |
| Create **DuckDB** database connection     | `dbc <- nodbi::src_duckdb(dbdir = "name_of_my_database", collection = "name_of_my_collection")`                         |
| Use connection with `ctrdata` functions   | `ctrdata::{ctrLoadQueryIntoDb, dbQueryHistory, dbFindIdsUniqueTrials, dbFindFields, dbGetFieldsIntoDf}(con = dbc, ...)` |
| Use connection with `nodbi` functions     | e.g., `nodbi::docdb_query(src = dbc, key = dbc$collection, ...)`                                                        |

## Vignettes

- [Install R package
  ctrdata](https://rfhb.github.io/ctrdata/articles/ctrdata_install.html)
- [Retrieve clinical trial
  information](https://rfhb.github.io/ctrdata/articles/ctrdata_retrieve.html)
- [Summarise and analyse clinical trial
  information](https://rfhb.github.io/ctrdata/articles/ctrdata_summarise.html)

## Example workflow

The aim is to download protocol-related trial information and tabulate
the trials’ status of conduct.

- Attach package `ctrdata`:

``` r
library(ctrdata)
```

- See help to get started with `ctrdata`:

``` r
help("ctrdata-package")
```

- Information on trial registers that can be used with `ctrdata`:

``` r
help("ctrdata-registers")
```

- Open registers’ advanced search pages in browser:

``` r
ctrOpenSearchPagesInBrowser()

# Please review and respect register copyrights:
ctrOpenSearchPagesInBrowser(copyright = TRUE)
```

- Adjust search parameters and execute search in browser

- When trials of interest are listed in browser, *copy the address from
  the browser’s address bar to the clipboard*

- Search used in this example:
  <https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&age=under-18&phase=phase-one&status=completed>

- Get address from clipboard:

``` r
q <- ctrGetQueryUrl()
# * Using clipboard content as register query URL: https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&age=under-18&phase=phase-one&status=completed
# * Found search query from EUCTR: query=cancer&age=under-18&phase=phase-one&status=completed

q
#                                                   query-term  query-register
# 1 query=cancer&age=under-18&phase=phase-one&status=completed           EUCTR
```

🔔 Queries in the trial registers can automatically copied to the
clipboard (including for “CTIS”, where the URL does not show the query)
using our solution
[here](#3-script-to-automatically-copy-users-query-from-web-browser).

- Retrieve protocol-related information, transform and save to database:

The database collection is specified first, using `nodbi` (see above for
how to specify `PostgreSQL`, `RSQlite`, `DuckDB` or `MongoDB` as
backend, see section
[Databases](#databases-that-can-be-used-with-ctrdata)); then, trial
information is retrieved and loaded into the collection:

``` r
# Connect to (or newly create) an SQLite database
# that is stored in a file on the local system:
db <- nodbi::src_sqlite(
  dbname = "some_database_name.sqlite_file",
  collection = "some_collection_name"
)

# Retrieve trials from public register:
ctrLoadQueryIntoDb(
  queryterm = q,
  con = db
)
# * Found search query from EUCTR: query=cancer&age=under-18&phase=phase-one&status=completed
# Checking helper binaries: . . done
# (1/3) Checking trials in EUCTR:
# Retrieved overview, multiple records of 88 trial(s) from 5 page(s) to be downloaded (estimate: 4.4 MB)
# Downloading trials...
# Note: register server cannot compress data, transfer takes longer, about 0.3s per trial
# Download status: 5 done; 0 in progress. Total size: 6.99 Mb (100%)... done!             
# (2/3) Converting to JSON, 339 records converted
# (3/3) Importing JSON records into database...
# = Imported or updated 339 records on 88 trial(s) 
# No history found in expected format.
# Updated history ("meta-info" in "some_collection_name")
```

Under the hood, scripts `euctr2json.sh` and `xml2json.php` (in
`ctrdata/exec`) transform EUCTR plain text files and CTGOV as well as
ISRCTN `XML` files to `ndjson` format, which is imported into the
database collection.

- Analyse

Tabulate the status of trials that are part of an agreed paediatric
development program (paediatric investigation plan, PIP). `ctrdata`
functions return a data.frame (or a tibble, if package `tibble` is
loaded).

``` r
# Get all records that have values in the fields of interest:
result <- dbGetFieldsIntoDf(
  fields = c(
    "a7_trial_is_part_of_a_paediatric_investigation_plan",
    "p_end_of_trial_status",
    "a2_eudract_number"
  ),
  con = db
)

# Find unique (deduplicated) trial identifiers for trials that have more than
# one record, for example for several EU Member States or in several registers:
uniqueids <- dbFindIdsUniqueTrials(con = db)
# Searching for duplicate trials... 
#  - Getting all trial identifiers (may take some time), 347 found in collection
#  - Finding duplicates among registers' and sponsor ids...
#  - 256 EUCTR _id were not preferred EU Member State record for 91 trials
#  - Keeping 91 records from EUCTR
# = Returning keys (_id) of 91 records in collection "some_collection_name"

# Keep only unique / de-duplicated records:
result <- subset(
  result,
  subset = `_id` %in% uniqueids
)

# Tabulate the selected clinical trial information:
with(
  result,
  table(
    p_end_of_trial_status,
    a7_trial_is_part_of_a_paediatric_investigation_plan
  )
)
#                           a7_trial_is_part_of_a_paediatric_investigation_plan
# p_end_of_trial_status      FALSE TRUE
#   Completed                   46   20
#   GB - no longer in EU/EEA     1    1
#   Ongoing                      3    1
#   Prematurely Ended            2    2
#   Restarted                    0    1
#   Temporarily Halted           1    1
```

- Add records from another register (CTGOV) into the same collection

- Search used in this example:
  <https://classic.clinicaltrials.gov/ct2/results?cond=neuroblastoma&rslt=With&recrs=e&age=0&intr=Drug>

``` r
# Retrieve trials from another register:
ctrLoadQueryIntoDb(
  queryterm = "cond=neuroblastoma&rslt=With&recrs=e&age=0&intr=Drug",
  register = "CTGOV",
  con = db
)
# 
```

- Add records from a third register (ISRCTN) into the same collection

- Search used in this example:
  <https://www.isrctn.com/search?q=neuroblastoma>

``` r
# Retrieve trials from another register:
ctrLoadQueryIntoDb(
  queryterm = "https://www.isrctn.com/search?q=neuroblastoma",
  con = db
)
# * Found search query from CTGOV: cond=neuroblastoma&rslt=With&recrs=e&age=0&intr=Drug
# Checking helper binaries: done
# (1/3) Checking trials in CTGOV:
# Retrieved overview, records of 55 trial(s) are to be downloaded (estimate: 0.44 MB)
# Download status: 1 done; 0 in progress. Total size: 754.96 Kb (100%)... done!             
# (2/3) Converting to JSON, 55 records converted
# (3/3) Importing JSON records into database...
# = Imported or updated 55 trial(s)                
# Updated history ("meta-info" in "some_collection_name")
```

<div id="workflow-ctis-example">

</div>

- Add records from a fourth register (CTIS 🔔) into the same collection

Queries in the CTIS search interface can be automatically copied to the
clipboard so that a user can paste them into `queryterm`, see
[here](#3-script-to-automatically-copy-users-query-from-web-browser). As
of June 2023, more than 200 trials are publicly accessible in CTIS. See
[below](documents-example) for how to download documents from CTIS.

``` r
# Retrieve trials from another register:
ctrLoadQueryIntoDb(
  queryterm = "https://euclinicaltrials.eu/app/#/search?ageGroupCode=2",
  con = db
)
# * Found search query from CTIS: ageGroupCode=2
# (1/5) Downloading trials list, found 25 trials
# (2/5) Downloading and processing part I and parts II... (estimate: 3.75 Mb)
# Download status: 25 done; 0 in progress. Total size: 3.84 Mb (100%)... done!             
# . . . . . . . . . . . . . . . . . . . . . . . . . 
# (3/5) Downloading and processing additional data: 
# publicevents, summary, layperson, csr, cm, inspections, publicevaluation
# Download status: 37 done; 0 in progress. Total size: 1.53 Mb (100%)... done!             
# 25
# (4/5) Importing JSON records into database...
# (5/5) Updating with additional data: . .         
# = Imported / updated 25 / 25 / 25 records on 25 trial(s)
# Updated history ("meta-info" in "some_collection_name")

allFields <- dbFindFields(".*", db)
# Finding fields in database collection (may take some time)
# Field names cached for this session.
length(allFields[grepl("CTIS", names(allFields))])
# [1] 2254

allFields[grepl("defer|consideration$", allFields, ignore.case = TRUE)]
#                                                                                            CTIS 
#                                                                           "hasDeferrallApplied" 
#                                                                                            CTIS 
# "publicEvaluation.partIIEvaluationList.partIIRfiConsiderations.rfiConsiderations.consideration" 
#                                                                                            CTIS 
#                       "publicEvaluation.partIRfiConsiderations.rfiConsiderations.consideration" 
#                                                                                            CTIS 
#                  "publicEvaluation.partIRfiConsiderations.rfiConsiderations.part1Consideration" 
#                                                                                            CTIS 
#                  "publicEvaluation.validationRfiConsiderations.rfiConsiderations.consideration" 
#                                                                                            CTIS 
#             "publicEvaluation.validationRfiConsiderations.rfiConsiderations.part1Consideration" 

dbGetFieldsIntoDf("publicEvaluation.partIRfiConsiderations.rfiConsiderations.consideration", db)[1,2]
# publicEvaluation.partIRfiConsiderations.rfiConsiderations.consideration
# In(EX)clusion criteria: An adequate definition of WOCBP or postmenopausal woman 
# is missing and should be added to the protocol. / The rationale for the treatment duration of 7 to 
# 18 weeks cannot be followed. No data are available for this short time period and nivolumab treatment. 
# The shortest duration tested so far in 1 year in adjuvant or maintenance treatment protocol. 
# The sponsor is asked to justify and substantiate his assumption that this treatment duration is 
# adequate with respective data. / E: Information regarding the special clinical conditions for 
# conducting clinical trials with minors, \nsee Article 32 Par. 1 lit e) to g) of Regulation (EU) 
# 536/2014 is missing. Please revise the protocol accordingly. So it is indicated to include the 
# patients older than 18 years first and in case of positive results the planned younger patients 
# could follow.\nStatistical Comment: The statistical analyses are missing in the trial protocol. 
# Biometric adequate is a restriction to descriptive evaluations. A sequential evaluation is 
# recommended (adults first and then children). The protocol has to be amended accordingly / 
# Discontinuation criteria for study subjects and clinical trial termination criteria are missing 
# and have to be added. Please amend. [...]

# use an alternative to dbGetFieldsIntoDf()
allData <- nodbi::docdb_query(src = db, key = db$collection, query = '{"ctrname":"CTIS"}')
# names of top-level data items
sort(names(allData))
#  [1] "_id"                           "ageGroup"                     
#  [3] "applications"                  "authorizationDate"            
#  [5] "authorizedPartI"               "authorizedPartsII"            
#  [7] "cm"                            "coSponsors"                   
#  [9] "ctNumber"                      "ctrname"                      
# [11] "ctStatus"                      "decisionDate"                 
# [13] "eeaEndDate"                    "eeaStartDate"                 
# [15] "endDateEU"                     "eudraCtInfo"                  
# [17] "gender"                        "hasAmendmentApplied"          
# [19] "hasDeferrallApplied"           "id"                           
# [21] "initialApplicationId"          "isRmsTacitAssignment"         
# [23] "lastUpdated"                   "memberStatesConcerned"        
# [25] "mscTrialNotificationsInfoList" "primarySponsor"               
# [27] "publicEvaluation"              "record_last_import"           
# [29] "recruitmentStatus"             "sponsorType"                  
# [31] "startDateEU"                   "submissionDate"               
# [33] "therapeuticAreas"              "title"                        
# [35] "totalNumberEnrolled"           "totalPartIISubjectCount"      
# [37] "trialCountries"                "trialEndDate"                 
# [39] "trialGlobalEnd"                "trialPhase"                   
# [41] "trialStartDate" 
# 
format(object.size(allData), "MB")
# [1] "20.1 Mb"
```

- Analyse across trials

Show umulative start of trials over time.

``` r
# get names of all fields / variables in the collaction
length(dbFindFields(".*", con = db))
# Finding fields in database collection (may take some time)
# Field names cached for this session.
# [1] 2946

dbFindFields("startdate", con = db)
# Using cache of fields.

# get trial data
result <- dbGetFieldsIntoDf(
  fields = c(
    # CTGOV
    "start_date",
    "overall_status",
    # EUCTR
    "trialInformation.recruitmentStartDate",
    "x5_trial_status", 
    # ISRCTN
    "trialDesign.overallStartDate",
    # CTIS
    "authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate",
    "ctStatus"
  ),
  con = db
)

# Deduplicate trials and obtain unique identifiers 
# for trials that have records in several registers:
uniqueids <- dbFindIdsUniqueTrials(con = db)
result <- subset(result, `_id` %in% uniqueids)

# Merge variables / fields from different registers
result$start <- dfMergeTwoVariablesRelevel(
  df = result, 
  colnames = 'contains("date")'
)
# dfMergeTwoVariablesRelevel() is deprecated, use dfMergeVariablesRelevel()

# Merge variables / fields from different registers with releveling
statusvalues <- list(
  "ongoing" = c(
    # EUCTR
    "Recruiting", "Active", "Ongoing", "Temporarily Halted", "Restarted",
    # CTGOV
    "Active, not recruiting", "Enrolling by invitation", "Not yet recruiting",
    # CTIS
    "Ongoing, recruiting", "Ongoing, recruitment ended", 
    "Ongoing, not yet recruiting", "Authorised, not started"
  ),
  "completed" = c("Completed", "Prematurely Ended", "Terminated"),
  "other" = c("GB - no longer in EU/EEA", "Trial now transitioned",
              "Withdrawn", "Suspended", "No longer available")
)
result$state <- dfMergeVariablesRelevel(
  df = result, 
  colnames = 'contains("status")',
  levelslist = statusvalues
)

# example plot
library(ggplot2)
ggplot(result) + 
  stat_ecdf(aes(x = start, colour = state))
ggsave(
  filename = "man/figures/README-ctrdata_across_registers.png",
  width = 5, height = 3, units = "in"
)
```

<figure>
<img
src="https://raw.githubusercontent.com/rfhb/ctrdata/master/docs/reference/figures/README-ctrdata_across%20registers.png"
alt="Analysis across registers" />
<figcaption aria-hidden="true">Analysis across registers</figcaption>
</figure>

- Result-related trial information

Analyse some simple result details (see this
[vignette](https://rfhb.github.io/ctrdata/articles/ctrdata_summarise.html)
for more examples):

``` r
# Get all records that have values in any of the specified fields
result <- dbGetFieldsIntoDf(
  fields = c(
    "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
    "clinical_results.baseline.group_list.group",
    "clinical_results.baseline.analyzed_list.analyzed.units",
    "study_design_info.allocation",
    "location"
  ),
  con = db
)

# Transform all fields into long name - value format
result <- dfTrials2Long(df = result)
# Total 6386 rows, 12 unique names of variables

# [1.] get counts of subjects for all arms into data frame
# This count is in the group where either its title or its
# description starts with "Total"
nsubj <- dfName2Value(
  df = result,
  valuename = "clinical_results.baseline.analyzed_list.analyzed.count_list.count.value",
  wherename = paste0(
    "clinical_results.baseline.group_list.group.title|",
    "clinical_results.baseline.group_list.group.description"
  ),
  wherevalue = "^Total$"
)

# [2.] count number of sites
nsite <- dfName2Value(
  df = result,
  # some ctgov records use location.name, others location.facility.name
  valuename = "^location.*name$"
)
# count
nsite <- tapply(
  X = nsite[["value"]],
  INDEX = nsite[["_id"]],
  FUN = length,
  simplify = TRUE
)
nsite <- data.frame(
  "_id" = names(nsite),
  nsite,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  row.names = NULL
)

# [3.] randomised?
ncon <- dfName2Value(
  df = result,
  valuename = "study_design_info.allocation"
)

# merge sets
nset <- merge(nsubj, nsite, by = "_id")
nset <- merge(nset, ncon, by = "_id")

# example plot
library(ggplot2)
ggplot(data = nset) +
  labs(
    title = "Neuroblastoma trials with results",
    subtitle = "clinicaltrials.gov"
  ) +
  geom_point(
    mapping = aes(
      x = nsite,
      y = value.x,
      colour = value.y == "Randomized"
    )
  ) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Number of sites") +
  ylab("Total number of subjects") +
  labs(colour = "Randomised?")
ggsave(
  filename = "man/figures/README-ctrdata_results_neuroblastoma.png",
  width = 5, height = 3, units = "in"
)
```

<figure>
<img
src="https://raw.githubusercontent.com/rfhb/ctrdata/master/docs/reference/figures/README-ctrdata_results_neuroblastoma.png"
alt="Neuroblastoma trials" />
<figcaption aria-hidden="true">Neuroblastoma trials</figcaption>
</figure>

<div id="documents-example">

</div>

- Download documents: retrieve protocols, statistical analysis plans and
  other documents into the local folder `./files-.../`

``` r
# EUCTR document files can be downloaded when results are requested
ctrLoadQueryIntoDb(
  queryterm = "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&age=under-18&phase=phase-one",
  euctrresults = TRUE,
  documents.path = "./files-euctr/",
  con = db
)
# * Found search query from EUCTR: query=cancer&age=under-18&phase=phase-one
# [...]
# Created directory ./files-euctr/
# Downloading trials...
# [...]
# = Imported or updated results for 102 trials
# = documents saved in './files-euctr'

# CTGOV files
ctrLoadQueryIntoDb(
  queryterm = "cond=Neuroblastoma&type=Intr&recrs=e&phase=1&u_prot=Y&u_sap=Y&u_icf=Y",
  register = "CTGOV",
  documents.path = "./files-ctgov/",
  con = db
)
# * Found search query from CTGOV: cond=Neuroblastoma&type=Intr&recrs=e&phase=1&u_prot=Y&u_sap=Y&u_icf=Y
# [...]
# Downloading documents into 'documents.path' = ./files-ctgov/
# - Created directory ./files-ctgov
# Applying 'documents.regexp' to 14 documents
# Downloading 7 documents:
# Download status: 7 done; 0 in progress. Total size: 30.88 Mb (100%)... done!             
# Newly saved 7 document(s) for 7 trial(s); 0 document(s) for 0 trial(s) already existed in ./files-ctgov

# CTIS files
ctrLoadQueryIntoDb(
  queryterm = "https://euclinicaltrials.eu/app/#/search?ageGroupCode=2",
  documents.path = "./files-ctis/",
  con = db
)
# * Found search query from CTIS: ageGroupCode=2
# [...]
# * Downloading documents into 'documents.path' = ./files-ctis/
# - Created directory ./files-ctis
# - Getting ids of lists with document information
# - Downloading 510 lists with document information (estimate: 10.2 Mb)
# Download status: 510 done; 0 in progress. Total size: 6.96 Mb (100%)... done!             
# - Processing document information in 510 lists
# - Creating subfolder for each trial
# - Applying 'documents.regexp' to 2909 documents
# - Downloading 258 missing documents
# Download status: 258 done; 0 in progress. Total size: 203.70 Mb (100%)... done!             
# = Newly saved 235 document(s) for 25 trial(s) (latest versions only, deduplicated 
# if e.g. in application and authorised part); 0 document(s) for 0 trial(s) already existed in ./files-ctis
```

## Additional features under consideration

- Retrieve previous versions of protocol- or results-related
  information. The challenge is that, apparently, initial versions
  cannot be queried and historical versions can only be retrieved
  one-by-one and not in structured format.

- Merge results-related fields retrieved from different registers (e.g.,
  corresponding endpoints). The challenge is the incomplete congruency
  and different structure of fields.

## Acknowledgements

- Data providers and curators of the clinical trial registers. Please
  review and respect their copyrights and terms and conditions, see
  `ctrOpenSearchPagesInBrowser(copyright = TRUE)`.

- Package `ctrdata` has been made possible building on the work done for
  [R](https://www.r-project.org/),
  [curl](https://cran.r-project.org/package=curl),
  [httr](https://cran.r-project.org/package=httr),
  [jqr](https://cran.r-project.org/package=jqr),
  [xml2](https://cran.r-project.org/package=xml2),
  [rvest](https://cran.r-project.org/package=rvest),
  [dplyr](https://cran.r-project.org/package=dplyr),
  [stringi](https://cran.r-project.org/package=stringi),
  [lubridate](https://cran.r-project.org/package=lubridate),
  [mongolite](https://cran.r-project.org/package=mongolite),
  [jsonlite](https://cran.r-project.org/package=jsonlite),
  [nodbi](https://cran.r-project.org/package=nodbi),
  [RPostgres](https://cran.r-project.org/package=RPostgres),
  [RSQLite](https://CRAN.R-project.org/package=RSQLite) and
  [clipr](https://cran.r-project.org/package=clipr).

### Issues and notes

- Please file issues and bugs
  [here](https://github.com/rfhb/ctrdata/issues). Also check out how to
  handle some of the closed issues, e.g. on [C stack usage too close to
  the limit](https://github.com/rfhb/ctrdata/issues/22) and on a [SSL
  certificate problem: unable to get local issuer
  certificate](https://github.com/rfhb/ctrdata/issues/19#issuecomment-820127139)

- Information in trial registers may not be fully correct; see for
  example [this publication on
  CTGOV](https://doi.org/10.1136/bmj.k1452).

- No attempts were made to harmonise field names between registers
  (nevertheless, `dfMergeVariablesRelevel()` can be used to merge and
  map several variables / fields into one).

## Trial records’ JSON in databases

### PostgreSQL

<figure>
<img
src="https://raw.githubusercontent.com/rfhb/ctrdata/master/docs/reference/figures/README-ctrdata_json_postgresql.jpg"
alt="Example JSON representation in PostgreSQL" />
<figcaption aria-hidden="true">Example JSON representation in
PostgreSQL</figcaption>
</figure>

### MongoDB

<figure>
<img
src="https://raw.githubusercontent.com/rfhb/ctrdata/master/docs/reference/figures/README-ctrdata_json_mongodb.jpg"
alt="Example JSON representation in MongoDB" />
<figcaption aria-hidden="true">Example JSON representation in
MongoDB</figcaption>
</figure>

### SQLite

<figure>
<img
src="https://raw.githubusercontent.com/rfhb/ctrdata/master/docs/reference/figures/README-ctrdata_json_sqlite.jpg"
alt="Example JSON representation in SQLite" />
<figcaption aria-hidden="true">Example JSON representation in
SQLite</figcaption>
</figure>
