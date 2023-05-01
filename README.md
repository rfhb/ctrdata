
<!-- README.md is generated from README.Rmd -->
<!-- badges: start -->

[![CRAN](https://badges.cranchecks.info/worst/ctrdata.svg)](https://cran.r-project.org/package=ctrdata)
[![ctrdata status
badge](https://rfhb.r-universe.dev/badges/ctrdata)](https://rfhb.r-universe.dev)
[![codecov](https://codecov.io/gh/rfhb/ctrdata/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rfhb/ctrdata)
[![R-CMD-CHECK-ubuntu-duckdb-mongodb-sqlite](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-linux.yaml/badge.svg)](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-linux.yaml)
[![R-CMD-CHECK-win-macos-duckdb-mongodb-sqlite](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-win-macos.yaml/badge.svg)](https://github.com/rfhb/ctrdata/actions/workflows/check-standard-win-macos.yaml)
<!-- badges: end -->

# ctrdata for aggregating and analysing clinical trials

The package `ctrdata` provides functions for retrieving (downloading)
information on clinical trials from public registers, and for
aggregating and analysing this information; it can be used for the

- EU Clinical Trials Register (“EUCTR”,
  <https://www.clinicaltrialsregister.eu/>)
- ClinicalTrials.gov (“CTGOV”, <https://clinicaltrials.gov/>)
- ISRCTN (<https://www.isrctn.com/>)
- EU Clinical Trials Information System (“CTIS”,
  <https://euclinicaltrials.eu/>) 🔔 NEW since 2023-03-25 (see
  [example](#workflow-ctis-example)) in workflow below

The motivation is to understand trends in design and conduct of trials,
their availability for patients and their detailled results. `ctrdata`
is a package for the [R](https://www.r-project.org/) system, but other
systems and tools can be used with the databases created with the
package. This README was reviewed on 2023-05-01 for version 1.13.0.9000.

## Main features

- Protocol- and results-related trial information is easily retrieved
  (downloaded): Users define a query in a register’s web interface and
  then use `ctrdata` to retrieve in one go all trials found and to
  accumulate information from different registers. Personal annotations
  can be including during retrieval. Synonyms of an active substance can
  also be found.
- Retrieved (downloaded) trial information is transformed and stored in
  a single collection of a document-centric database, for fast and
  offline access. Uses `DuckDB`, `PostgreSQL`, `RSQLite` or `MongoDB`,
  via R package `nodbi`: see section
  [Databases](#databases-that-can-be-used-with-ctrdata) below. Easily
  re-run any previous query in a collection to retrieve and update trial
  records.
- Analyses can be done with `R` (using convenience functions in
  `ctrdata` to identify unique (de-duplicated) trial records across
  registers, to merge and recode fields as well as to enumerate and
  provide easy access to deeply-nested fields) and with many other
  systems.
- 🔔Queries in the registers can be automatically copied to the
  clipboard (including for “CTIS”, where the URL does not show the
  query), see
  [here](#3-script-to-automatically-copy-users-query-from-web-browser)
  NEW since 2023-04-15

Remember to respect the registers’ terms and conditions (see
`ctrOpenSearchPagesInBrowser(copyright = TRUE)`). Please cite this
package in any publication as follows: “Ralf Herold (2023). ctrdata:
Retrieve and Analyze Clinical Trials in Public Registers. R package
version 1.13.0, <https://cran.r-project.org/package=ctrdata>”.

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
package `ctrdata` (see [Example workflow](#example-workflow)), to with
the registers EUCTR, CTGOV, ISRCTN (but not CTIS); the function also
checks if the tools can be used.

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
reflect the parameters the user specified for the querying this
register.

In the web browser, install the [Tampermonkey browser
extension](https://www.tampermonkey.net/), click on “New user script”
and then on “Tools”, then enter into “Import from URL” this URL:
[`https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js`](https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js)
and last click on “Install”.

The browser extension can be disabled and enabled by the user. When
enabled, the URLs to all user’s queries in the registers are now
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
| `dfMergeTwoVariablesRelevel()`     | Merge two simple variables into a new variable, optionally map values to a new set of values                                   |
| `installCygwinWindowsDoInstall()`  | Convenience function to install a Cygwin environment (MS Windows only)                                                         |

If package `dplyr` is loaded, a tibble is returned instead of a
data.frame.

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
| Create **DuckDB** database connection     | `dbc <- nodbi::src_duckdb(dbname = "name_of_my_database", collection = "name_of_my_collection")`                        |
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
development program (paediatric investigation plan, PIP):

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

# Find unique trial identifiers for trials that have nore than
# one record, for example for several EU Member States:
uniqueids <- dbFindIdsUniqueTrials(con = db)
# Searching for duplicate trials... 
#  - Getting all trial identifiers (may take some time), 339 found in collection
#  - Finding duplicates among registers' and sponsor ids...
#  - 251 EUCTR _id were not preferred EU Member State record for 88 trials
#  - Keeping 88 records from EUCTR
# = Returning keys (_id) of 88 records in collection "some_collection_name"

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
#   Completed                   43   21
#   GB - no longer in EU/EEA     1    1
#   Ongoing                      2    1
#   Prematurely Ended            2    2
#   Temporarily Halted           1    1
```

- Add records from another register (CTGOV) into the same collection

- Search used in this example:
  <https://clinicaltrials.gov/ct2/results?cond=neuroblastoma&rslt=With&recrs=e&age=0&intr=Drug>

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
# Checking helper binaries: . . . done
# (1/3) Checking trials in CTGOV:
# Retrieved overview, records of 53 trial(s) are to be downloaded (estimate: 0.42 MB)
# Download status: 1 done; 0 in progress. Total size: 718.51 Kb (100%)... done!             
# (2/3) Converting to JSON, 53 records converted
# (3/3) Importing JSON records into database...
# = Imported or updated 28 trial(s)                
# Updated history ("meta-info" in "some_collection_name")
```

<div id="workflow-ctis-example">

</div>

- Add records from a fourth register (CTIS 🔔) into the same collection

Queries in the CTIS search interface can be automatically copied to the
clipboard so that a user can paste them into `queryterm`, see
[here](#3-script-to-automatically-copy-users-query-from-web-browser). As
of April 2023, more than 150 trials are publicly accessible in CTIS.

``` r
# Retrieve trials from another register:
ctrLoadQueryIntoDb(
  queryterm = "https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup?ageGroupCode=3",
  documents.path = "./files-ctis", # new since 2023-05-01
  con = db
)
# * Found search query from CTIS: ageGroupCode=3
# (1/5) Downloading trials list, found 135 trials
# (2/5) Downloading and processing part I and parts II... (estimate: 20.25 Mb)
# Download status: 135 done; 0 in progress. Total size: 20.35 Mb (100%)... done!             
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# (3/5) Downloading and processing additional data: 
# publicevents
# summary
# layperson
# csr
# cm
# inspections
# publicevaluation 
# Download status: 213 done; 0 in progress. Total size: 6.24 Mb (100%)... done!             
# 135
# (4/5) Importing JSON records into database...
# (5/5) Updating with additional data: . . .       
# Downloading documents into 'documents.path' = ./files-ctis
# - Created directory /Users/ralfherold/Daten/mak/r/emea/ctrdata/files-ctis
# - Getting ids of lists with document information
# - Downloading 1594 lists with document information (estimate: 31.88 Mb)
# Download status: 1594 done; 0 in progress. Total size: 4.93 Mb (100%)... done!             
# - Processing document information in 1594 lists
# - Creating subfolder for each trial
# - Applying 'documents.regexp' to 1651 documents:
# - Downloading 380 documents
# Download status: 380 done; 0 in progress. Total size: 223.96 Mb (100%)... done!             
# Newly saved 356 document(s) for 116 trial(s) (latest versions only, and 
# deduplicated if e.g. in applications and authorised parts); 
# 0 document(s) for 0 trial(s) already existed in ./files-ctis
# = Imported / updated 135 / 135 / 2 / 135 records on 135 trial(s)
# Updated history ("meta-info" in "some_collection_name")


allFields <- dbFindFields(".*", db)
length(allFields[grepl("CTIS", names(allFields))])
# [1] 2690


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

dbGetFieldsIntoDf("publicEvaluation.partIRfiConsiderations.rfiConsiderations.consideration", db)[1,]
#               _id    publicEvaluation.partIRfiConsiderations.rfiConsiderations.consideration
# 2022-500024-30-00    A detailed description of potential protocol deviations for the per-protocol 
# set is missing on page 63/76 of the protocol and should be added. / Please, also amend the 
# rationale why the secondary aims are exploratory, considering the study design, sample size,
# and statistical modeling. / The sample size is based on uncorrected two sided p-value, 
# ignoring multiplicity. The study will be more realistic by re-calculating what the sample-size 
# could be, based on the adjusted one-side p-value. Please elaborate your reply. / Sample-size 
# calculations make sense, except for considering multiplicity adjusted p-value. It is debatable 
# whether hierarchical testing should be applied in this project since the objectives and 
# outcomes are not ranked/ordered as hierarchical/sequential.


# use an alternative to dbGetFieldsIntoDf()
allData <- nodbi::docdb_query(src = db, key = db$collection, query = '{"ctrname":"CTIS"}')
# names of top-level data items
names(allData)
#  [1] "_id"                           "ctrname"                      
#  [3] "id"                            "record_last_import"           
#  [5] "title"                         "ctNumber"                     
#  [7] "ctStatus"                      "primarySponsor"               
#  [9] "coSponsors"                    "submissionDate"               
# [11] "initialApplicationId"          "applications"                 
# [13] "memberStatesConcerned"         "eeaStartDate"                 
# [15] "trialGlobalEnd"                "trialStartDate"               
# [17] "authorizedPartI"               "authorizedPartsII"            
# [19] "authorizationDate"             "isRmsTacitAssignment"         
# [21] "eudraCtInfo"                   "lastUpdated"                  
# [23] "mscTrialNotificationsInfoList" "totalPartIISubjectCount"      
# [25] "trialEndDate"                  "eeaEndDate"                   
# [27] "trialCountries"                "decisionDate"                 
# [29] "therapeuticAreas"              "recruitmentStatus"            
# [31] "sponsorType"                   "totalNumberEnrolled"          
# [33] "hasDeferrallApplied"           "hasAmendmentApplied"          
# [35] "cm"                            "publicEvaluation"             
# [37] "trialPhase"                    "ageGroup"                     
# [39] "gender"                        "startDateEU"                  
# [41] "endDateEU" 
# 
format(object.size(allData), "MB")
# [1] "111.9 Mb"
```

- Result-related trial information

Analyse some simple result details (see vignette for more examples):

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
  # some ctgov records use
  # location.name, others use
  # location.facility.name
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

# Example plot
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

- Retrieve protocols, statistical analysis plans and other documents
  into a local folder `./files/`

``` r
# euctr files are downloaded as part of results
ctrLoadQueryIntoDb(
  queryterm = q,
  euctrresults = TRUE,
  documents.path = "./files-euctr/",
  con = db
)

# ctgov files (integrated since 2023-05-01)
ctrLoadQueryIntoDb(
  queryterm = "cond=Neuroblastoma&type=Intr&recrs=e&phase=1&u_prot=Y&u_sap=Y&u_icf=Y",
  register = "CTGOV",
  documents.path = "./files-ctgov/",
  con = db
)
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
  (nevertheless, `dfMergeTwoVariablesRelevel()` can be used to merge and
  map two variables / fields into one).

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
