
<!-- README.md is generated from README.Rmd -->

[![](https://cranlogs.r-pkg.org/badges/ctrdata)](https://cran.r-project.org/package=ctrdata)
[![Build
Status](https://travis-ci.org/rfhb/ctrdata.png?branch=master)](https://travis-ci.org/rfhb/ctrdata)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rfhb/ctrdata?branch=master&svg=true)](https://ci.appveyor.com/project/rfhb/ctrdata)
[![codecov](https://codecov.io/gh/rfhb/ctrdata/branch/master/graph/badge.svg)](https://codecov.io/gh/rfhb/ctrdata)
\[Note codecov does not check MS Windows-only
code\]

[![Slack](https://img.shields.io/badge/Slack-Join-green.svg)](https://rfhb.slack.com/messages/C6N1Y75B6)
Join Slack channel and discuss

# ctrdata for aggregating and analysing clinical trials

The package `ctrdata` provides functions for retrieving (downloading)
information on clinical trials from public registers, and for
aggregating and analysing such information. It can be used for the
European Union Clinical Trials Register (“EUCTR”,
<https://www.clinicaltrialsregister.eu/>) and for ClinicalTrials.gov
(“CTGOV”, <https://clinicaltrials.gov/>). Development of `ctrdata`
started mid 2015 and was motivated by the wish to understand trends in
designs and conduct of trials and their availability for patients. The
package is to be used within the [R](https://www.r-project.org/) system.

Last edit 2019-08-10 for version 0.19.9001, with breaking changes, bug
fixes and new features:

  - minimised dependencies: works now with `RSQLite` (\>= 2.1.2), as
    well as with local and remote MongoDB servers, via R package `nodbi`
    (\>= 0.2.9.9100). This is a breaking change that could not be
    avoided when generalising the database access, which was made
    possible by introducing a REGEXP operator into
    [RSQLite](https://github.com/r-dbi/RSQLite/pull/296) and adding a
    set of methods to `nodbi` based on the Json1 extension of
    [SQLite](https://github.com/ropensci/nodbi/pull/25).

  - synonyms of active substances to better find trials can be retrieved
    with function `ctrFindActiveSubstanceSynonyms()`

  - dates are now returned as Date types, and some Yes / No fields are
    returned as logical, by function `dbGetFieldsIntoDf()`

  - personal annotations can be added when records are retrieved from a
    register (new options `annotate.text` and `annotate.mode` for
    function `ctrLoadQueryIntoDb()`), for later use in analysis

Main features:

  - Protocol-related information on clinical trials is easily retrieved
    (downloaded) from public online sources: Users define a query using
    the registers’ web pages interfaces and then use `ctrdata` for
    retrieving all trials resulting from the query.

  - Results-related information on these clinical trials is now included
    (since August 2017) when information is retrieved (downloaded).

  - Retrieved (downloaded) trial information is transformed and stored
    in a document-centric database (because the registers provide nested
    data), for fast and offline access. This can then be analysed with
    `R` (or others systems). Easily re-run a previous query to update a
    database collection.

  - Unique (de-duplicated) clinical trial records are identified (a
    database collection may hold information from more than one
    register, and trials may have more than one record in a register).
    `ctrdata` has also functions to merge protocol-related information
    from different registers and to recode it. Vignettes are provided to
    get started and with detailed examples such as analyses of time
    trends of details of clinical trial protocols.

Remember to respect the registers’ copyrights and terms and conditions
(see `ctrOpenSearchPagesInBrowser(copyright = TRUE)`). Please cite this
package in any publication as follows: `Ralf Herold (2019). ctrdata:
Retrieve and Analyze Information on Clinical Trials from Public
Registers. R package version 0.19, https://github.com/rfhb/ctrdata`

<!--

```r
citation("ctrdata")
#> 
#> To cite package 'ctrdata' in publications use:
#> 
#>   Ralf Herold (2019). ctrdata: Retrieve and Analyze Information on
#>   Clinical Trials from Public Registers. R package version
#>   0.18.9005. https://github.com/rfhb/ctrdata
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ctrdata: Retrieve and Analyze Information on Clinical Trials from Public Registers},
#>     author = {Ralf Herold},
#>     year = {2019},
#>     note = {R package version 0.18.9005},
#>     url = {https://github.com/rfhb/ctrdata},
#>   }
```
-->

Package `ctrdata` has been used for example for:

  - Blog post on [Innovation coming to paediatric
    research](https://paediatricdata.eu/2018/01/14/innovation-coming-to-paediatric-research/)

  - Report on [The impact of collaboration: The value of UK medical
    research to EU science and
    health](https://www.cancerresearchuk.org/about-us/we-develop-policy/we-work-with-government/exiting-the-eu/uk-and-eu-research#downloads)

Overview of functions used in sequence:

![Overview workflow](inst/image/README-ctrdata_sequence_diagram.jpeg)

# Installation

## 1\. Install package in R

Within [R](https://www.r-project.org/), use the following commands to
get and install package `ctrdata`:

``` r
# Development version from github.com:
install.packages("devtools")
devtools::install_github("ropensci/nodbi")

# Release version:
install.packages("ctrdata")

# Development version from github.com:
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")

# Set build_opts like this to build vignettes:
devtools::install_github("rfhb/ctrdata", build_opts = "")
```

Package `ctrdata` can be found [here on
CRAN](https://cran.r-project.org/package=ctrdata).

## 2\. Command line tools `perl`, `sed`, `cat` and `php` (5.2 or higher)

These command line tools are only required for
`ctrGetQueryUrlFromBrowser()`, a main function of package `ctrdata`.

In Linux and macOS, these are usually already installed.

For MS Windows, install [cygwin](https://cygwin.org/install.html): In
`R`, run `ctrdata::installCygwinWindowsDoInstall()` for an automated
minimal installation into `c:\cygwin` (installations in folders
corresponding to `c:\cygw*` will also be recognised and used).
Alternatively, install manually cygwin with packages `perl`, `php-jsonc`
and `php-simplexml`. This installation will consume about 160 MB disk
space; administrator credentials not
needed.

# Overview of functions in `ctrdata`

| Name                           | Function                                                                                                                   |
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------- |
| ctrOpenSearchPagesInBrowser    | Open search pages of registers or execute search in web browser                                                            |
| ctrFindActiveSubstanceSynonyms | Find synonyms and alternative names for an active substance                                                                |
| ctrGetQueryUrlFromBrowser      | Import from clipboard the URL of a search in one of the registers                                                          |
| ctrLoadQueryIntoDb             | Retrieve (download) or update, and annotate, information on clinical trials from register and store in database collection |
| dbQueryHistory                 | Show the history of queries that were downloaded into the database collection                                              |
| dbFindFields                   | Find names of fields in the database collection                                                                            |
| dbFindIdsUniqueTrials          | Produce a vector of de-duplicated identifiers of clinical trial records in the database collection                         |
| dbGetFieldsIntoDf              | Create a data.frame from records in the database collection with the specified fields                                      |
| dfMergeTwoVariablesRelevel     | Merge two variables into a single variable, optionally map values to a new set of values                                   |
| installCygwinWindowsDoInstall  | Convenience function to install a cygwin environment (MS Windows only)                                                     |

# Example workflow

The aim is to download protocol-related trial information and tabulate
the trials’ status.

  - Attach package `ctrdata`:

<!-- end list -->

``` r
library(ctrdata)
#> Information on this package and how to use it:
#> https://cran.r-project.org/package=ctrdata
#> 
#> Please respect the requirements and the copyrights of the
#> clinical trial registers when using their information. Call
#> ctrOpenSearchPagesInBrowser(copyright = TRUE) and visit
#> https://www.clinicaltrialsregister.eu/disclaimer.html
#> https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use
#> Checking helper binaries: 
#> Checking completed.
```

  - Open registers’ advanced search pages in browser:

<!-- end list -->

``` r
ctrOpenSearchPagesInBrowser()

# Please review and respect register copyrights:
ctrOpenSearchPagesInBrowser(copyright = TRUE)
```

  - Click search parameters and execute search in browser

  - Copy address from browser address bar to clipboard

  - Get address from clipboard:

<!-- end list -->

``` r
q <- ctrGetQueryUrlFromBrowser()
# * Found search query from EUCTR.

q
#                                  query-term query-register
# 1 query=cancer&age=under-18&phase=phase-one          EUCTR
```

  - Retrieve protocol-related information, transform, save to database
    and analyse:

Under the hood, scripts `euctr2json.sh` and `xml2json.php` (in
`ctrdata/exec`) transform EUCTR plain text files and CTGOV xml files to
ndjson format, which is imported into the database. If no database
connection is specified in parameter `con`, an in-memory SQLite database
is created.

``` r
# Connect to (or create) a SQLite database:
db <- nodbi::src_sqlite(dbname = "some_database_name.sqlite_file", 
                        collection = "some_collection_name")
# Alternative, if a MongoDB is available to user:
# db <- nodbi::src_mongo(url = "mongodb://localhost", 
#                        db = "some_database_name",
#                        collection = "some_collection_name")

# Retrieve trials from public register:
ctrLoadQueryIntoDb(
  queryterm = 
    paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?", 
           "query=cancer&age=under-18&phase=phase-one"),
  con = db)

# Minimalistic, with in-memory SQLite:  
# ctrLoadQueryIntoDb(q)
```

Tabulate the status of paediatric trials that are part of an agreed
development program

``` r
# Get all records that have values in all specified fields:
result <- dbGetFieldsIntoDf(
  fields = c("a7_trial_is_part_of_a_paediatric_investigation_plan", 
             "p_end_of_trial_status", 
             "a2_eudract_number"),
  con = db)
# Warning message:
# 1332 of 2227 records dropped which did not have values for any of the specified fields.  

# Find trials records that are duplicated by EU Member State: 
uniqueids <- dbFindIdsUniqueTrials(con = db)
# * Total of 3559 records in collection.
# Searching for duplicates, found 
#  - 1773 EUCTR _id were not preferred EU Member State record of trial
#  - 4 CTGOV _id (nct) in EUCTR a52_us_nct_...
#  - 6 CTGOV secondary_id / nct_alias / org_study_id in EUCTR a2_eudract_number
#  - 0 CTGOV secondary_id / nct_alias / org_study_id in EUCTR a52_us_nct_...
#  - 0 CTGOV secondary_id / nct_alias / org_study_id in EUCTR a51_isrctn_...
#  - 0 CTGOV secondary_id / nct_alias / org_study_id in EUCTR a41_sponsors_protocol_...
# Concatenating 459 records from EUCTR and 1318 from CTGOV:
# = Returning keys (_id) of 1777 out of total 3559 records in collection "ctrdata".

# Keep only unique / deduplicated records:
result <- result[ result[["_id"]] %in% uniqueids, ]

# Tabulate the clinical trial information:
with (result, table (p_end_of_trial_status, 
                     a7_trial_is_part_of_a_paediatric_investigation_plan))
#                    a7_trial_is_part_of_a_paediatric_investigation_plan
# p_end_of_trial_status Information not present in EudraCT  No Yes
#    Completed                                          60  58   7
#    Ongoing                                            80 201   7
#    Prematurely Ended                                  24  14   0
#    Restarted                                           0   1   0
#    Temporarily Halted                                  1   0   0
```

# Representation of JSON in databases

## MongoDB

![Example JSON representation in
MongoDB](inst/image/README-ctrdata_json_mongodb.jpg)

## SQLite

![Example JSON representation in
SQLite](inst/image/README-ctrdata_json_sqlite.jpg)

# Features in the works

  - Explore using the Windows Subsystem for Linux (WSL) instead of
    cygwin

  - Merge results-related information retrieved from different registers
    (e.g. corresponding endpoints) and prepare for analysis across
    trials.

  - Explore relevance to retrieve previous versions of protocol- and
    results-related information

# Acknowledgements

  - Data providers and curators of the clinical trial registers. Please
    review and respect their copyrights and terms and conditions
    (`ctrOpenSearchPagesInBrowser(copyright = TRUE)`).

  - This package `ctrdata` has been made possible based on the work done
    for [R](https://www.r-project.org/),
    [curl](https://cran.r-project.org/package=curl),
    [httr](https://cran.r-project.org/package=httr),
    [xml2](https://cran.r-project.org/package=xml2),
    [rvest](https://cran.r-project.org/package=rvest).
    [mongolite](https://cran.r-project.org/package=mongolite),
    [nodbi](https://github.com/ropensci/nodbi),
    [RSQLite](https://CRAN.R-project.org/package=RSQLite) and
    [clipr](https://cran.r-project.org/packages=clipr),

# Issues and notes

  - Please file issues and bugs
    [here](https://github.com/rfhb/ctrdata/issues).

  - Package `ctrdata` should work and is continually tested on Linux,
    Mac OS X and MS Windows systems. Linux and MS Windows are tested
    using continuous integration, see badges at the beginning of this
    document. Please file an issue for any problems.

  - The information in the registers may not be fully correct; see [this
    publication on CTGOV](https://www.bmj.com/content/361/bmj.k1452).

  - No attempts were made to harmonise field names between registers,
    but `dfMergeTwoVariablesRelevel()` can be used to merge and map two
    variables / fields into one. So far, there is no typing of database
    fields; they are all strings (except for indices).
