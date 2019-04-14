
<!-- README.md is generated from README.Rmd. -->

[![Build
Status](https://travis-ci.org/rfhb/ctrdata.png?branch=master)](https://travis-ci.org/rfhb/ctrdata)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rfhb/ctrdata?branch=master&svg=true)](https://ci.appveyor.com/project/rfhb/ctrdata)
[![codecov](https://codecov.io/gh/rfhb/ctrdata/branch/master/graph/badge.svg)](https://codecov.io/gh/rfhb/ctrdata)
\[Note codecov does not check MS Windows-only code\]

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

Last edit 2019-04-14 for version 0.18.1, with bug fixes and new
features:

  - dates are now returned as Date types, and some Yes / No fields are
    returned as logical, by function `dbGetFieldsIntoDf()`,
  - personal annotations can be added when records are retrieved from a
    register (new options `annotate.text` and `annotate.mode` for
    function `ctrLoadQueryIntoDb()`), for later use in analysis, and
  - synonyms of active substances to better find trials can be retrieved
    with function `ctrFindActiveSubstanceSynonyms()`,
  - improved functioning with remote Mongo databases, and removed need
    for local installation of MongoDB.

Main features:

  - Protocol-related information on clinical trials is easily retrieved
    (downloaded) from public online sources: Users define a query using
    the registers’ web pages interfaces and then use `ctrdata` for
    retrieving all trials resulting from the query.

  - Results-related information on these clinical trials is now included
    (since August 2017) when information is retrieved (downloaded).

  - Retrieved (downloaded) trial information is transformed and stored
    in a document-centric database (MongoDB), for fast and offline
    access. This can then be analysed with `R` (or others systems).
    Easily re-run a previous query to update a database collection.

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
Registers. R package version 0.17. https://github.com/rfhb/ctrdata`

<!--

```r
citation("ctrdata")
#> Warning in citation("ctrdata"): no date field in DESCRIPTION file of
#> package 'ctrdata'
#> Warning in citation("ctrdata"): could not determine year for 'ctrdata' from
#> package DESCRIPTION file
#> 
#> To cite package 'ctrdata' in publications use:
#> 
#>   Ralf Herold (NA). ctrdata: Retrieve and Analyze Information on
#>   Clinical Trials from Public Registers. R package version
#>   0.18.9001. https://github.com/rfhb/ctrdata
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ctrdata: Retrieve and Analyze Information on Clinical Trials from Public Registers},
#>     author = {Ralf Herold},
#>     note = {R package version 0.18.9001},
#>     url = {https://github.com/rfhb/ctrdata},
#>   }
```
-->

Package `ctrdata` has been used for example for:

  - Blog post on [Innovation coming to paediatric
    research](https://paediatricdata.eu/2018/01/14/innovation-coming-to-paediatric-research/)

  - Report on [The impact of collaboration: The value of UK medical
    research to EU science and
    health](http://www.cancerresearchuk.org/about-us/we-develop-policy/we-work-with-government/exiting-the-eu/uk-and-eu-research#downloads)

Overview of functions used in sequence:

![Overview workflow](inst/image/README-ctrdata_sequence_diagram.jpeg)

# Installation

## 1\. Install package in R

Within [R](https://www.r-project.org/), use the following commands to
get and install package `ctrdata`:

``` r
# Release version:
install.packages("ctrdata")

# Development version from github.com:
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")
```

Package `ctrdata` can be found [here on
CRAN](https://cran.r-project.org/package=ctrdata).

## 2\. Command line tools `perl`, `sed`, `cat` and `php` (5.2 or higher)

These command line tools are only required for
`ctrGetQueryUrlFromBrowser()`, a main function of package `ctrdata`. In
Linux and macOS, these are usually already installed.

For MS Windows, install [cygwin](https://cygwin.org/install.html): In
`R`, run `ctrdata::installCygwinWindowsDoInstall()` for an automated
minimal installation into `c:\cygwin`. Alternatively, install manually
cygwin with packages `perl`, `php-jsonc` and `php-simplexml` (consumes
about 160 MB disk space; administrator credentials not needed).

## 3\. Mongo database

A remote or a local mongo database server can be used with the package
`ctrdata`. Suggested installation instructions for a local database
server are
[here](https://docs.mongodb.com/manual/administration/install-community/).

A remote mongo database server such as
[here](https://www.mongodb.com/cloud/atlas) could be used; this is shown
in the examples vignette.

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
#> Registered S3 method overwritten by 'rvest':
#>   method            from
#>   read_xml.response xml2
#> Information on this package and how to use it:
#> https://cran.r-project.org/package=ctrdata
#> 
#> Please respect the requirements and the copyrights of the
#> clinical trial registers when using their information. Call
#> ctrOpenSearchPagesInBrowser(copyright = TRUE) and visit
#> https://www.clinicaltrialsregister.eu/disclaimer.html
#> https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use
#> Testing helper binaries:
#> completed.
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

If no parameters are given for a database connection: mongodb is used on
localhost, port 27017, database “users”, collection “ctrdata”.

Under the hood, scripts `euctr2json.sh` and `xml2json.php` (in
`ctrdata/exec`) transform EUCTR plain text files and CTGOV xml files to
json format, which is imported into the database.

``` r
# Retrieve trials from public register:
ctrLoadQueryIntoDb(paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?", 
                          "query=cancer&age=under-18&phase=phase-one"))
# Alternative: 
# ctrLoadQueryIntoDb(q)
```

Tabulate the status of deduplicated trials

``` r
# Get all records that have values in all specified fields.
# Note that b31_... is an element within the array b1_...
result <- dbGetFieldsIntoDf(c("b1_sponsor.b31_and_b32_status_of_the_sponsor", 
                              "p_end_of_trial_status", "a2_eudract_number"))

# Eliminate trials records duplicated by EU Member State: 
uniqueids <- dbFindIdsUniqueTrials()
result    <- result[ result[["_id"]] %in% uniqueids, ]

# Tabulate the status of the clinical trial on the date of information retrieval
# Note some trials have more than one sponsor and values are concatenated with /.
with (result, table (p_end_of_trial_status, b1_sponsor.b31_and_b32_status_of_the_sponsor))
#                     b1_sponsor.b31_and_b32_status_of_the_sponsor
# p_end_of_trial_status    Commercial  Non-Commercial  Non-Commercial / Non-Commercial
#   Completed                      81              32                                0
#   Ongoing                       205             239                               12
#   Prematurely Ended              15              12                                0
#   Restarted                       0               1                                0
#   Temporarily Halted              4               1                                0
```

# Representation in mongodb, as JSON

![Example JSON representation](inst/image/README-ctrdata_json.jpg)

# Features in the works

  - Explore NoSQL databases other than Mongo

  - Explore using the Windows Subsystem for Linux (WSL) instead of
    cygwin

  - Merge results-related information retrieved from different registers
    (e.g. corresponding endpoints) and prepare for analysis across
    trials.

  - Explore relevance to retrieve previous versions of protocol- and
    results-related information

  - Abstract database access

# Acknowledgements

  - Data providers and curators of the clinical trial registers. Please
    review and respect their copyrights and terms and conditions
    (`ctrOpenSearchPagesInBrowser(copyright = TRUE)`).

  - This package `ctrdata` has been made possible based on the work done
    for [R](http://www.r-project.org/),
    [curl](https://github.com/jeroenooms/curl),
    [clipr](https://github.com/mdlincoln/clipr),
    [mongolite](https://cran.r-project.org/package=mongolite),
    [httr](https://cran.r-project.org/package=httr),
    [xml2](https://cran.r-project.org/package=xml2) and
    [rvest](https://cran.r-project.org/package=rvest).

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
