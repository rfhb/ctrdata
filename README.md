[![Build Status](https://travis-ci.org/rfhb/ctrdata.png?branch=master)](https://travis-ci.org/rfhb/ctrdata)

# README.md for R package ctrdata on github.com

Online version of this document: [https://github.com/rfhb/ctrdata/](https://github.com/rfhb/ctrdata/)

## Background

The package `ctrdata` provides functions for retrieving information from public registers of clinical trials, and for aggregating and analysing such information. It can be used for the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/) and for ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/). Development of `ctrdata` started mid 2015 and was motivated by the wish to understand trends in designs and conduct of trials and their availability for patients. The package can only be used within the [R](https://www.r-project.org/) computing system. Last edit 2016-10-31 for version 0.9 (see NEWS.md). 

Main features:

* Protocol-related information on clinical trials is easily retrieved from public online sources: Users define queries using the registers' web pages interfaces and then use `ctrdata` for retrieving the results of the queries. 

* Retrieved (downloaded) information is transformed and stored in a document-centric database (mongo), for fast and offline access to detailed information on clinical trial protocols. This can then be analysed with `R` (or others systems). Easily re-run a previous query to update the database. In `ctrdata/exec`, scripts `euctr2json.sh` and `xml2json.php` transform EUCTR plain text files and CTGOV xml files to json format. 

* Unique (de-duplicated) clinical trial records are identified (as a database may hold information from more than one register, and trials may have more than one record in a register). `ctrdata` has also functions to merge information from different registers and to recode it. Vignettes are provided to get started and with detailed examples such as analyses of time trends of details of clinical trial protocols. 

Remember to respect the registers' copyrights and terms and conditions. 

Overview of functions used in sequence:

![Overview workflow][1]

## Installation

Within [R](https://www.r-project.org/), use the following commands to get and install the current development version of package `ctrdata` from github.com:

```R
#
install.packages("devtools")
#
# Unfortunately, package rmongodb was removed from CRAN on 2016-08-25, 
# see https://cran.r-project.org/web/packages/rmongodb/index.html. 
# Work is ongoing for package ctrdata to use a different mongodb connector. 
# Until this is ready, rmongodb can be installed as follows: 
devtools::install_github("mongosoup/rmongodb")
#
devtools::install_github("rfhb/ctrdata")
#
# If this fails, see 'Issues and Notes' below.
#
# Note the packages that will be installed 
# automatically as dependencies of ctrdata: 
# rmongodb, RCurl, curl, clipr
#
```

Other requirements:

* A local [mongodb](https://www.mongodb.org/) version 3 installation: 

From this installation, binaries `mongoimport` and `mongo` need to be accessed. For Ubuntu, please note this seems to ship mongodb version 2.x, not the required version 3; please follow installation instructions [here for Ubuntu 15, same as for Debian](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-debian/#install-mongodb) and here for [Ubuntu 14 and earlier](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/#install-mongodb). For Mac OS X and Windows, please follow installation instructions [here](https://www.mongodb.org/downloads#production). For Mac OS X alternatively install mongo using [homebrew](http://brew.sh/) using the command line `brew install mongodb`. 

* Command line tools `perl`, `sed`, `cat` and `php` (5.2 or higher): 

In Linux and Mac OS X systems, these are usually already installed. For MS Windows, the recommendation is to install [cygwin](https://cygwin.com/install.html): In `R`, run `ctrdata::installCygwinWindowsDoInstall()` for an automated installation into `c:\cygwin`. Manual cygwin installation: In the graphical interface of the cygwin installer, type `perl` in the `Select packages` field and click on `Perl () Default` so that this changes to `Perl () Install`, repeat with `php-jsonc` and `php-simplexml` (shown [here](http://slu.livejournal.com/17395.html)). An installation does not require administrator credentials, see [here](https://cygwin.com/faq/faq.html#faq.setup.noroot). `Rtools` is *not* required for `ctrdata` on MS Windows. Package php7.0-xml has to be installed if PHP 7 is used (php5 includes this). 


## Overview of functions in `ctrdata`

Name  | Function
---------------------------- | --------------------------------------------
ctrOpenSearchPagesInBrowser	| Open advanced search pages of register(s) or execute search in default web browser
ctrGetQueryUrlFromBrowser	| Import from clipboard the URL of a search in one of the registers
ctrLoadQueryIntoDb	| Retrieve or update information on clinical trials from register and store in database
dbQueryHistory	| Show the history of queries that were loaded into a database
dbFindVariable	| Find names of keys (fields) in the database
dbFindIdsUniqueTrials	| Produce a vector of de-duplicated identifiers of clinical trial records in the database
dbGetVariablesIntoDf	| Create a data frame from records in the database that have specified fields in the database
dfMergeTwoVariablesRelevel	| Merge related variables into a single variable, and optionally map values to a new set of values
installCygwinWindowsDoInstall	| Convenience function to install a cygwin environment under MS Windows, including perl, php, sed

<!--

```R
#
# library(DiagrammeR)
# mermaid("
# sequenceDiagram
# 
#   user_R->>database: define collection and namespace
# 
#   note left of user_R: query and download
#     
#   user_R->>euctr: ctrOpenSearchPagesInBrowser()
#   euctr->>euctr: user defines query
#   euctr->>user_R: ctrGetQueryUrlFromBrowser()
# 
#   euctr->>database: ctrLoadQueryIntoDb()
#     
#   ctgov->>ctgov: user defines query
#   ctgov->>database: ctrLoadQueryIntoDb()
#   
#   note left of user_R: prepare and analyse
#     
#   database->>user_R: dbFindVariable()
#   database->>user_R: dbGetVariablesIntoDf()
#   database->>user_R: dbFindIdsUniqueTrials()
# 
#   user_R->>user_R: dfMergeTwoVariablesRelevel()
# 
#   user_R->>user_R: any R functions such as summary()
# 
# ")
#
```
-->



## Example workflow

### Download protocol-related trial information and tabulate trial status

* Attach package `ctrdata`: 
```R
library(ctrdata)
```

* Open register's advanced search page in browser: 
```R
#
ctrOpenSearchPagesInBrowser()
#
# Please review and respect register copyrights:
#
ctrOpenSearchPagesInBrowser(copyright = TRUE)
#
```

* Click search parameters and execute search in browser 

* Copy address from browser address bar to clipboard

* Get address from clipboard: 
```R
#
q <- ctrGetQueryUrlFromBrowser()
#
# Found search query from EUCTR.
# [1] "cancer&age=under-18"
#
```

* Retrieve protocol-related information, transform, save to database and analyse:
```R
#
# Retrieve from public register
#
ctrLoadQueryIntoDb(register = "EUCTR", queryterm = "cancer&age=under-18")
#
# Same if q was defined as per above: ctrLoadQueryIntoDb(q)
#
# If no parameters are given for a database connection: uses mongodb
# on localhost, port 27017, database "users", collection "ctrdata"
# note: when run for first time, may download variety.js
#
# Get all records that have values in all specified fields.
# Note that b31_... is an element within the array b1_...
#
result <- dbGetVariablesIntoDf(c("b1_sponsor.b31_and_b32_status_of_the_sponsor", 
                                 "x5_trial_status", "a2_eudract_number"))
#
# Eliminate trials records duplicated by EU member state: 
#
result <- dbFindIdsUniqueTrials(result)
#
# Tabulate the status of the clinical trial on the date of information retrieval
# Note some trials have more than one sponsor and values are concatenated with /.
#
with (result, table (x5_trial_status, b1_sponsor.b31_and_b32_status_of_the_sponsor))
#
#                     b1_sponsor.b31_and_b32_status_of_the_sponsor
# x5_trial_status      Commercial  Non-Commercial  Non-Commercial / Non-Commercial
#   Completed                  81              32                                0
#   Ongoing                   205             239                               12
#   Prematurely Ended          15              12                                0
#   Restarted                   0               1                                0
#   Temporarily Halted          4               1                                0
#
```

### Representation in mongodb, as JSON

![Example JSON representation][2]



## In the works - next steps
 
* Add more package tests

* Make internal access to mongo database more generic

## Acknowledgements 

* Data providers and curators of the clinical trial registers. Please review and respect their copyrights and terms and conditions (`ctrOpenSearchPagesInBrowser(copyright = TRUE)`). 

* This package `ctrdata` has been made possible based on the work done for [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [clipr](https://github.com/mdlincoln/clipr), [rmongodb](https://github.com/mongosoup/rmongodb) and of course for [R](http://www.r-project.org/). 

* [Variety](https://github.com/variety/variety), a Schema Analyzer for MongoDB

* Contributors to community documentation


## Issues and notes

* Please file issues and bugs here: [https://github.com/rfhb/ctrdata/issues](https://github.com/rfhb/ctrdata/issues). 

* Package `ctrdata` should work on Linux, Mac OS X and MS Windows systems, but these could not all be tested, please file an issue in case of problems. 

* Plans include to map register fields to a single global definition, e.g. based on CDISC. Hence, the data format may change. 

* Results-related information will also be covered in the near future. 

* In case `devtool::install_github("rfhb/ctrdata")` fails, a proxy may need to be specified: 
```R
# set a proxy
library(httr)
set_config(use_proxy("proxy.server.domain", 8080))
# and / or
# change library path on windows to not use UNC notation (\\server\directory)
.libPaths("D:/my/directory/")
#
```

* Be aware that the information in the registers may or may not be fully correct; for example see [this publication on CTGOV](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3066456/#S7). 

* Similar to the EUCTR design, one record per concerned EU Member state per trial is retrieved from EUCTR with the default setting `details = TRUE`. 

* So far, no attempts are made to harmonise and map field names between different registers directly in the database. However, the function `dfMergeTwoVariablesRelevel()` can be used to manually merge and map two variables / fields. 

* So far, there is no typing of database fields; they are all strings (except for indices). 

* Package `ctrdata` also uses [Variety](https://github.com/variety/variety), which will automatically be downloaded into the package's `exec` directory when first using a function that needs it. This may fail if this directory is not writable for the user and this issue is not yet addressed. Note that `variety.js` may not work with remote mongo databases, see documentation of `dbFindVariable()`. 

* In case `curl` fails with an SSL error, run this code to update the certificates in the root of package `curl`:
```R
httr::GET("https://github.com/bagder/ca-bundle/raw/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt", write_disk(system.file("", package = "curl"), "inst/cacert.pem", overwrite = TRUE))
```

[1]: ./ctrdata_sequence_diagram.png "Sequence diagram"
[2]: ./ctrdata_json.jpg "JSON representation"

