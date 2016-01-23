---
output:
  html_document:
    toc: yes
  pdf_document:
    toc: yes
  word_document: default
---

[![Build Status](https://travis-ci.org/rfhb/ctrdata.svg?branch=master)](https://travis-ci.org/rfhb/ctrdata)

# README.md for R package ctrdata on github.com

## Background

The package `ctrdata` provides functions for retrieving information from public registers, and for aggregating and analysing such information. It can be used for the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/) and for ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/). Development of `ctrdata` started mid 2015 and was motivated by the wish to understand trends in designs and conduct of trials and their availability for patients. The package can only be used within the [R](https://www.r-project.org/) computing system. Last edit 2016-01-23 for version 0.5.9 (preparations ongoing for CRAN submission). 

Features implemented so far:

* Protocol-related information on clinical trials is retrieved from public online sources. 

* Users define queries using the registers' web pages interfaces and then use `ctrdata` for retrieving the results of the queries. 

* Retrieved (downloaded) information is transformed and stored in a document-centric database (mongo) for further access. This provides fast and offline access to detailed information on clinical trials, for analysis in `R` (see examples). 

* Unique (de-duplicated) clinical trial records are identified, as the same database may hold information from more than one register. 
  
* Records in a database can be updated by a simple command. 

* In the background, specially-written scripts `exec/euctr2json.sh` and `exec/xml2json.php` transform EUCTR plain text files and CTGOV xml files to json format. 

* Reminder to respect the registers' copyrights and terms and conditions is shown when loading the package. Please also read the "Acknowledgments" and "Notes" sections below. 


## Installation

Within [R](https://www.r-project.org/), use the following commands to get and install the current development version of package `ctrdata` from github.com:

```R
#
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")
#
# Note the packages that will be installed 
# automatically as dependencies of ctrdata: 
# rmongodb, RCurl, curl, clipr
#
```

Other requirements:

* A local [mongodb](https://www.mongodb.org/) version 3 installation: 

From this installation, binaries `mongoimport` and `mongo` required. For Ubuntu, please note this seems to ship mongodb version 2.x, not the required version 3; please follow installation instructions [here for Ubuntu 15, same as for Debian](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-debian/#install-mongodb) and here for [Ubuntu 14 and earlier](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/#install-mongodb). For Mac OS X and Windows, please follow installation instructions [here](https://www.mongodb.org/downloads#production). For Mac OS X alternatively install mongo using [homebrew](http://brew.sh/) using the command line `brew install mongodb`. 

* Command line tools `perl`, `sed`, `cat` and `php` (5.2 or higher): 

In Linux and Mac OS X systems, these are usually already installed. For MS Windows, the recommendation is to install [cygwin](https://cygwin.com/install.html): In `R`, run `ctrdata::installCygwinWindowsDoInstall()` for an automated installation into `c:\cygwin`. Manual cygwin installation: In the graphical interface of the cygwin installer, type `perl` in the `Select packages` field and click on `Perl () Default` so that this changes to `Perl () Install`, repeat with `php-jsonc` and `php-simplexml` (shown [here](http://slu.livejournal.com/17395.html)). An installation does not require administrator credentials, see [here](https://cygwin.com/faq/faq.html#faq.setup.noroot). `Rtools` is *not* required for `ctrdata` on MS Windows. 


## Overview of functions in `ctrdata`

Name  | Function
------------- | -------------
ctrOpenSearchPagesInBrowser	| Open advanced search pages of register(s) in default web browser.
ctrQueryHistoryInDb	| Show the history of queries that were loaded into a database
ctrGetQueryUrlFromBrowser	| Import from clipboard the URL of a search in one of the registers
ctrLoadQueryIntoDb	| Retrieve information on clinical trials from register and store in database
dbFindIdsUniqueTrials	| This function checks for duplicates in the database based on the clinical trial identifier and returns a list of ids of unique trials
dbFindVariable	| Find names of keys (fields) in the database
dbFindUniqueEuctrRecord	| Select a single trial record when there are records for different EU Member States for this trial
dbGetVariablesIntoDf	| Create a data frame from records in the database that have specified fields in the database
dfMergeTwoVariablesRelevel	| Merge related variables into a single variable, and optionally map values to a new set of values.
installMongoCheckVersion	| Check the version of the build of the mongo server to be used
installMongoFindBinaries	| Convenience function to find location of mongo database binaries (mongo, mongoimport)
installCygwinWindowsDoInstall	| Convenience function to install a cygwin environment under MS Windows, including perl and php
installCygwinWindowsTest	| Convenience function to test for working cygwin installation


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
ctrLoadQueryIntoDb(q)
#
# If no parameters are given for a database connection: uses mongodb
# on localhost, port 27017, database "users", collection "ctrdata"
# note: when run for first time, may download variety.js
#
# Show which queries have been downloaded into the database so far
#
ctrQueryHistoryInDb()
#
# Total number of records: 6143
# Number of queries in history: 2
#       query-timestamp query-register query-records                  query-term
# 1 2016-01-13-10-51-56          CTGOV          5233 type=Intr&cond=cancer&age=0
# 2 2016-01-13-10-40-16          EUCTR           910         cancer&age=under-18
#
# find names of fields of interest in database:
#
dbFindVariable("date")
#
# Returning first of 20 keys found.
# [1] "firstreceived_date"
#
# Get all records that have values in all specified fields.
# Note that b31_... is an element within the array b1_...
#
result <- dbGetVariablesIntoDf(c("b1_sponsor.b31_and_b32_status_of_the_sponsor", "x5_trial_status"))
#
# Tabulate the status of the clinical trial on the date of information retrieval
#
with (result, table (x5_trial_status, b31_and_b32_status_of_the_sponsor))
#
#                     b31_and_b32_status_of_the_sponsor
# x5_trial_status      Commercial Non-Commercial
#   Completed                 138             30
#   Not Authorised              3              0
#   Ongoing                   339            290
#   Prematurely Ended          35              4
#   Restarted                   8              0
#   Temporarily Halted         14              4
#
```

### More and complex examples

Can be found here: [https://cdn.rawgit.com/rfhb/ctrdata/master/inst/doc/EXAMPLES.html](https://cdn.rawgit.com/rfhb/ctrdata/master/inst/doc/EXAMPLES.html)


## In the works - next steps
 
* Provide differential update mechanism (using the RSS feeds provided by the registers), which should be more efficient than the current full re-download

* Provide examples and possibly special functions to support analysing time trends of clinical trials

* Make internal access to mongo database more generic


## Acknowledgements 

* Data providers and curators of the clinical trial registers. Please review and respect their copyrights and terms and conditions (`ctrOpenSearchPagesInBrowser(copyright = TRUE)`). 

* This package `ctrdata` has been made possible based on the work done for [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [clipr](https://github.com/mdlincoln/clipr), [rmongodb](https://github.com/mongosoup/rmongodb) and of course for [R](http://www.r-project.org/). 

* [Variety](https://github.com/variety/variety), a Schema Analyzer for MongoDB

* Contributors to community documentation


## Issues and notes

* Please file issues and bugs here: [https://github.com/rfhb/ctrdata/issues](https://github.com/rfhb/ctrdata/issues). 

* Package `ctrdata` should work on Linux, Mac OS X and MS Windows systems, but these could not all be tested, please file an issue in case of problems. 

* Be aware that the information in the registers may or may not be fully correct; for example see [this publication on CTGOV](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3066456/#S7). 

* Similar to the EUCTR design, one record per concerned EU Member state per trial is retrieved from EUCTR with the default setting `details = TRUE`. 

* So far, no attempts are made to harmonise and map field names between different registers directly in the database. However, the function `dfMergeTwoVariablesRelevel()` can be used to manually merge and map two variables / fields. 

* So far, there is no typing of database fields; they are all strings (except for indices). 

* Package `ctrdata` also uses [Variety](https://github.com/variety/variety), which will automatically be downloaded into the package's `exec` directory when first using a function that needs it. This may fail if this directory is not writable for the user and this issue is not yet addressed. Note that `variety.js` may not work with remote mongo databases, see documentation of `dbFindVariable()`. 

* In case `curl` fails with an SSL error, run this code to update the certificates in the root of package `curl`:
```R
httr::GET("https://github.com/bagder/ca-bundle/raw/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt", write_disk(system.file("", package = "curl"), "inst/cacert.pem", overwrite = TRUE))
```

