[![Build Status](https://travis-ci.org/rfhb/ctrdata.png?branch=master)](https://travis-ci.org/rfhb/ctrdata)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rfhb/ctrdata?branch=master&svg=true)](https://ci.appveyor.com/project/rfhb/ctrdata)
[![codecov](https://codecov.io/gh/rfhb/ctrdata/branch/master/graph/badge.svg)](https://codecov.io/gh/rfhb/ctrdata)
Codecov does not check MS Windows-only code. 

# README.md for R package ctrdata on github.com

Online version of this document: [https://github.com/rfhb/ctrdata/](https://github.com/rfhb/ctrdata/)

## Background

The package `ctrdata` provides functions for retrieving (downloading) information clinical trials from public registers, and for aggregating and analysing such information. It can be used for the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/) and for ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/). Development of `ctrdata` started mid 2015 and was motivated by the wish to understand trends in designs and conduct of trials and their availability for patients. The package is to be used within the [R](https://www.r-project.org/) system. Last edit 2017-06-28 for version 0.9.14 (see NEWS.md, continuous integration now working on MS Windows in addition to Linux).  

Main features:

* Protocol-related information on clinical trials is easily retrieved (downloaded) from public online sources: Users define a query using the registers' web pages interfaces and then use `ctrdata` for retrieving all trials resulting from the query. 

* Retrieved (downloaded) trial information is transformed and stored in a document-centric database (MongoDB), for fast and offline access. This can then be analysed with `R` (or others systems). Easily re-run a previous query to update the database.  

* Unique (de-duplicated) clinical trial records are identified (as a database may hold information from more than one register, and trials may have more than one record in a register). `ctrdata` has also functions to merge information from different registers and to recode it. Vignettes are provided to get started and with detailed examples such as analyses of time trends of details of clinical trial protocols. 

Remember to respect the registers' copyrights and terms and conditions. 

Overview of functions used in sequence:

![Overview workflow][1]

## Installation

### 1. Within R

Within [R](https://www.r-project.org/), use the following commands to get and install the current development version of package `ctrdata` from github.com:

### 2. Local [mongodb](https://www.mongodb.org/) (version 3) installation

Follow instructions for various operating systems [here](https://docs.mongodb.com/manual/administration/install-community/). For macOS alternatively use [homebrew](http://brew.sh/): `brew install mongodb`. From this installation, binaries `mongoimport{.exe}` and `mongo{.exe}` are needed.

### 3. Command line tools `perl`, `sed`, `cat` and `php` (5.2 or higher)

In Linux and macOS, these are usually already installed. For MS Windows, install [cygwin](https://cygwin.com/install.html): In `R`, run `ctrdata::installCygwinWindowsDoInstall()` for an automated installation into `c:\cygwin`; alternatively manually install cygwin with packages `perl`, `php-jsonc` and `php-simplexml` (administrator credentials not needed). 


```R
#
install.packages("devtools")
#
devtools::install_github("rfhb/ctrdata")
#
# - In case of a problem with the SSL CA cert:
install.packages("httr")
httr::set_config(httr::config(ssl_verifypeer = 0L))
devtools::install_github("rfhb/ctrdata")
# 
# - If not connecting, a proxy may need to be set:
install.packages("httr")
httr::set_config(httr::use_proxy("proxy.server.domain", 8080))
devtools::install_github("rfhb/ctrdata")
#
```


## Overview of functions in `ctrdata`

Name  | Function
---------------------------- | --------------------------------------------
ctrOpenSearchPagesInBrowser	| Open search pages of registers or execute search in web browser
ctrGetQueryUrlFromBrowser	| Import from clipboard the URL of a search in one of the registers
ctrLoadQueryIntoDb	| Retrieve (download) or update information on clinical trials from register and store in database
dbQueryHistory	| Show the history of queries that were downloaded into the database
dbFindVariable	| Find names of keys (fields) in the database
dbFindIdsUniqueTrials	| Produce a vector of de-duplicated identifiers of clinical trial records in the database
dbGetVariablesIntoDf	| Create a data frame from records in the database with specified fields 
dfMergeTwoVariablesRelevel	| Merge two variables into a single variable, optionally map values to a new set of values
installCygwinWindowsDoInstall	| Convenience function to install a cygwin environment under MS Windows, including perl, sed, cat and php

<!--

```R
# save as 798 * 698

library(DiagrammeR)
mermaid("
sequenceDiagram

  user_R->>database: define collection and namespace

  note left of user_R: query and download

  user_R->>euctr: ctrOpenSearchPagesInBrowser()
  euctr->>euctr: user defines query
  ctgov->>ctgov: user defines query

  euctr->>database: ctrLoadQueryIntoDb()

  ctgov->>database: ctrLoadQueryIntoDb()

  note left of user_R: prepare and analyse

  database->>database: dbFindVariable()

  database->>user_R: dbGetVariablesIntoDf()
  database->>database: dbFindIdsUniqueTrials()

  user_R->>user_R: dfMergeTwoVariablesRelevel()

  user_R->>user_R: any R functions such as summary()

")

```
-->


## Example workflow

### Download protocol-related trial information and tabulate trial status

* Attach package `ctrdata`: 
```R
#
library(ctrdata)
# 
# Information on this package and how to use it: 
# https://github.com/rfhb/ctrdata/
# 
# Please respect the requirements and the copyrights of the
# clinical trial registers when using their information. Call
# ctrOpenSearchPagesInBrowser(copyright = TRUE) and visit
# 
# https://www.clinicaltrialsregister.eu/disclaimer.html
# https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use
# 
# 
# Helper binaries tested.
#
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
#          query-term   query-register
# 1  cancer&age=adult            EUCTR
#
```

* Retrieve protocol-related information, transform, save to database and analyse:
```R
#
# Retrieve from public register
#
ctrLoadQueryIntoDb("https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&age=adult")
#
# Alternatively: ctrLoadQueryIntoDb(q)
#
# If no parameters are given for a database connection: mongodb is used
# on localhost, port 27017, database "users", collection "ctrdata". 
# Note: when run for first time, may download variety.js
#
# Under the hood, scripts `euctr2json.sh` and `xml2json.php` (in `ctrdata/exec`) 
# transform EUCTR plain text files and CTGOV xml files to json format.
#
# Get all records that have values in all specified fields.
# Note that b31_... is an element within the array b1_...
#
result <- dbGetVariablesIntoDf(c("b1_sponsor.b31_and_b32_status_of_the_sponsor", 
                                 "x5_trial_status", "a2_eudract_number"))
#
# Eliminate trials records duplicated by EU member state: 
#
uniqueids <- dbFindIdsUniqueTrials()
result    <- result[ result[["_id"]] %in% uniqueids, ]
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
 
* Cover results-related information. 


## Acknowledgements 

* Data providers and curators of the clinical trial registers. Please review and respect their copyrights and terms and conditions (`ctrOpenSearchPagesInBrowser(copyright = TRUE)`). 

* This package `ctrdata` has been made possible based on the work done for [Variety](https://github.com/variety/variety), [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [clipr](https://github.com/mdlincoln/clipr), [mongolite](https://cran.r-project.org/web/packages/mongolite/) and for [R](http://www.r-project.org/). 


## Issues and notes

* Please file issues and bugs here: [https://github.com/rfhb/ctrdata/issues](https://github.com/rfhb/ctrdata/issues). 

* Package `ctrdata` should work and was tested on Linux, Mac OS X and MS Windows systems. Linux and MS Windows are tested using continuous integration, see badges at the beginning of this document. Please file an issue for any problems. 

* The information in the registers may not be fully correct; see [this publication on CTGOV](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3066456/#S7). 

* No attempts were made to harmonise field names between registers, but `dfMergeTwoVariablesRelevel()` can be used to merge and map two variables / fields into one. So far, there is no typing of database fields; they are all strings (except for indices). 


[1]: ./ctrdata_sequence_diagram.jpeg "Sequence diagram"
[2]: ./ctrdata_json.jpg "JSON representation"

