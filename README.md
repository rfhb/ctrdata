---
output:
  html_document:
    toc: yes
  pdf_document:
    toc: yes
  word_document: default
---
# README.md for R package ctrdata on github.com

## Aims

The aims of `ctrdata` include to provide functions primarily for retrieving information from public registers on clinical trials, and for aggregating and analysing downloaded information. This is primarily for the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/), but also for ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/). The registers have limited options in terms of aggregation and an application programming interface (API). Development of `ctrdata` started mid 2015, last edit 2015-10-08 for version 0.4. 

Key features implemented so far:

* Protocol-related information on clinical trials is retrieved from public online sources. 

* Users can define queries in the registers' browser-based interfaces and then use for retrieval. 

* Retrieved (downloaded) information is transformed and stored in a document-centric database (mongo) for further access.  

* This provides fast and offline access to detailed information on clinical trials, for analysis in `R` (examples below). 

* Unique (de-duplicated) clinical trial records are identified, as the database may well hold information from more than one register. 
  
* Records in a database can be updated by a simple command. 

* In the background, `exec/euctr2json.sh` is a special script that transforms EUCTR plain text files to json format. 

* Reminder of the copyrights and terms and conditions of the respective register shown when loading the package. 

Please read the "Acknowledgments" and "Notes" sectionsbelow. 

Please file issues and bugs here: [https://github.com/rfhb/ctrdata/issues](https://github.com/rfhb/ctrdata/issues). 

## Installation

Within R, use the following commands to get and install the current development version of package `ctrdata` from github.com:

```R
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")
```

Other requirements:

* A local [mongodb](https://www.mongodb.org/) version 3 installation. From this installation, binaries `mongoimport` and `mongo` required. Note Ubuntu seems to ship mongodb version 2.x, not the required version 3. Please follow installation instructions [here for Ubuntu 15, same as for Debian](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-debian/#install-mongodb) and here for [Ubuntu 14 and earlier](http://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/#install-mongodb).   

* Command line tools `perl`, `sed`, `cat` and `php` (5.2 or higher). 

To satisfy requirements for MS Windows, the recommendation is:

* An installation of [cygwin](https://cygwin.com/install.html). 

In R, simply run `ctrdata::installCygwinWindowsDoInstall()` for an automated installation into `c:\cygwin`. 

For manual instalation, cygwin can be installed without administrator credentials (details are explained [here](https://cygwin.com/faq/faq.html#faq.setup.noroot)). In the graphical interface of the cygwin installer, type `perl` in the `Select packages` field and click on `Perl () Default` so that this changes to `Perl () Install`, repeat with `php-jsonc` and `php-simplexml` (details are shown [here](http://slu.livejournal.com/17395.html)). 

`Rtools` is *not* required for `ctrdata` on MS Windows. 


## Overview of functions in `ctrdata`

Name  | Function
------------- | -------------
ctrOpenSearchPagesInBrowser	| Open advanced search pages of register(s) in default web browser.
ctrQueryHistoryInDb	| Show the history of queries that were loaded into a database
ctrGetQueryUrlFromBrowser	| Import from clipboard the URL of a search in one of the registers
ctrLoadQueryIntoDb	| Retrieve information on clinical trials from register and store in database
dbFindIdsUniqueTrials	| This function checks for duplicates in the database based on the clinical trial identifier and returns a list of ids of unique trials
dbFindVariable	| Find names of keys (fields) in the data base
dbFindUniqueEuctrRecord	| Select a single trial record when there are records for different EU Member States for this trial
dbGetVariablesIntoDf	| Create a data frame from records in the data base that have specified fields in the data base
dfMergeTwoVariablesRelevel	| Merge related variables into a single variable, and optionally map values to a new set of values.
installMongoCheckVersion	| Check the version of the build of the mongo server to be used
installMongoFindBinaries	| Convenience function to find location of mongo database binaries (mongo, mongoimport)
installCygwinWindowsDoInstall	| Convenience function to install a cygwin environment under MS Windows, including perl and php
installCygwinWindowsTest	| Convenience function to test for working cygwin installation


## Examples

### Download trial information and tabulate trial status

* Attach package `ctrdata`: 
```R
library(ctrdata)
```

* Open register's advanced search page in browser: 
```R
ctrOpenSearchPagesInBrowser()
# please review and respect register copyrights
ctrOpenSearchPagesInBrowser(copyright = TRUE)
```

* Click search parameters and execute search in browser 

* Copy address from browser address bar to clipboard

* Get address from clipboard: 
```R
q <- ctrGetQueryUrlFromBrowser()
# Found search query from EUCTR.
# [1] "cancer&age=children&status=ongoing"
```

* Retrieve protocol-related information, transform, save to database:
```R
ctrLoadQueryIntoDb(q)
# if no parameters are given for a database connection: uses mongodb
# on localhost, port 27017, database "users", collection "ctrdata"
# note: when run for first time, may download variety.js
#
# show which queries have been imported into the database so far
ctrQueryHistoryInDb()
# Total number of records: 1553
# Number of queries in history: 2
#       query-timestamp query-register query-records                           query-term
# 1 2015-10-08-23-06-34          CTGOV            26 ependymoma&recr=Open&type=Intr&age=0
# 2 2015-10-08-23-02-51          EUCTR            24            ependymoma&status=ongoing
```

* Find names of fields of interest in database:
```R
dbFindVariable("date")
# Returning first of 5 keys found.
# [1] "firstreceived_date"
#
dbFindVariable("number_of_subjects", allmatches = TRUE)
# [1] "f1151_number_of_subjects_for_this_age_range" "f1161_number_of_subjects_for_this_age_range" 
# [3] "f11_number_of_subjects_for_this_age_range" "f1141_number_of_subjects_for_this_age_range" 
# [5] "f121_number_of_subjects_for_this_age_range"  "f1111_number_of_subjects_for_this_age_range"
# [7] "f1121_number_of_subjects_for_this_age_range" "f1131_number_of_subjects_for_this_age_range" 
# [9] "f131_number_of_subjects_for_this_age_range" 
```

* Analyse some clinical trial information:
```R
# get all records that have values in all specified fields
result <- dbGetVariablesIntoDf(c("b31_and_b32_status_of_the_sponsor", "x5_trial_status"))
table (result$x5_trial_status)
#  Completed   Not Authorised   Ongoing   Prematurely Ended   Restarted   Temporarily Halted 
#         95                4        96                  17           4                  3 
```

### Deduplicate country-specific records of a trial, visualise trial information

```R
# Relation between number of study participants in one country and those in whole trial? 
result <- dbGetVariablesIntoDf(c("f41_in_the_member_state", "f422_in_the_whole_clinical_trial"))
plot(f41_in_the_member_state ~ f422_in_the_whole_clinical_trial, result)
#
# how many clinical trials are ongoing or completed, per country? (see also other example below) 
result <- dbGetVariablesIntoDf(c("a1_member_state_concerned", "x5_trial_status"))
table(result$a1_member_state_concerned, result$x5_trial_status)
#
# how many clinical trials where started in which year? 
result <- dbGetVariablesIntoDf(c("a1_member_state_concerned", "n_date_of_competent_authority_decision", 
                                 "a2_eudract_number"))
# to eliminate trials records duplicated by member state: 
result <- dbFindUniqueEuctrRecord(result)
# visualise 
result$startdate <- strptime(result$n_date_of_competent_authority_decision, "%Y-%m-%d")
hist(result$startdate, breaks = "years", freq = TRUE, las = 1); box()
```
![Histogram][1]

### Download from another register, check for duplicates, merge variables and re-organise values 

```R
# get data from another register
ctrLoadQueryIntoDb(queryterm = "ependymoma&recr=Open&type=Intr&age=0", register = "CTGOV")
#
# data from which queries were downloaded into the database? 
ctrQueryHistoryInDb()
# 
# get columns from the database from different registers
# this takes some time because variables are merged sequentially
dbFindVariable("status", allmatches = TRUE)
result <- dbGetVariablesIntoDf(c("overall_status", "x5_trial_status", "a2_eudract_number"))
#
# find ids of unique trials and subset the result set to these unique trials
ids_of_unique_trials <- dbFindIdsUniqueTrials()
result <- subset (result, subset = `_id` %in% ids_of_unique_trials)
result <- dbFindUniqueEuctrRecord(result)
#
# now condense two variables into a new one for analysis
tmp <- dfMergeTwoVariablesRelevel(result, c("overall_status", "x5_trial_status"))
table(tmp)
#
# condense two variables and in addition, condense their values into new value
statusvalues <- list("ongoing" = c("Recruiting", "Active", "Ongoing", "Active, not recruiting", 
                                   "Enrolling by invitation", "Restarted"),
                    "completed" = c("Completed", "Prematurely Ended", "Terminated"),
                    "other"     = c("Withdrawn", "Suspended", "No longer available", 
                                    "Not yet recruiting"))
tmp <- dfMergeTwoVariablesRelevel(result, c("overall_status", "x5_trial_status"), statusvalues)
table(tmp)
# completed   ongoing     other 
#      1059       671       115
```

### Use another mongo library for analysis, for example count sites per trial

```R
library(mongolite)
#
m <- mongo(db = "users", collection = "ctrdata")
# 
# check if there are any duplicates
ids_of_unique_trials <- dbFindIdsUniqueTrials()
#
# find the elements that represent a site
out <- m$find('{}', '{"location.facility.name": 1}')
#
# if there were duplicates, the unique trials could be retained like this
out <- subset (out, subset = `_id` %in% ids_of_unique_trials)
#
# helper function to count elements, 
# whether these are an array or a set
# of subdocuments in the data base 
count.elements <- function (dataframecolumn) {
   return(sapply(dataframecolumn, 
                 function(x) ifelse (is.data.frame(tmp <- unlist (x[[1]])), 
                                     nrow(tmp), length(tmp))))
}
#
# sum up number of sites per trial
out$number_of_sites <- count.elements (out$location)
# for many trials, no locations are specified
out <- subset (out, subset=number_of_sites >= 1)
#
# draw histogram
hist (out$number_of_sites)
hist (log(out$number_of_sites))
#
```

### Use aggregation functions of the data base to find endpoints

```R
library(mongolite)
#
m <- mongo(db = "users", collection = "ctrdata")
#
# number of all entries
m$count()
# number of ctgov records using json for query:
m$count('{"_id": {"$regex": "NCT[0-9]{8}"}}')
#
# count number of records in which certain terms occur,
# in any of the elements of the array in primary_outcome
#
# regular expressions are used (after "$regex"), case insensitive ("i")
#
# recommendation: to best define regular expressions for analyses, 
# inspect field primary_outcome.measure in data base, or print:
m$distinct("primary_outcome.measure", query = '{"_id": {"$regex": "NCT[0-9]{8}"}}')
#
# OS
m$aggregate('[{"$match": {"primary_outcome.measure": 
                         {"$regex": "overall survival", 
                          "$options": "i"}}}, 
              {"$group": {"_id": "null", "count": {"$sum": 1}}}]')
# 
# PFS, EFS, RFS, DFS
m$aggregate('[{"$match": {"primary_outcome.measure": 
                         {"$regex": "(progression|event|relapse|recurrence|disease)[- ]free", 
                          "$options": "i"}}}, 
              {"$group": {"_id": "null", "count": {"$sum": 1}}}]')
#
#
# now by year (in the future may be integrated into a mapreduce operation):
# 
# OS by year (firstreceived_date)
out <- m$aggregate('[{"$match": {"primary_outcome.measure": 
                                {"$regex": "overall survival", 
                                 "$options": "i"}}}, 
                     {"$project": {"_id": 1, "firstreceived_date": 1}}]')
# simple extraction of year from firstreceived_date such as "August 29, 2009"
out$year <- substr (out$firstreceived_date, tmp <- nchar(out$firstreceived_date) - 4, tmp + 4)
table (out$year)
# 2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015 
#    8     5     4    13     3     8     3     8     4     6     6 
#
# PFS, EFS, RFS, DFS by year (firstreceived_date)
out <- m$aggregate('[{"$match": {"primary_outcome.measure": 
                                {"$regex": "(progression|event|relapse|recurrence|disease)[- ]free", 
                                 "$options": "i"}}}, 
                     {"$project": {"_id": 1, "firstreceived_date": 1}}]')
# simple extraction of year from firstreceived_date such as "August 29, 2009"
out$year <- substr (out$firstreceived_date, tmp <- nchar(out$firstreceived_date) - 4, tmp + 4)
table (out$year)
# 2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015 
#   13     6     7    11    16    17    17    19    25    26    17
```


## In the works - next steps
 
* An efficient, differential update mechanism will be finalised and provided, using the RSS feeds that the registers provide after executing a query. 

* More examples for analyses will be provided in a separate document, with special functions to support analysing time trends of numbers and features of clinical trials. 


## Acknowledgements 

* Data providers and curators of the clinical trial registers. Please review and respect their copyrights and terms and conditions (`ctrOpenSearchPagesInBrowser(copyright = TRUE)`). 

* This package `ctrdata` has been made possible based on the work done for [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [rmongodb](https://github.com/mongosoup/rmongodb) and of course for [R](http://www.r-project.org/). 

* [Variety](https://github.com/variety/variety), a Schema Analyzer for MongoDB

* Contributors to community documentation


## Notes

* By design, each record from EUCTR when using `details = TRUE` (the default) represents information on the trial concerning the respective member state. This is necessary for some analyses, but not for others. 

* So far, no attempts are made to harmonise and map field names between different registers, but the function `dfMergeTwoVariablesRelevel()` can be used to manually merge and map two variables / fields. 

* So far, no efforts were made to type data base fields; they are all strings in the database. 

* Package `ctrdata` is expected to work on Linux, Mac OS X and MS Windows systems, if installation requirements (above) are met.  

* Package `ctrdata` also uses [Variety](https://github.com/variety/variety), which will automatically be downloaded into the package's `exec` directory when first using a function that needs it. This may fail if this directory is not writable for the user and this issue is not yet addressed. Note that `variety.js` may work well with remote mongo databases, see documentation of `dbFindVariable()`. 

* In case `curl` fails with an SSL error, run this code to update the certificates in the root of package `curl`:
```R
httr::GET("https://github.com/bagder/ca-bundle/raw/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt", write_disk(system.file("", package = "curl"), "inst/cacert.pem", overwrite = TRUE))
```

[1]: ./Rplot01.png "Number of trials authorised to start, by year"
