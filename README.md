---
output: 
  html_document: 
    toc: yes
---
# README.md for R package ctrdata on github.com

## Aims

The aims of `ctrdata` are to provide functions for retrieving, aggregating and analysing information on clinical trials from public registers, primarily the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/) and also ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/). The registers are not known to provide an application programming interface (API) and have limited aggregation options. Development of `ctrdata` started mid 2015, first push to github.com mid September 2015, last edit 2015-09-20. 

Key features implemented:

* Protocol-related information on clinical trials is retrieved from public online sources, from queries defined by the user. 

* Utility functions are included for defining queries in the registers' browser-based user interfaces. 

* Retrieved information is transformed and stored in a document-centric database (mongo) for further access. 

* Fast and offline access to detailed information on clinical trials, for use with `R`. 

* Unique (de-duplicated) clinical trial records are identified, as the database may well hold information from more than one register. 
  
* Records in a database will be updated by a simple query command. 

* In the background, `exec/euctr2json.sh` is a special script that transforms EUCTR plain text files to json format. 

This package `ctrdata` has been made possible based on the work done for [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [rmongodb](https://github.com/mongosoup/rmongodb) and of course for [R](http://www.r-project.org/). 


## Installation

Within R, use the following commands to get and install the current development version of package `ctrdata` from github.com:

```R
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")
```

Other requirements:

* A local [mongodb](https://www.mongodb.org/) installation. From this installation, `mongoimport` and `mongo` required. The database itself may be in a locally running MongoDB server or remotely accessible [e.g. mongolab](https://mongolab.com/). 

Additional requirements on MS Windows:

* An installation of [cygwin](https://cygwin.com/install.html). 

In R, simply run `ctrdata::installCygwin()` for an automated installation. 

For manual instalation, cygwin can be installed without administrator credentials as explained [here](https://cygwin.com/faq/faq.html#faq.setup.noroot). In the graphical interface of the cygwin installer, type `perl` in the `Select packages` field and click on `Perl () Default` so that this changes to `Perl () Install`, as also shown [here](http://slu.livejournal.com/17395.html). 

Note that Rtools is *not* required for `ctrdata` on MS Windows. 


## Example workflow

* Attach package `ctrdata`: 
```R
library(ctrdata)
```

* Open register's advanced search page in browser: 
```R
openCTRWebBrowser()
# please review and respect register copyrights
openCTRWebBrowser(copyright = TRUE)
```

* Click search parameters and execute search in browser 

* Copy address from browser address bar to clipboard

* Get address from clipboard: 
```R
q <- getCTRQueryUrl()
# Found search query from EUCTR.
# [1] "cancer&age=children&status=completed"
```

* Retrieve protocol-related information, transform, save to database:
```R
getCTRdata(q)
# if no parameters are given for a database connection: uses mongodb
# on localhost, port 27017, database "users", collection "ctrdata"
```

* Find names of fields of interest in database:
```R
findCTRkey("sites")   # note: when run for first time, will download variety.js
findCTRkey("n_date")
findCTRkey("number_of_subjects", allmatches = TRUE)
findCTRkey("time", allmatches = TRUE)
```


* Visualise some clinical trial information:
```R
# get certain fields for all records
result <- dbCTRGet (c("_id", "x5_trial_status"))
table (result$x5_trial_status)
#  Completed   Not Authorised   Ongoing   Prematurely Ended   Restarted   Temporarily Halted 
#         95                4        96                  17           4                  3 
```
```R
# Relation between number of study participants in one country and those in whole trial? 
result <- dbCTRGet(c("f41_in_the_member_state", "f422_in_the_whole_clinical_trial"))
plot(f41_in_the_member_state ~ f422_in_the_whole_clinical_trial, result)
```
```R
# how many clinical trials are ongoing or completed, per country? 
result <- dbCTRGet(c("a1_member_state_concerned", "x5_trial_status"))
table(result$a1_member_state_concerned, result$x5_trial_status)
```
```R
# how many clinical trials where started in which year? 
result <- dbCTRGet(c("a1_member_state_concerned", "n_date_of_competent_authority_decision"))
result$startdate <- strptime(result$n_date_of_competent_authority_decision, "%Y-%m-%d")
hist(result$startdate, breaks = "years", freq = TRUE, las = 1); box()
```
![Histogram][1]

* Retrieve additional trials from another register and check for duplicates:
```R
getCTRdata(queryterm = "cancer&recr=Open&type=Intr&age=0", register = "CTGOV")
uniquetrialsCTRdata()
```


## In the works - next steps
 
* An efficient, differential update mechanism will be finalised and provided, using the RSS feeds that the registers provide after executing a query. 

* A function to get into one column similar information (e.g., number of trial participants) from different registers (which use different fieldnames) in the mongo database. 

* More examples for analyses will be provided, with special functions to support analysing time trends of numbers and features of clinical trials. 

* Have a look at the database contents for example using [Robomongo](http://www.robomongo.org). 


## Acknowledgements 

* Data providers and curators of the clinical trial registers

* Contributors to community documentation

* [Variety](https://github.com/variety/variety), a Schema Analyzer for MongoDB


## Issues

* Query terms are not checked or parsed within `ctrdata`; this is not a priority because queries are conveniently composed in a web brower. 

* By design, each record from EUCTR when using `details = TRUE` (the default) represents information on the trial concerning the respective member state. This is necessary for some analyses, but not for others. 

* So far, no attempts are made to harmonise and map field names between different registers, such as by using standardised identifiers. 

* So far, no efforts were made to type data base fields; they are all strings (`2L` in mongo). 

* Package `ctrdata` is expected to work on Linux, Mac OS X and MS Windows systems, if installation requirements (above) are met.  

* Package `ctrdata` also uses [Variety](https://github.com/variety/variety). In fact, `variety.js` will automatically be downloaded into the package's `exec` directory when first using the function that needs it. This may fail if this directory is not writable for the user and this issue is not yet addressed. Note that `variety.js` may not work well with remote mongo databases, see documentation of `findCTRkeys()`. 

* In case `curl` fails with an SSL error, run this code to update the certificates in the root of package `curl`:
```R
httr::GET("https://github.com/bagder/ca-bundle/raw/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt", write_disk(system.file("", package = "curl"), "inst/cacert.pem", overwrite = TRUE))
```

[1]: ./Rplot.svg "Number of trials authorised to start, by year"
