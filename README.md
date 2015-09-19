# README.md for R package ctrdata on github.com

## Aims

The main aim of `ctrdata` is to provide functions for querying and retrieving information on clinical trials from two public registers, the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/) and ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/). The package addresses the wish for aggregating and trend-analysing information on clinical trials, particularly in the field of oncology. The public registers at this time do not provide a programming interface and provide divergent, limited means for aggregating. Metaregisters such as offered by the WHO ("ICTRP", http://apps.who.int/trialsearch/) do not hold latest information. By choice and for the author's objectives, the EUCTR is the main target of this package. Development started mid 2015, first push to github.com mid September 2015. 

Key features:

* Protocol-related information on clinical trials is retrieved from online sources, transformed and stored in a document-centric database (mongo). 

* Fast and offline access to detailed information on clinical trials. (Records will be updated when the query to retrieve the information is repeated). 

* Utility functions are included to facilitate defining queries of the registers by using their respective browser-based user interface. 

* Information is also provided for visualising the information stored on the clinical trials.

* Efforts are made to deduplicate clinical trial records when retrieved from more than one register. 
  
This package `ctrdata` has been made possible based on the work done for [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [rmongodb](https://github.com/mongosoup/rmongodb) and of course for [R](http://www.r-project.org/). 

## Installation

Within R, use the following commands to get and install the current development version of package `ctrdata` from github.com:

```R
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")
```

Other requirements:

* A local [mongodb](https://www.mongodb.org/) installation. From this installation, `mongoimport` is required. The database itself may be in a locally running MongoDB server or remotely accessible [e.g. mongolab](https://mongolab.com/). 

Additional requirements on MS Windows:

* An installation of [cygwin](https://cygwin.com/install.html). 

In R, simply run `ctrdata::installCygwin()` for an automated installation. 

For manual instalation, cygwin can be installed without administrator credentials as explained [here](https://cygwin.com/faq/faq.html#faq.setup.noroot). In the graphical interface of the cygwin installer, type `perl` in the ``Select packages` field and click on `Perl () Default` so that this changes to `Perl () Install`, as also shown [here](http://slu.livejournal.com/17395.html). 

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
# is there a relation between the number of study participants in a country and those in whole trial? 
result <- dbCTRGet(c("f41_in_the_member_state", "f422_in_the_whole_clinical_trial"))
plot (f41_in_the_member_state ~ f422_in_the_whole_clinical_trial, result)
```
```R
# how many clinical trials are ongoing or completed, per country? 
result <- dbCTRGet(c("a1_member_state_concerned", "x5_trial_status"))
table (result$a1_member_state_concerned, result$x5_trial_status)
```
```R
# how many clinical trials where started in which year? 
result <- dbCTRGet(c("a1_member_state_concerned", "n_date_of_competent_authority_decision"))
result$startdate <- strptime(result$n_date_of_competent_authority_decision, "%Y-%m-%d")
hist (result$startdate, breaks = "years", freq = TRUE, xlab = q)
```

* Retrieve additional trials from another register and check for duplicates:
```R
getCTRdata(queryterm = "cancer&recr=Open&type=Intr&age=0", register = "CTGOV")
uniquetrialsCTRdata()
```

* More examples and analysis functions to come. Further suggestions: inspect database contents for example using [Robomongo](http://www.robomongo.org) or analyse time trends of numbers and features of clinical trials. 


## Acknowledgements 

* Data providers and curators of the clinical trial registers

* Contributors to community documentation

* [Variety](https://github.com/variety/variety), a Schema Analyzer for MongoDB

## Issues

* By design, each record from EUCTR when using `details = TRUE` (the default) represents information on the trial concerning the respective member state. This is necessary for some analyses, but not for others. 

* So far, no attempts are made to harmonise and map field names between different registers, such as by using standardised identifiers. 

* So far, no efforts were made to type data base fields; they are all strings (`2L` in mongo). 

* Package `ctrdata` is expected to work on Linux, Mac OS X and MS Windows systems, if requirements (above) are met.  

* Package `ctrdata` also uses [Variety](https://github.com/variety/variety). Its file `variety.js` is will automatically be downloaded into the package's `exec` directory when first using the function that needs it. This may however fail if this directory is not writable for the user and this issue is not yet addressed.

* In case `curl` fails with an SSL error, run this code to update the certificates in the root of package `curl`:
```R
httr::GET("https://github.com/bagder/ca-bundle/raw/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt", write_disk(system.file("", package = "curl"), "inst/cacert.pem", overwrite = TRUE))
```
