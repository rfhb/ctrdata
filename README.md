# README.md for ctrdata package on github.com

## Aims

The main aim of `ctrdata` is to provide functions for querying and retrieving information on clinical trials from two public registers, the European Union Clinical Trials Register ("EUCTR", https://www.clinicaltrialsregister.eu/) and ClinicalTrials.gov ("CTGOV", https://clinicaltrials.gov/).  The package is motivated by the wish for aggregating and trend analysing information on clinical trials, particularly in the field of oncology. The public registers at this time do not provide a programming interface and divergent, limited means for aggregating. Metaregisters such as offered by the WHO ("ICTRP", http://apps.who.int/trialsearch/) do not hold latest information. By choice and for the author's objectives, the EUCTR is the main target of this package. 

Warnings to manage expectations: 

* Does not yet work on MS Windows, see Issues below. 

* So far, no attempts are made to harmonise and map field names between different registers, such as by using standardised identifiers. 

* So far, no efforts were made to type variables; they are all strings (`2L` in mongo). 

Key features:

* Protocol-related information on clinical trials is retrieved from online sources, transformed and stored in a document-centric database (mongo). 

* Fast and offline access to detailed information on clinical trials. (Records will be updated when the query to retrieve the information is repeated). 

* Utility functions are included to facilitate defining queries of the registers by using their respective browser-based user interface. 

* Information also provided for visualising the stored information on the clinical trials.

* Efforts are made to deduplicate clinical trial records when retrieved from more than one register. 
  
This package `ctrdata` has been made possible based on the work done for [RCurl](http://www.omegahat.org/RCurl/), [curl](https://github.com/jeroenooms/curl), [rmongodb](https://github.com/mongosoup/rmongodb) and of course for R itself [R](http://www.r-project.org/).

## Installation

Within R, use the following comments to get and install the current development version of package `ctrdata` from github.com:

```R
install.packages("devtools")
devtools::install_github("rfhb/ctrdata")
```

Other requirements: 

* installation of [perl](https://www.perl.org/get.html)

* installation of [sed](http://www.gnu.org/software/sed/)

* installation of [mongodb](https://www.mongodb.org/)

The executables should be on the path.

## Example workflow

1. Attach package `ctrdata` 
```R
library(ctrdata)
```

2. Open register's advanced search page, click search parameters and execute search in browser 
```R
openCTRWebBrowser()
```

3. Click search parameters and execute search in browser 

4. Copy address from browser address bar to clipboard

5. Get address from clipboard
```R
q <- getCTRQueryUrl()
```

6. Retrieve protocol-related information, transform, save to database
```R
getCTRdata(q)
```

7. Find names of fields of interest in database
```R
findCTRkey("sites")   # note: when run for first time, will download and install variety.js
findCTRkey("n_date")
findCTRkey("f11_number_of_subjects")
findCTRkey("subjects", allmatches = TRUE)
```

8. Visualise some clinical trial information
```R
# get certain fields for all records
result <- dbCTRGet (c("_id", "trial_status"))
table (result$trial_status)
```
```R
# is there a relation between the number of study participants in a country and those in whole trial? 
result <- dbCTRGet(c("a1_member_state_concerned", "f41_in_the_member_state", "f422_in_the_whole_clinical_trial"))
plot (f41_in_the_member_state ~ f422_in_the_whole_clinical_trial, result)
```
```R
# how many clinical trials are ongoing or completed, per country? 
result <- dbCTRGet(c("a1_member_state_concerned", "trial_status"))
table (result$a1_member_state_concerned, result$trial_status)
```
```R
# how many clinical trials where started in which year? 
result <- dbCTRGet(c("a1_member_state_concerned", "n_date_of_competent_authority_decision"))
result$startdate <- strptime(result$n_date_of_competent_authority_decision, "%Y-%m-%d")
hist (result$startdate, breaks = "years")
```

9. Retrieve additional trials from other register, deduplicate and combine for analysis
```R
# 
```

10. Do more - suggestions

* Inspect database contents for example using [Robomongo](http://www.robomongo.org)

* Analyse time trends of numbers and features of clinical trials


## Acknowledgements 

* Many thanks to those contributing to community documentation

* [Variety](https://github.com/variety/variety), a Schema Analyzer for MongoDB

## Issues

* Package `ctrdata` currently is expected to work on linux and Mac OS X system, but one of its main functions is not yet working on MS Windows because an executable script (`euctr2json.sh`) in the package has not yet been ported to this system (2015-09-13). This will be addressed soon.  

* Package `ctrdata` also uses [Variety](https://github.com/variety/variety). Its file `variety.js` is will automatically be downloaded into the package's `exec` directory when first using the function that needs it. This may however fail if this directory is not writable for the user and this issue is not yet addressed.

* In case `curl` fails with an SSL error, run this code to update the certificates in root of package `curl`:
```R
httr::GET("https://github.com/bagder/ca-bundle/raw/e9175fec5d0c4d42de24ed6d84a06d504d5e5a09/ca-bundle.crt", write_disk(system.file("", package = "curl"), inst/cacert.pem overwrite = TRUE))
```
