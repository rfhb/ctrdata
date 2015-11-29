---
output: 
  html_document: 
    number_sections: no
    toc: yes
---

# Examples for using R package `ctrdata` for clinical trial protocol-related information

Last edit 2015-11-29. General information on the library is available here: [https://github.com/rfhb/ctrdata](https://github.com/rfhb/ctrdata). 

## Find fields / variables of interest

```R
dbFindVariable("number_of_subjects", allmatches = TRUE)
#
# [1] "f1151_number_of_subjects_for_this_age_range" "f1161_number_of_subjects_for_this_age_range" 
# [3] "f11_number_of_subjects_for_this_age_range" "f1141_number_of_subjects_for_this_age_range" 
# [5] "f121_number_of_subjects_for_this_age_range"  "f1111_number_of_subjects_for_this_age_range"
# [7] "f1121_number_of_subjects_for_this_age_range" "f1131_number_of_subjects_for_this_age_range" 
# [9] "f131_number_of_subjects_for_this_age_range" 
#
```

## Update data base from query in specified register

Note the update functionality will be further improved. 

```R
# List queries 
#
ctrQueryHistoryInDb()
# 
# Update query number 1 in the history (re-downloading data)
#
ctrLoadQueryIntoDb(querytoupdate = 1) 
#
```


## Deduplicate country-specific records of a trial, visualise trial information

Note EUCTR provides country-specific records for trials, see README.md. 

```R
#
# Relation between number of study participants in one country and in whole trial? 
#
result <- dbGetVariablesIntoDf(c("f41_in_the_member_state", "f422_in_the_whole_clinical_trial"))
#
plot(f41_in_the_member_state ~ f422_in_the_whole_clinical_trial, result)
#
# How many clinical trials are ongoing or completed, per country?
#
result <- dbGetVariablesIntoDf(c("a1_member_state_concerned", "x5_trial_status"))
#
table(result$a1_member_state_concerned, result$x5_trial_status)
#
# How many clinical trials where started in which year?
#
result <- dbGetVariablesIntoDf(c("a1_member_state_concerned", "n_date_of_competent_authority_decision", 
                                 "a2_eudract_number"))
#
# Eliminate trials records duplicated by member state: 
#
result <- dbFindUniqueEuctrRecord(result)
#
# Visualise 
#
result$startdate <- strptime(result$n_date_of_competent_authority_decision, "%Y-%m-%d")
hist(result$startdate, breaks = "years", freq = TRUE, las = 1); box()
#
```
![Histogram][1]


## Download from another register, check for duplicates, merge variables and re-organise values 

```R
#
# Get data from another register
#
ctrLoadQueryIntoDb(queryterm = "ependymoma&recr=Open&type=Intr&age=0", register = "CTGOV")
#
# Data from which queries were downloaded into the database? 
#
ctrQueryHistoryInDb()
# 
# Get columns from the database from different registers.
# This takes some time because variables are merged sequentially.
#
dbFindVariable("status", allmatches = TRUE)
result <- dbGetVariablesIntoDf(c("overall_status", "x5_trial_status", "a2_eudract_number"))
#
# Find ids of unique trials and subset the result set to these unique trials
#
ids_of_unique_trials <- dbFindIdsUniqueTrials()
#
# Subset the result set to these unique trials
#
result <- subset (result, subset = `_id` %in% ids_of_unique_trials)
result <- dbFindUniqueEuctrRecord(result)
#
# Now merge two variables into a new one for analysis
#
tmp <- dfMergeTwoVariablesRelevel(result, c("overall_status", "x5_trial_status"))
#
table(tmp)
#
# Merge two variables and in addition, condense their values into a new value
#
statusvalues <- list("ongoing" = c("Recruiting", "Active", "Ongoing", "Active, not recruiting", 
                                   "Enrolling by invitation", "Restarted"),
                    "completed" = c("Completed", "Prematurely Ended", "Terminated"),
                    "other"     = c("Withdrawn", "Suspended", "No longer available", 
                                    "Not yet recruiting"))
#
tmp <- dfMergeTwoVariablesRelevel(result, c("overall_status", "x5_trial_status"), statusvalues)
#
table(tmp)
#
# completed   ongoing     other 
#      1059       671       115
#
```

## Use another mongo package for analysis, for example to count sites per trial

Note: Packages to access mango such as `mongolite` or `RMongo` may work to access data that are already in the dababase. However, package `rmongodb` is still necessary to retrieve data from registers and to store these data into the database. 

```R
#
library(mongolite)
#
m <- mongo(db = "users", collection = "ctrdata")
# 
# Check if there are any duplicates
#
ids_of_unique_trials <- dbFindIdsUniqueTrials()
#
# Find the database elements that represent a site
#
out <- m$find('{}', '{"location.facility.name": 1}')
#
# If there were duplicates, the unique trials could be retained like this
#
out <- subset (out, subset = `_id` %in% ids_of_unique_trials)
#
# Define a helper function to count elements, irrespective of whether
# the elements are in an array or are a set of subdocuments in the database
#
count.elements <- function (dataframecolumn) {
   return(sapply(dataframecolumn, 
                 function(x) ifelse (is.data.frame(tmp <- unlist (x[[1]])), 
                                     nrow(tmp), length(tmp))))
}
#
# Sum up number of sites per trial
#
out$number_of_sites <- count.elements (out$location)
#
# For many trials, no locations seem to be specified
#
out <- subset (out, subset=number_of_sites >= 1)
#
# Draw histogram
#
hist (out$number_of_sites)
#
```

## Use aggregation functions of the data base to find specific trial endpoints

```R
#
library(mongolite)
#
m <- mongo(db = "users", collection = "ctrdata")
#
# Number of all records
#
m$count()
#
# Number of ctgov records using json for query
#
m$count('{"_id": {"$regex": "NCT[0-9]{8}"}}')
#
# The following uses the aggregation pipeline in mongo.
#
# Count number of records in which certain terms occur,
# in any of the elements of the array in primary_outcome. 
# Regular expressions ("$regex") are case insensitive ("i")
#
# Recommendation: to best define regular expressions for analyses, 
# inspect the field primary_outcome.measure in data base, or print:
#
m$distinct("primary_outcome.measure", query = '{"_id": {"$regex": "NCT[0-9]{8}"}}')
#
# OS
#
m$aggregate('[{"$match": {"primary_outcome.measure": 
                         {"$regex": "overall survival", 
                          "$options": "i"}}}, 
              {"$group": {"_id": "null", "count": {"$sum": 1}}}]')
#
# PFS, EFS, RFS or DFS
#
m$aggregate('[{"$match": {"primary_outcome.measure": 
                         {"$regex": "(progression|event|relapse|recurrence|disease)[- ]free", 
                          "$options": "i"}}}, 
              {"$group": {"_id": "null", "count": {"$sum": 1}}}]')
#
#
# Now by year (see details on [mongo's aggregation pipleline](https://docs.mongodb.org/manual/core/aggregation-pipeline/) )
# 
# OS by year
#
out <- m$aggregate('[{"$match": {"primary_outcome.measure": 
                                {"$regex": "overall survival", 
                                 "$options": "i"}}}, 
                     {"$project": {"_id": 1, "firstreceived_date": 1}}]')
#                     
# Extraction of year from firstreceived_date such as "August 29, 2009"
#
out$year <- substr (out$firstreceived_date, tmp <- nchar(out$firstreceived_date) - 4, tmp + 4)
#
table (out$year)
#
# 2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015 
#    8     5     4    13     3     8     3     8     4     6     6 
#
# PFS, EFS, RFS, DFS by year (firstreceived_date)
#
out <- m$aggregate('[{"$match": {"primary_outcome.measure": 
                                {"$regex": "(progression|event|relapse|recurrence|disease)[- ]free", 
                                 "$options": "i"}}}, 
                     {"$project": {"_id": 1, "firstreceived_date": 1}}]')
#
# Extraction of year from firstreceived_date such as "August 29, 2009"
#
out$year <- substr (out$firstreceived_date, tmp <- nchar(out$firstreceived_date) - 4, tmp + 4)
#
table (out$year)
#
# 2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015 
#   13     6     7    11    16    17    17    19    25    26    17
#
```

## Using mapreduce operations to analyse participant numbers

Note that the mongodb documentation includes that a mapreduce operation might not be as fast and efficient as using the aggregation pipeline, which was used in the preceding example. 

```R
#
library(mongolite)
#
m <- mongo(db = "users", collection = "ctrdata")
#
# Count number of trials (trial records) with number of study participants 
# in bins of hundreds of participants
#
hist <- m$mapreduce(
#
  map = "function(){emit(Math.floor(this.f422_in_the_whole_clinical_trial/100)*100, 1)}", 
  #
  reduce = "function(id, counts){return Array.sum(counts)}"
  #
)
#
hist
#
plot (hist, type = "h", las = 1)
#
```

[1]: ./Rplot01.png "Number of trials authorised to start, by year"

