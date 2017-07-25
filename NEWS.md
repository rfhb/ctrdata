# ctrdata 0.10.0
 - 2017-07-25
 - fully load results from ctgov
 - prepare loading results from euctr
 
# ctrdata 0.9.14
 - 2017-06-28
 - refactored system calls
 - windows now part of continuous integration
 
# ctrdata 0.9.13
 - 2017-06-23
 - refactored ctrLoadQueryIntoDb
 
# ctrdata 0.9.12
 - 2017-06-18
 - Preparing for new CTGOV interface
 - Improved documentation
 - Corrected ctrGetQueryUrlFromBrowser return value

# ctrdata 0.9.11.1
 - 2017-02-04
 - Improved documentation

# ctrdata 0.9.11
 - 2017-01-15
 - Added functionality: EUCTR fallback import mechanism if large JSON file fails to import into mongoDB (by splitting and importing one JSON file for each trial, tested with several thousand trials)

# ctrdata 0.9.10.1
 - 2017-01-12
 - Fixes issues with conversion of EUCTR records that did not have details.
 - Fixes issue that placebo information was converted into IMP fields. 
 
# ctrdata 0.9.10.0
 - 2016-12-28
 - Added metadata attributes to returned objects to indicate database, query, timestamp etc.
 
# ctrdata 0.9.9.5
 - 2016-12-14
 - Added option ctrLoadQueryIntoDb(querytoupdate = "last") to re-download last query in collection
 
# ctrdata 0.9.9.4
 - 2016-11-18
 - Added progress indicator to ctrLoadQueryIntoDb() to indicate network download traffic
 
# ctrdata 0.9.9.3
 - 2016-11-17
 - deduplication in dbFindIdsUniqueTrials() optimised for speed and memory, added check by ISRCTN

# ctrdata 0.9.9.2
 - 2016-11-13
 - corrected deduplication in dbFindIdsUniqueTrials()

# ctrdata 0.9.9.1
 - 2016-11-12
 - renamed ctrQueryHistoryInDb() to dbQueryHistory()
 - note: change in json format of query history, breaking compatibility
 - refactored all concerned functions to use mongolite
 - rmongodb is no more supported

# ctrdata 0.9
 - 2016-10-17
 - changed implementation of dbFindIdsUniqueTrials()
 - editorial changes to examples

# ctrdata 0.8.1
 - 2016-09-07
 - added field to indicate source register
 - improved ctrLoadQueryIntoDb() with details = FALSE
 - added example for map plotting

# ctrdata 0.8
 - 2016-09-04
 - dbFindIdsUniqueTrials now encapsulates dfFindIdsUniqueEuctrRecord
 - dfFindIdsUniqueEuctrRecords removed
 - installation instructions updated after recently rmongodb was removed from CRAN

# ctrdata 0.7
 - 2016-05-29
 - dbGetVariablesIntoDf changed to concatenate values in array and objects 
 - completed test adaptation for travis
 - improving perl regex
 - checking helper applications

# ctrdata 0.6.2
 - 2016-04-20
 - add / update field "record_last_import" for every imported / updated record

# ctrdata 0.6.1
 - 2016-04-02
 - changed to provide vignettes

# ctrdata 0.6
 - 2016-02-25
 - different update mechanism for EUCTR implemented
 - corrected function name from db... to dfFindUniqueEuctrRecord()

# ctrdata 0.5.9
 - 2016-01-23
 - Corrected bugs
 - Started preparation of submission to CRAN
 - Preparing to include package unit tests

# ctrdata 0.5
 - 2015-11-29
 - Published on github
 - Improved documentation

# ctrdata 0.4
 - 2015-10-08
 - Renamed all functions for consistency and ease-of-use

# ctrdata 0.3
 - 2015-10-06
 - Added functionality to download xml data from CTGOV, which includes more data than the csv format

# ctrdata 0.2.8
 - Changed and extended how history of queries is included in database.
 - New function dbCTRQueryHistory()

# ctrdata 0.2.7
 - Added function for merging variables such as from different registers and optionally to merge values into new values
 - Note that function findCTRkey was renamed to dbFindCTRkey because it acts on the database

# ctrdata 0.2.5
 - Added function for selecting preferred language versions of trials from EUCTR
 - Improved use of automatic proxy configuration script

# ctrdata 0.2.2
 - Added proxy function and improved installation of cygwin under MS Windows

# ctrdata 0.2
 - 2015-09-19
 - Now also working on MS Windows

# ctrdata 0.1
 - 2015-09-15
 - First version with basic functionality
 - Limited testing
 - Works on Mac OS X (10.10.x)
