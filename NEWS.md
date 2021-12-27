# ctrdata 1.8.0.9002
 - 2021-12-27
 - chunked trial batches in ndjson files for accelerated database import
 - if package dplyr is loaded, functions return a tibble instead of a data frame
 - dbFindFields() returns a vector of fields which now has as names the register in which a field occurs
 - accelerated binary checks (cygwin / Windows)
 - remove internet proxy mangling in order to use system configuration (e.g., transparent proxies used, or environment variable https_proxy specified by user)
 - refactored internal caching
 - correct dbGetFieldsIntoDf() for specific nested data structures
 - correct dfTrials2Long() for specific fields

# ctrdata 1.8.0.9001
 - 2021-12-11
 - thorough documentation improvement
 - simplified dbFindFields
 - cleaned up testing binaries
 - cleaned up heper scripts
 - removed ctrGetQueryUrlFromBrowser(), long deprecated
 
# ctrdata 1.8.0.9000
 - 2021-11-22
 - uses nodbi 0.6.0
 - can use PostgreSQL as backend
 - include PostgreSQL in Github Actions
 
# ctrdata 1.8.0
 - 2021-11-18
 - changes to match nodbi 0.5.0 
 - simplifying database operations (user-visible functions: 
   ctrLoadQueryIntoDb, dbFindIdsUniqueTrials, dbGetFieldsIntoDf), 
   without changes to API

# ctrdata 1.7.1.9000
 - 2021-08-23
 - new development version

# ctrdata 1.7.1
 - 2021-08-22
 - fix DBI not needed in Imports anymore (CRAN Note)
 - fix potential file name issue in conversion script
 - fix dbFindFields() to never return _id (previously depended on database backend)
 - changed tests (not on CRAN detection, register availability, additional tests)

# ctrdata 1.7.0
 - 2021-07-24
 - much reduced database backend-specific code, using nodbi 0.4.3 (released 2021-07-23)
   which also introduces transactions for sqlite using RSQLite >=2.2.4 (released 2021-03-12)
 - temporary directory creation only when needed, more automated deletion
 - changes in detecting non-functioning register servers
 - further streamlined unit testing

# ctrdata 1.6.0
 - 2021-05-09
 - added support for ISRCTN
 - refactored checking binaries and caching this info 
 - updated EUCTR download parameters
 - refactored ctrGetQueryUrl and ctrOpenSearchPagesInBrowser
 - harmonised error checking
 - avoid some errors with external scripts
 - refactored url / query mangling, added detailed testing
 - refactored storing JSON into database (handle big files, reduce memory)
 - improved dbFindIdsUniqueTrials (speed, memory, register coverage)
 - factored out conversion to JSON
 - accelerated EUCTR results and history download and storage
 - external scripts now create multiple chunks of records
 - use further identifier fields with dbFindIdsUniqueTrials

# ctrdata 1.5.3.9000
 - 2021-04-29
 - adding user info which field entries could not be typed

# ctrdata 1.5.3
 - 2021-04-19
 - include message how to handle server certificate issues,
   by propagating user settings for httr to curl operations
 - ensure identical return structures when no new trials found
 - dfTrials2Long: harmonise identifier level assignment, 
   address cases where field occurs only once in input df
 - dfMergeTwoVariablesRelevel: corrected and improved user info
 - dfName2Value: remove duplicate rows, e.g. from duplicated criteria
 
# ctrdata 1.5.2
 - 2021-04-05
 - bugfix of EOL for converting EUCTR files

# ctrdata 1.5.1
 - 2021-03-21
 - bugfix for non-matching euctr protocol and result ids: 
   any trials from EUCTR for which results were downloaded with 
   version 1.5.0 should be downloaded again (ctrLoadQueryIntoDb)
 - dfTrials2Long refactored and accelerated
 - API change: dfTrials2Long return value (identifier replaces main_id and sub_id) 
 - new option to save EUCTR results PDF files in user-specified directory
 
# ctrdata 1.5.0
 - 2021-03-14
 - return values of dbGetFieldsIntoDf are now mostly 
   identical whether using src_mongo or src_sqlite,
   to best ensure portability of analysis code
 - permit dots in queries / URLs
 - improved handling of queryterm
 - renamed ctrGetQueryUrlFromBrowser to ctrGetQueryUrl
 - soft deprecated ctrGetQueryUrlFromBrowser
 - ensure parallel retrievals from EUCTR
 - speed up routines in dbGetFieldsIntoDf
 - make dfTrials2Long handle NA better
 - improved documentation, clarified examples
 - simplified internals for typing fields, 
   start typing results fields

# ctrdata 1.4.1
 - 2020-11-03
 - reset row names on data frames returned by functions
 - update curl parameters when accessing EUCTR

# ctrdata 1.4
 - 2020-10-17
 - new: easy access to variables with
   dfTrials2Long() + dfName2Value()
 - improved dfMergeTwoVariablesRelevel() 
   to maintain type of data
 - revised and simplified vignettes
 - deprecated: dfListExtractKey()
 - refactored parts of euctr retrieval
 - notify user when euctr register server
   does not permit compression and how 
   long retrieval will take

# ctrdata 1.3.2.9000
 - 2020-10-08
 - fixed identifying unique ids

# ctrdata 1.3.2
 - 2020-10-03
 - quote system file paths

# ctrdata 1.3.1
 - 2020-08-01
 - fix error in CI tests

# ctrdata 1.3.0
 - 2020-07-27
 - workaround EUCTR certificate issue
 - streamline ctrGetQueryUrlFromBrowser()
 - better handling of complex fields
 - include further tests for query string handling,
   checking more parameters and return values
 - better clean-up after testing
 - ctrLoadQueryIntoDb(querytorerun = ...) now looks
   for the date when the querytorerun was last run, 
   to more often use euctr update options
 - switching from travis to github action
 - upped coverage of code tested
 
# ctrdata 1.2.1
 - 2020-05-18
 - tinytest >= 1.2.1 to avoid regression error
 - correct testing detail

# ctrdata 1.2
 - 2019-12-07
 - correct cygwin install detail

# ctrdata 1.1
 - 2019-11-12
 - release after nodbi 0.4 is available

# ctrdata 1.0.1.9005
 - 2019-11-09
 - update description for installation from github
 
# ctrdata 1.0.1.9004
 - 2019-11-04
 - handled mixed arrays and text values in same key of ctgov trial records
 - more user information during importing
 
# ctrdata 1.0.1.9003
 - 2019-11-04
 - further nesting added for euctr trial records
 - user verbose information extended for record importing
 
# ctrdata 1.0.1.9002
 - 2019-11-03
 - improved parsing of euctr trial records
 - correct re-opening of sqlite connection
 
# ctrdata 1.0.1
 - 2019-10-22
 - correction of testing

# ctrdata 1.0
 - 2019-10-16
 - switch to nodbi::scr_{mongo,sqlite}() with 
   re-implementation of most functions
 - switch from testthat to tinytest, so that users 
   can check with tinytest::test_package("ctrdata")
 - improvements to euctr trial import  
 - new function dfListExtractKey

# ctrdata 0.18.9005
 - 2019-05-02
 - speed up testing bash commands under windows

# ctrdata 0.18.2
 - 2019-04-30
 - new release
 - extended compatibility with cygwin and Windows
 
# ctrdata 0.18.9004
 - 2019-04-28
 - find and use any cygw* under windows
 - refactored escaping bash command when called under windows

# ctrdata 0.18.9002
 - 2019-04-21
 - corrected typing date fields

# ctrdata 0.18.1
 - 2019-04-14
 - simplified cygwin install
 - updated documentation
 - corrected inconsistent handling of query terms

# ctrdata 0.18.9001
 - 2019-04-12
 - added automated proxy handling

# ctrdata 0.18
 - 2019-04-11
 - release version
 - bug fixes in field typing
 - move to use remote mongodb server
 - updated vignettes
 
# ctrdata 0.17
 - 2019-03-27
 - release version
 
# ctrdata 0.16.9002
 - 2019-03-26
 - improve dbFindFields() formatting
 - added parameter to force running a query again
   
# ctrdata 0.16.9001
 - 2019-03-26
 - added further typing (some of the numeric fields)
 - improve cygwin install attempts and information
   
# ctrdata 0.16.9000
 - 2019-03-24
 - removed dependency on local mongodb installation (major rewrite)
 - improved support for remote mongodb servers (note changes in host / db / uri parameters)
   
# ctrdata 0.15.9007
 - 2019-03-15
 - Important: Added no checking of SSL certificates for EUCTR because the EUCTR server is
   not sending the required intermediate and root certificates, thus failing curl and httr, see
   https://www.digicert.com/help/?host=www.clinicaltrialsregister.eu

# ctrdata 0.15.0
 - 2019-03-13
 - fixed EUCTR results retrieval (curl return value order not predictable)
 - removed second time adding metadata in one function
 - streamlined user information and progress feedback

# ctrdata 0.14.3
 - 2019-03-12
 - turned error into message when no new trials are found
 - prevent failing tests if no new trials found in rss feed
 
# ctrdata 0.14.2
 - 2019-03-07
 - harmonise user information
 
# ctrdata 0.14.1
 - 2019-03-07
 - replaced RCurl (which failed for some register servers) by httr and curl
 - create README.md from README.Rmd

# ctrdata 0.14
 - 2019-03-06
 - intention to submit to CRAN, therefore changing several warnings to messages, improve testing of tool chain applications
 
# ctrdata 0.13.3
 - 2019-03-03
 - prettified dbFindFields() by removing count symbols (XX.) from paths
 - improve converting of invalid XML from EUCTR result files to JSON
 
# ctrdata 0.13.2
 - 2019-02-28
 - made EUCTR retrieval more robust
 - added marginal case for url of single trial in EUCTR
 - extended timeout for checking online status of EUCTR
 
# ctrdata 0.13.1
 - 2019-02-24
 - added typing of dates and some logical fields when using dbGetFieldsIntoDf()

# ctrdata 0.13
 - 2019-01-06
 - changes : 
   - dbGetVariablesIntoDf() is deprecated, use dbGetFieldsIntoDf() instead
   - dbFindVariable() is deprecated, use dbFindFields() instead
   - in dbMergeTwoVariablesRelevel() parameter varnames is deprecated, use colnames instead
 
# ctrdata 0.12.1
 - 2018-12-15
 - added function ctrFindActiveSubstanceSynonyms() to obtain synonyms for an active substance
 - added user information on number of trials in CTGOV to be downloaded, and limit this to 5000 per query
 - corrected import from EUCTR for details = FALSE
   
# ctrdata 0.12
 - 2018-05-19
 - added possibility to add user's annotations to records retrieved with a query (new option annotate.text)
   
# ctrdata 0.11.2
 - 2018-04-22
 - changed queryterm processing
   
# ctrdata 0.11.1
 - 2018-04-07
 - improved installFindMongoBinaries(), 
   should now better detect mongo binary locations and use 
   for example in cron scripts, which may not have access 
   to a user's path information

# ctrdata 0.11
 - 2018-01-28
 - newly retrieved: EUCTR results history, into new fields 
   "firstreceived_results_date" and "version_results_history"
 - adding feature as per issue #8

# ctrdata 0.10.4
 - 2017-12-28
 - note on compatibility with mongoDB versions
 - fixing issue #8
 - simplified license
 
## ctrdata 0.10.3
 - 2017-11-24
 - changed custom-built "x5_trial_status" to "p_end_of_trial_status" provided by EUCTR
 
# ctrdata 0.10.2
 - 2017-11-22
 - editorial project updates
 
# ctrdata 0.10.1
 - 2017-07-30
 - now loading results from euctr where available as xml
 
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
