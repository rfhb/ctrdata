## Test environments

* local: macOS 21.6.0, R 4.3.0
* win-builder: R Under development (unstable) (2022-10-11 r83083 ucrt)
* macOS builder: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8
* r-hub-builder: macOS 10.13.6 High Sierra, R-release, CRAN's setup; Fedora Linux, R-devel, clang, gfortran

## Local R CMD check results

0 errors | 0 warnings | 0 notes 

(NOTES on URLs starting with https://eudract.ema.europa.eu/ 
are due to an incomplete certificate chain presented by this 
server, and this cannot be rectified by the package author.)

## Submission reason

### Bug fixes

 - cater for very short EUCTR results-related information
 - show warning as beta CTGOV website is not supported
 - limit unit testing to MongoDB and SQLite
 - return error for ctrGetQueryUrl() if no query URL
 - prevent re-using connections to reduce http/2 layer errors
 - update query history when querytoupdate was used but no new records found
 - make ctrLoadQueryIntoDb() to always return visible result
 - correct dfTrials2Long() identifier (EUCTR no more top "1" across fields)
 - correct non-ASCII characters

## Reverse dependencies

There are no reverse dependencies so far.

----------
Thank you!
Ralf Herold
