## Test environments

* local: macOS 21.6.0, R 4.2.3
* win-builder: R version 4.3.0 alpha (2023-03-26 r84066 ucrt)
* macOS builder: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8
* r-hub-builder: Windows Server 2022, R-release, 32/64 bit; Fedora Linux, R-devel, GCC; Debian Linux, R-devel, clang, ISO-8859-15 locale

## Local R CMD check results

0 errors | 0 warnings | 0 notes 

(NOTES on URLs starting with https://eudract.ema.europa.eu/ 
are due to an incomplete certificate chain presented by this 
server, and this cannot be rectified by the package author.)

## Submission reason

New / extended features: 
 - data from CTIS is imported more completely
 - adapt other functions to accommodate CTIS
 - provide Tampermonkey script to get URL of a user's query onto clipboard

## Bug fixes

 - speed up ctrLoadQueryIntoDb() for CTIS with nodbi >=0.9.2.9000
 - keep register names on vector returned by dbFindIdsUniqueTrials()
 - correct dbFindFields() for EUCTR

## Reverse dependencies

None at the moment.

----------
Thank you!
Ralf Herold
