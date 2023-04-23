## Test environments

* local: macOS 21.6.0, R 4.2.3 (ok)

* github actions: windows-2022 20230417.1 R4.3.0 | macos-12 20230416.1 R4.3.0 | macos-12 20230416.1 R4.2.3 | ubuntu-22.04 20230417.1 R Under development (unstable) (2023-04-22 r84300) (ok)

* Win-builder: R Under development (unstable) (2023-04-23 r84305 ucrt) using platform: x86_64-w64-mingw32 (64-bit) (ok)

* R-hub: macOS 10.13.6 High Sierra, R-release, CRAN's setup | R version 4.1.1 (2021-08-10) x86_64-apple-darwin17.0 (64-bit) | R version 4.1.2 (2021-11-01) i386-pc-solaris2.10 (ok) 


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
