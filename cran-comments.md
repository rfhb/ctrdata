## Test environments

* local: R Under development (unstable) (2026-06-24 r90190) on aarch64-apple-darwin23 (ok)

* Win-builder: R version 4.5.3 (2026-03-11 ucrt); R version 4.6.1 (2026-06-24 ucrt); R Under development (unstable) (2026-08-27 r90452 ucrt) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel' (ok)

* macOS builder: timed out


## Local R CMD check results

0 errors | 0 warnings | 0 notes* 

Sometimes, notes are triggered by throttling services for URLs from certain sites (e.g., Status: 429, Message: Too Many Requests)


## Submission reason

Bug fixes and minor functional improvements

- Added support for MariaDB (nodbi v0.15.0)
- Added tests to increase coverage of code testing
- Type as `NA` new ISRCTN texts that indicate a value is missing 
- Added using all secondary ISRCTN identifiers for `dbFindIdsUniqueTrials()`
- Refactored `ctrFindActiveSubstanceSynonyms()` to use MeSH terms in CTGOV2
- Corrected `f.primaryEndpointResults()` for CTGOV (did not affect CTGOV2)
- Corrected `f.sampleSize()`: for EUCTR, in news for 1.26.2 (#62), testing
- Corrected import of history of results for EUCTR (`euctrresultshistory = TRUE`)
- Minor update to browser script (see https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser) 


## Reverse dependency checks

No reverse dependencies were found. 


----

Many thanks,
Ralf
