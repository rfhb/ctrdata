## Test environments

* local: R version 4.4.2 (2024-10-31) on aarch64-apple-darwin20 (ok)

* Win-builder: R version 4.4.2 (2024-10-31 ucrt); R version 4.3.3 (2024-02-29 ucrt) (ok)

* macOS builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

Explanation of NOTES: 
* links are accessible manually but may respond with an HTML error 403 in test systems
* https://eudract.ema.europa.eu/ may present an incomplete certificate chain


## Submission reason

* Adapt to upstream changes in CTGOV, curl
* Fixes for documents download
* Code improvements (speed, consistency)


## Reverse dependency checks

No reverse dependencies detected at this time. 


----

Many thanks,
Ralf
