## Test environments

* local: R version 4.4.1 (2024-06-14) on x86_64-apple-darwin20 (ok)

* Win-builder: R Under development (unstable) (2024-08-27 r87062 ucrt); R version 4.4.1 (2024-06-14 ucrt); R version 4.3.3 (2024-02-29 ucrt) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)

* Mac builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

Explanation of NOTES: 
- links are accessible manually but may respond with an HTML error 403 in test systems
- https://eudract.ema.europa.eu/ may present an incomplete certificate chain


## Submission reason

### Bug fixes
- Fix incomplete downloads from `CTIS` by disabling HTTP/2 multiplexing in another function

### Improvement
- Added typing for newly appearing variables in `CTIS`
- Accelerated `CTIS` trial data processing


## Reverse dependency checks

No reverse dependencies detected at this time. 


----

Many thanks,
Ralf
