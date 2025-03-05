## Test environments

* local: R version 4.4.2 Patched (2024-11-08 r87310) on aarch64-apple-darwin20 (ok)

* Win-builder: R version 4.4.2 (2024-10-31 ucrt); R version 4.3.3 (2024-02-29 ucrt) ; R Under development (unstable) (2024-12-12 r87438 ucrt) (ok)

* macOS builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)


## Local R CMD check results

0 errors | 0 warnings | 2 notes 

Explanation of NOTES: 

* frequent use of non-standard evaluation in f.* functions

## Submission reason

- Correct certain testing conditions
- Refactor `dfTrials2Long` to correct generating identifiers for marginal cases
- New function `ctrShowOneTrial()` with a widget to inspect a trial structure and data
- Change dependency to Suggests

## Reverse dependency checks

No reverse dependencies detected at this time. 


----

Many thanks,
Ralf
