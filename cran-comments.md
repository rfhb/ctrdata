## Test environments

* local: R version 4.5.1 RC (2025-06-05 r88281) on aarch64-apple-darwin20 (ok)

* Win-builder: R version 4.5.1 (2025-06-13 ucrt); R version 4.4.3 (2025-02-28 ucrt); R Under development (unstable) (2025-07-24 r88447 ucrt) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)

* macOS builder: r-oldrel-macosx-arm64|4.6.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0; r-devel-macosx-arm64|4.5.1|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0 (ok)

## Local R CMD check results

0 errors | 0 warnings | 0 notes


## Submission reason

I am sorry for this submission of a bug fix shortly after the latest release. 
Only this week, we became aware that an erroneous reference value list was used since months. 

- Replace a list of reference values for one register
- Reduce size of installed package in view of CRAN Check Note


## Reverse dependency checks

No reverse dependencies. 


----

Many thanks,
Ralf
