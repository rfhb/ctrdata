## Test environments

* local: R Under development (unstable) (2026-01-16 r89305) on aarch64-apple-darwin20 (ok)

* Win-builder: R version 4.5.2 (2025-10-31 ucrt); R Under development (unstable) (2026-01-13 r89301 ucrt); R version 4.4.3 (2025-02-28 ucrt) (ok)

* GitHub actions: windows-2022, r: 'release'; ubuntu-latest, r: 'devel' (ok)

* macOS builder: Build system: r-devel-macosx-arm64|4.6.0|macosx|macOS 26.2 (25C56)|Mac mini|Apple M1||en_US.UTF-8|macOS 14.4|clang-1700.6.3.2|GNU Fortran (GCC) 14.2.0 (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 notes


## Submission reason

- Address CRAN Note concerning links between help pages and vignettes
- Refactored downloading data and documents to run, throttle and retry more robustly
- Updated user info, clarify history is updated only if at least one trial is loaded
- Bug fixes for evolving and edge cases


## Reverse dependency checks

No reverse dependencies. 


----

Many thanks,
Ralf
