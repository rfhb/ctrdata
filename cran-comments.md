## Test environments

* local: R version 4.5.0 (2025-04-11) on aarch64-apple-darwin20 (ok)

* Win-builder: R version 4.4.3 (2025-02-28 ucrt); R Under development (unstable) (2025-05-03 r88188 ucrt); R version 4.5.0 (2025-04-11 ucrt) (ok)

* macOS builder: r-release-macosx-arm64|4.4.2|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0 (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 notes


## Submission reason

- Adapt to upstream, no more warning to be handled (RSQLite 2.3.10 since 2025-05-02 accepts now up to 1000 arguments)
- Fixed txt2ndjson conversion for one or a few rare specific trial(s) from EUCTR
- Revised function to work with more than 80,000 records
- Make both CTIS1 and CTIS2 name work for deduplication


## Reverse dependency checks

No reverse dependencies. 


----

Many thanks,
Ralf
