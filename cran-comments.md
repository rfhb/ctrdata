## Test environments

* local: R version 4.5.1 Patched (2025-09-12 r88822) on aarch64-apple-darwin20 (ok)

* Win-builder:R version 4.4.3 (2025-02-28 ucrt); R Under development (unstable) (2025-12-06 r89118 ucrt); R version 4.5.2 (2025-10-31 ucrt) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)

* macOS builder: currently not working

## Local R CMD check results

0 errors | 0 warnings | 0 notes


## Submission reason

- Workaround for an upstream issue with one of the registers
- Change API call to new endpoint of one register
- Bug fixes (adapt to longer pagination token of one register)
- Add CITATION file with recent reference publication


## Reverse dependency checks

No reverse dependencies. 


----

Many thanks,
Ralf
