## Test environments

* local: macOS 21.6.0, R 4.2.3 (ok)

* macOS builder: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* Win-builder: R Under development (unstable) (2024-01-20 r85814 ucrt) (error as nodbi 0.10.0 published on CRAN on 2024-01-22 is not yet available)

* R-hub: Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

NOTES come from checking package documentation (referenced publicationswork when accessed manually but respond with HTML error 403 when accessed from test systems; https://eudract.ema.europa.eu/ presents an incomplete certificate chain, beyond my control).


## Submission reason

### Improvements
- Downloads are re-used during an interactive session 
- More register data fields get typed as number, logical or date

### Possibly breaking changes
- Reimplemented `dbGetFieldsIntoDf()` and `dbFindFields()` to accelerate and simplify, based on improved `nodbi` package v0.9.9

### Bug fixes
- Adapted to newly available data in CTIS
- Corrected escaping for JSON operation


## revdepcheck results

There are 0 reverse dependencies.


----

Many thanks,
Ralf
