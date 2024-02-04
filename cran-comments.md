## Test environments

* local: macOS 21.6.0, R 4.2.3 (ok)

* macOS builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* Win-builder: R Under development (unstable) (2024-02-02 r85855 ucrt) x86_64-w64-mingw32; R version 4.2.3 (2023-03-15 ucrt) x86_64-w64-mingw32 (64-bit); R version 4.3.2 (2023-10-31 ucrt) x86_64-w64-mingw32 (64-bit) (ok)

* R-hub: Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC (ok)

* GitHub actions: macOS-lates, R release (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

NOTES come from checking package documentation (referenced publicationswork when accessed manually but respond with HTML error 403 when accessed from test systems; https://eudract.ema.europa.eu/ presents an incomplete certificate chain, beyond my control).


## Submission reason

### Improvements
- Provide access to additional data from `CTIS`, improved user information

### Bug fixes
- Added handling of Unicode emitted by `EUCTR` for at least one trial


## revdepcheck results

There are 0 reverse dependencies.


----

Many thanks,
Ralf
