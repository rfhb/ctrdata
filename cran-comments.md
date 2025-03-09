## Test environments

* local: R Under development (unstable) (2025-03-08 r87910) on aarch64-apple-darwin20 (ok)

* Win-builder: R version 4.4.3 (2025-02-28 ucrt); R Under development (unstable) (2025-03-08 r87910 ucrt); R version 4.3.3 (2024-02-29 ucrt) (ok)

* macOS builder: r-release-macosx-arm64|4.4.2|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0 (ok; macOS builder for R devel not working)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)

Results: 

On Win-builder other than R Under development, 1 NOTE which reads "Author field differs from that derived from Authors@R", even though I built the uploaded package with the latest R Under development (unstable) (2025-03-08 r87910), as recommended in https://stat.ethz.ch/pipermail/r-package-devel/2025q1/011402.html. 


## Local R CMD check results

0 errors | 0 warnings | 0 notes


## Submission reason

- New features and functions 
- Corrected testing, typing
- Expanded and improved docs

## Reverse dependency checks

No reverse dependencies found. 


----

Many thanks,
Ralf
