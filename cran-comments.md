## Test environments

* local: R Under development (unstable) (2025-03-10 r87922) on aarch64-apple-darwin20 (ok)

* Win-builder: R Under development (unstable); R version 4.3.3 (2024-02-29 ucrt) (2025-03-12 r87950 ucrt) (ok)

* macOS builder: r-release-macosx-arm64|4.4.2|macosx|macOS 13.3.1 (22E261) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 notes


## Submission reason

- Fix user-reported bug, unrelated to recent release

- Fix CRAN "NOTE installed size is  5.3Mb"

- Cannot fix CRAN "ERROR Installation failed. Flavor: r-devel-linux-x86_64-debian-gcc" because this is caused by "Error: ERROR: no permission to install to directory ‘/home/hornik/tmp/R.check/r-devel-gcc/Work/build/Packages’"


## Reverse dependency checks

No reverse dependencies. 


----

Many thanks,
Ralf
