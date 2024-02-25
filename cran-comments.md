## Test environments

* local: R Under development (unstable) (2024-02-23 r85975) using platform: x86_64-apple-darwin20 (ok)

* macOS builder: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* Win-builder: R Under development (unstable) (2024-02-24 r85984 ucrt) using platform: x86_64-w64-mingw32; R version 4.3.2 (2023-10-31 ucrt) using platform: x86_64-w64-mingw32 (64-bit); R version 4.2.3 (2023-03-15 ucrt) using platform: x86_64-w64-mingw32 (64-bit) (ok)

* R-hub: Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel, clang, gfortran (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

Explanation of NOTES: 
- links to publications are accessible manually but may respond with an HTML error 403 in test systems
- https://eudract.ema.europa.eu/ may present an incomplete certificate chain.


## Submission reason

- The ERROR on CRAN is related to a dependency which was corrected on 2024-02-24 (nodbi version 0.10.2, see https://cloud.r-project.org/web/checks/check_results_nodbi.html)

- Correct handling multiple public events with `CTIS`
- Switch sequence of API endpoints used with `CTIS` 
- Re-use `CTIS` downloads in a given session


## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

----

Many thanks,
Ralf
