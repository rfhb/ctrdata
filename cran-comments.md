## Test environments

- local: macOS 21.6.0, R 4.2.3 (ok)

- macOS builder: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

- Win-builder: 4.3.2 (2023-10-31 ucrt) using platform: x86_64-w64-mingw32 (64-bit); R version 4.2.3 (2023-03-15 ucrt)
* using platform: x86_64-w64-mingw32; R Under development (unstable) (2023-11-23 r85618 ucrt) x86_64-w64-mingw32 (ok)

- R-hub: Windows Server 2022, R-devel, 64 bit (ok) but builds on Fedora Linux, R-devel, clang, gfortran; Ubuntu Linux 20.04.1 LTS, R-release, GCC timed out after ~75 minutes, apparently because duckdb was compiled from source (it is listed among Suggests) even though `rhub::check_for_cran(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false")` had been used. 



## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

NOTES come from checking package documentation (referenced publicationswork when accessed manually but respond with HTML error 403 when accessed from test systems; https://eudract.ema.europa.eu/ presents an incomplete certificate chain, beyond my control).



## Submission reason

- Removed system dependencies (SystemRequirements is now empty)
- Corrected various issues including all raised on GitHub 
- Reduced size of included test database 
- Refactored text processing code
- Reorganised files in R folder
- Improved documentation 



## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

----

Thank you! 

Ralf
