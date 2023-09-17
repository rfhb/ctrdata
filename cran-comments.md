## Test environments

- local: macOS 21.6.0, R 4.2.3 (ok)

- macOS builder: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

- Win-builder: R Under development (unstable) (2023-09-08 r85113 ucrt); R version 4.2.3 (2023-03-15 ucrt); R version 4.3.1 (2023-06-16 ucrt) (ok)

- R-hub: Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

NOTES on URLs starting with https://eudract.ema.europa.eu/ 
are due to an incomplete certificate chain presented by this 
server, and this cannot be rectified by the package author.


## Submission reason

- to address issues following stream changes, including 
  fixing handling of multi-language strings from CTIS
- correct creating lists for downloading documents for ctis

## Reverse dependencies

None at the moment.

----

Many thanks,
Ralf
