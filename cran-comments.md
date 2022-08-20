## Test environments

* local: macOS 21.6.0, R 4.2.1, R 3.6.3
* win-builder: R Under development (unstable) (2022-08-19 r82734 ucrt)
* macOS builder: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8 
* r-hub-builder: Apple Silicon (M1), macOS 11.6 Big Sur, R-release; macOS 10.13.6 High Sierra, R-release, CRAN's setup; 
  Debian Linux, R-devel, clang, ISO-8859-15 locale;	Fedora Linux, R-devel, GCC

## Local R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Submission reason

Fixes concerning: 

 - documentation (missed escaping one URL and this 
   resolves a CRAN check note, ability to run examples)
 
 - package testing (timeouts and methods) 
 
 - functionality (slow speed of dfName2Value(), row names of
   dfName2Value(), tibble handling, certain queries to ISRCTN, 
   handling missing data in dbGetFieldsIntoDf(), complex fields
   in dbGetFieldsIntoDf(), `wherevalue` lacked in dfName2Value())
 
----------
Thank you!
Ralf Herold
