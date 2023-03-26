## Test environments

* local: macOS 21.6.0, R 4.3.0
* win-builder: R version 4.3.0 alpha (2023-03-23 r84035 ucrt)
* macOS builder: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8 
* r-hub-builder: Windows Server 2022, R-release, 32/64 bit; Fedora Linux, R-devel, GCC; Debian Linux, R-devel, clang, ISO-8859-15 locale

## Local R CMD check results

0 errors | 0 warnings | 0 notes 

(NOTES on URLs starting with https://eudract.ema.europa.eu/ 
are due to an incomplete certificate chain presented by this 
server, and this cannot be rectified by the package author.)

## Submission reason

### Bug fixes

 - added first access to new register: CTIS, the EU Clinical Trial Information System
 - stop (instead of warning) if register host errors (e.g. incorrect number of records)
 - switch to use curl::multi_download() which can resume retrievals from registers
 - require curl >= 5.0
 
## Reverse dependencies

There are no reverse dependencies so far.

----------
Thank you!
Ralf Herold
