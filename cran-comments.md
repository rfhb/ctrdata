## Test environments
* local: 86_64-apple-darwin15.6.0
* travis: R oldrel, release, devel
* win-builder: windows-x86_64-devel
* appveyor: r-release and r-oldrel

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Submission reason
* release after nodbi 0.4 is available, 
  to handle mix of arrays and text values in same json key
* parsing extended for trial records from EUCTR
* correction of tests and of re-opening sqlite connection

