## Test environments
* local: macOS (darwin15.6.0), R 3.6.3; Windows (19042.928), R 4.0.4
* github-actions: Windows (Microsoft Windows Server 2019), R release
* github-actions: macOS (10.15.7), R release and R oldrel
* win-builder: x86_64-w64-mingw32, R unstable (2021-04-15 r80175)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
None so far

## Submission reason
* extended to work with a third register of clinical trials (ISRCTN)
* refactored and improved query handling, checking binaries, duplicate ids
* better handle queries with thousands of trials and many trial details
* reduced code complexity
