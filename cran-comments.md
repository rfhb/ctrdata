## Test environments
* local: macOS (darwin15.6.0), R 3.6.3; Windows (19042.928), R 4.0.4
* github-actions: Windows (Microsoft Windows Server 2019), R release
* github-actions: macOS (10.15.7), R release and R oldrel
* win-builder: x86_64-w64-mingw32, 4.1.0 beta (2021-05-06 r80268)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
None so far

## Submission reason
* new feature: extended to a third register of clinical trials (ISRCTN)
* refactored and improved query handling, checking binaries, deduplicating ids
* reduced code complexity, accelerated functions and reduced memory use
