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
* added message in case of server connectivity issues
* propagating user settings for httr to curl operations
* bug fix: make return structures identical if trials found or not
* bug fix: corrected and improved user info in dfMergeTwoVariablesRelevel
* address marginal cases in dfTrials2Long (field occurs only once, identifier level) 
* remove duplicate rows fromdfName2Value, e.g. when criteria are duplicated
