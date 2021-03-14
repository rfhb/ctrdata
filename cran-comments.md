## Test environments
* local: macOS (x86_64-apple-darwin15.6.0), R 3.6.3
* github-actions: Windows (Microsoft Windows Server 2019), R release
* github-actions: macOS (10.15.7), R release and R oldrel
* win-builder: x86_64-w64-mingw32, R unstable (2021-03-11 r80086)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
None so far

## Submission reason
* improved functionality (speed, minimise impact of database choice)
* additional package tests (coverage now more than 85% on appveyor)
* minor bug fixing (in part subsequent to updates of other packages)
