## Test environments

* local: macOS (20.6.0), R 3.6.3, R 4.1.2;
* github-actions: Windows (Microsoft Windows Server 2019), R release
* github-actions: macOS (10.15.7), R release and R oldrel
* R: win-builder (2021-11-19 r81213)
* macOS builder

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None

## Submission reason

* changes to match nodbi 0.5.0 
* simplifying database operations (user-visible functions: 
  ctrLoadQueryIntoDb, dbFindIdsUniqueTrials, dbGetFieldsIntoDf), 
  without changes to API

## Note

* this release depends on nodbi 0.5.0, which I just submitted to CRAN
* if nodbi 0.5.0 is rejected, please do not release this submission

----------
Thanks & greetings
Ralf
