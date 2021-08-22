## Test environments
* local: macOS (20.6.0), R 3.6.3, R 4.1.0; Windows (19043.1110), R 4.1.0
* github-actions: Windows (Microsoft Windows Server 2019), R release
* github-actions: macOS (10.15.7), R release and R oldrel
* R: win-builder (2021-08-20 r80804), R-hub builder (check_for_cran)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
None

## Submission reason
* fix DBI not needed in Imports anymore (should fix Note on CRAN)
* fix potential file name issue in conversion script for register text to JSON
* fix dbFindFields() to never return _id (previously depended on database backend)
* changed tests (CRAN detection, register availability, additional tests) (should fix Error on CRAN for r-patched-linux-x86_64)


----------
Thanks & greetings
Ralf
