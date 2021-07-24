## Test environments
* local: macOS (darwin17.0), R 3.6.3, R 4.1.0; Windows (19043.1110), R 4.1.0
* github-actions: Windows (Microsoft Windows Server 2019), R release
* github-actions: macOS (10.15.7), R release and R oldrel

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
None

## Submission reason
* removed database backend-specific code
* now requires nodbi >=0.4.3
* bug fixes (typing for certain date fields, closing interrupted connections)
* better testing for register server availability and functioning


----------
Thank you
Ralf
