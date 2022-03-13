## Test environments

* local: macOS (21.3.0), R 3.6.3, R 4.1.2
* github-actions: Microsoft Windows Server 2022 10.0.20348, R version 4.1.3 (2022-03-10)
* github-actions: macOS 11.6.4 20G417, R version 4.1.3 (2022-03-10), R version 4.1.2 (2021-11-01)
* github-actions: Ubuntu 20.04.4 LTS, R Under development (unstable) (2022-03-12 r81880), R version 4.1.3 (2022-03-10), R version 4.0.5 (2021-03-31)
* win-builder: R Under development (unstable) (2022-03-12 r81880 ucrt)
* macOS builder: r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8 

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None

## Submission reason

 - expanded and corrected documentation
 - refactored and accelerated database import
 - return tibbles if dplyr is loaded
 - refactored and improved dbGetFieldsIntoDf() 
 - refactored and improved fields typing
 - refactored and accelerated binaries checking
 - refactored internal caching
 - corrected dfTrials2Long() for nested and special fields
 - dbFindIdsUniqueTrials() now works if only single trial in any register
 - dbFindFields() now returns names of node and all leaf fields
 - changed EU Member State to default to DE
 - corrected installCygwinWindowsDoInstall() to properly update installation

## Notes

"R CMD check --as-cran ctrdata_1.9.0.tar.gz" reports: 

"Found the following (possibly) invalid URLs:
  URL: https://clinicaltrials.gov/ct2/about-site/terms-conditions\#Use
    From: man/ctrdata-registers.Rd
    Status: 400
    Message: Bad Request"

As far as I understand this, this is neither a NOTE, nor a WARNING, nor an ERROR. 

File R/ctrdata-register.R includes several hyperlinks to URLs that have query 
strings and / or an anchor, such as https://serverFqdn?&queryStrings#anchorName. 

Such URLs are ok in a '\href{}{}' but not when this is within a '\tabular{}' environment, 
where & and # remain reserved. Thus, in R/ctrdata-registers.R, & and # are escaped for rendering 
using LaTeX, but unchanged for other formats (using an '\ifelse{}{}{}' construct).
The hyperlinks are correct in the PDF version of the package manual and in the HTML help. 
No other solution was found to handle this situation (URL shorteners are undesirable). 

IMHO the extraction of URLs from Rd files would work if the argument "ifdef" was TRUE 
either in the signature of function ".get_urls_from_Rd" (line 47 in 
https://svn.r-project.org/R/trunk/src/library/tools/R/urltools.R) or when called by 
"url_db_from_package_Rd_db" (line 175 in same file). I simulated this; it worked for my case. 

Please let me know if this potential bug and solution should be reported (to Core). 

----------
Many thanks!
Ralf Herold
