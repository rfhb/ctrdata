## Test environments

* local: macOS 21.5.0, R 3.6.3, R 4.1.2
* win-builder: R Under development (unstable) (2022-06-24 r82518 ucrt)
* r-hub-builder: Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel, clang, gfortran; Ubuntu Linux 20.04.1 LTS, R-release, GCC
* macOS builder: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Submission reason

 - added: ctrLoadQueryIntoDb now also extracts and saves results files other than PDF files
 
 - changed: ctrLoadQueryIntoDb gains parameter euctrresultsfilespath, deprecating euctrresultspdfpath
 
 - fixed: ctrFindActiveSubstanceSynonyms() returns NULL for non-existing active substance

## Notes

(1) "URL: https://www.clinicaltrialsregister.eu/
    From: DESCRIPTION
          man/ctrdata-registers.Rd
          README.md
    Status: Error
    Message: libcurl error code 35"

This is due to an incomplete certificate chain on the server, beyond my control. 

(2) "Found the following (possibly) invalid URLs:
  URL: https://clinicaltrials.gov/ct2/about-site/terms-conditions\#Use
    From: man/ctrdata-registers.Rd
    Status: 400
    Message: Bad Request"

The file R/ctrdata-register.R includes hyperlinks to URLs with query 
strings and an anchor, such as https://serverFqdn?&queryStrings#anchorName. 
Such URLs are ok in a '\href{}{}' but not when this is within a '\tabular{}' environment, 
where & and # remain reserved. In R/ctrdata-registers.R, & and # are escaped for rendering 
using LaTeX, but unchanged for other formats (using an '\ifelse{}{}{}' construct).

The hyperlinks are correct in the PDF version of the package manual and in the HTML help. 
No other solution was found to handle this situation (URL shorteners are undesirable). 

----------
Thank you!
Ralf Herold
