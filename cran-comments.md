## Test environments

* local: macOS (21.4.0), R 3.6.3, R 4.1.2
* github-actions: Microsoft Windows Server 2022 10.0.20348, R version 4.2.0 (2022-04-22 ucrt)
* github-actions: macOS 11.6.5 20G527, R version 4.2.0 (2022-04-22), R version 4.1.3 (2022-03-10)
* github-actions: Ubuntu 20.04.4 LTS, R Under development (unstable) (2022-04-22 r82232), R version 4.2.0 (2022-04-22), R version 4.1.3 (2022-03-10)
* win-builder: R Under development (unstable) (2022-04-23 r82240 ucrt)
* r-hub-builder: Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel, clang, gfortran; Ubuntu Linux 20.04.1 LTS, R-release, GCC
* macOS builder: r-devel-macosx-arm64

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

## Submission reason

- type e811... variables
- bugfix in dbGetFieldsIntoDf
- bugfix annotations mix up for some backends
- editorial update vignettes

## Notes

(1) "URL: https://www.clinicaltrialsregister.eu/about.html
    From: man/ctrdata-registers.Rd
    Status: Error
    Message: libcurl error code 35"

This is due to an incomplete certificate chain on the server, beyond my control. 

(2) "Found the following (possibly) invalid URLs:
  URL: https://clinicaltrials.gov/ct2/about-site/terms-conditions\#Use
    From: man/ctrdata-registers.Rd
    Status: 400
    Message: Bad Request"

The file R/ctrdata-register.R includes several hyperlinks to URLs that have query 
strings and / or an anchor, such as https://serverFqdn?&queryStrings#anchorName. 
Such URLs are ok in a '\href{}{}' but not when this is within a '\tabular{}' environment, 
where & and # remain reserved. In R/ctrdata-registers.R, & and # are escaped for rendering 
using LaTeX, but unchanged for other formats (using an '\ifelse{}{}{}' construct).
The hyperlinks are correct in the PDF version of the package manual and in the HTML help. 
No other solution was found to handle this situation (URL shorteners are undesirable). 

----------
Many thanks!
Ralf Herold
