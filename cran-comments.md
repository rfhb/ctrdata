## Test environments

* local: R Under development (unstable) (2024-02-23 r85975) using platform: x86_64-apple-darwin20 (ok)

* Win-builder: R version 4.4.1 (2024-06-14 ucrt) using platform: x86_64-w64-mingw32; R Under development (unstable) (2024-06-29 r86852 ucrt) using platform: x86_64-w64-mingw32;R version 4.3.3 (2024-02-29 ucrt) using platform: x86_64-w64-mingw32 (64-bit) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; macOS-latest, r: 'oldrel'; ubuntu-latest, r: 'devel' (ok)


## Local R CMD check results

0 errors | 0 warnings | 0 or 1 notes 

Explanation of NOTES: 
- links are accessible manually but may respond with an HTML error 403 in test systems
- https://eudract.ema.europa.eu/ may present an incomplete certificate chain.


## Submission reason

- Addressing `CRAN` note "installed size is 5.1 Mb" by reducing demo database
- Accommodate upstream changes: retirement of `CTGOV` and relaunch of `CTIS`
- Split utils.R into files for functions and fields


## Reverse dependency checks

No reverse dependencies detected at this time. 


----

Many thanks,
Ralf
