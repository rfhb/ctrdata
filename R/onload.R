### ctrdata package
### initialising

# create environment private to this package
# for caching (field names, binary checks)
# name of hidden environment: .ctrdataenv
# see also ctrCache
.ctrdataenv <- new.env()

# check helper functions
.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "\nInformation on this package and how to use it:\n",
    "https://cran.r-project.org/package=ctrdata\n\n",
    "Please respect the requirements and the copyrights of the\n",
    "clinical trial registers when using their information. Call\n",
    "ctrOpenSearchPagesInBrowser(copyright = TRUE) and visit\n",
    "https://www.clinicaltrialsregister.eu/disclaimer.html\n",
    "https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use\n",
    "https://www.isrctn.com/page/faqs#usingISRCTN\n"
  )

  # return
  invisible()

} # .onAttach
