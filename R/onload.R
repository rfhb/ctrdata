### ctrdata package
### initialising

# create environment private to this package
# for caching the location of mongo binaries
.dbffenv <- new.env(parent = emptyenv())

# check helper functions
.onAttach <- function(libname, pkgname) {
  #
  packageStartupMessage(
    "Information on this package and how to use it:\n",
    "https://cran.r-project.org/package=ctrdata\n\n",
    "Please respect the requirements and the copyrights of the\n",
    "clinical trial registers when using their information. Call\n",
    "ctrOpenSearchPagesInBrowser(copyright = TRUE) and visit\n",
    "https://www.clinicaltrialsregister.eu/disclaimer.html\n",
    "https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use\n"
  )
  #
  # check availabilities
  packageStartupMessage(
    "Checking helper binaries: ",
    appendLF = FALSE)
  #
  if (.Platform$OS.type == "windows") {
    installCygwinWindowsTest()
  }
  #
  if (!suppressWarnings(
    installFindBinary(commandtest = "php --version"))) {
    packageStartupMessage(
      "\nphp not found, ctrLoadQueryIntoDb() will not work ",
      appendLF = FALSE)
  }
  #
  if (!suppressWarnings(
    installFindBinary(commandtest = "php -r 'simplexml_load_string(\"\");'"))) {
    packageStartupMessage(
      "\nphp xml not found, ctrLoadQueryIntoDb() will not work ",
      appendLF = FALSE)
  }
  #
  if (!suppressWarnings(
    installFindBinary(commandtest = "echo x | sed s/x/y/"))) {
    packageStartupMessage(
      "\nsed not found, ctrLoadQueryIntoDb() will not work ",
      appendLF = FALSE)
  }
  #
  if (!suppressWarnings(installFindBinary(commandtest = "perl -V:osname"))) {
    packageStartupMessage(
      "\nperl not found, ctrLoadQueryIntoDb() will not work ",
      appendLF = FALSE)
  }
  #
  packageStartupMessage("done.")
  #
  invisible()
  #
} # .onAttach
