### ctrdata package
### initialising

# create environment private to this package
# for caching the location of mongo binaries
.privateEnv <- new.env()

# check helper functions
.onAttach <- function(libname, pkgname) {
  #
  packageStartupMessage("\nInformation on this package and how to use it: \nhttps://github.com/rfhb/ctrdata/\n\n",
                        "Please respect the requirements and the copyrights of the\n",
                        "clinical trial registers when using their information. Call\n",
                        "ctrOpenSearchPagesInBrowser(copyright = TRUE) and visit\n\n",
                        "https://www.clinicaltrialsregister.eu/disclaimer.html\n",
                        "https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use\n"
  )
  #
  # check availabilities
  packageStartupMessage("Testing helper binaries: ")
  #
  if (.Platform$OS.type == "windows") installCygwinWindowsTest()
  #
  if (!suppressWarnings(installFindBinary("php --version")))
    packageStartupMessage("php not found, ctrLoadQueryIntoDb() will not work.")
  #
  if (!suppressWarnings(installFindBinary("php -r 'simplexml_load_string(\"\");'")))
    packageStartupMessage("php xml not found, ctrLoadQueryIntoDb() will not work.")
  #
  if (!suppressWarnings(installFindBinary("echo x | sed s/x/y/")))
    packageStartupMessage("sed not found, ctrLoadQueryIntoDb() will not work.")
  #
  if (!suppressWarnings(installFindBinary("perl -V:osname")))
    packageStartupMessage("perl not found, ctrLoadQueryIntoDb() will not work.")
  #
  if (class(try(installMongoFindBinaries(), silent = TRUE)) == "try-error")
    packageStartupMessage("mongo / mongoimport not found, ctrLoadQueryIntoDb() will not work.")
  #
  packageStartupMessage("completed.")
  #
  invisible()
  #
}
