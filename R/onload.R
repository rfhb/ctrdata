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
                        "https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use\n\n"
  )
  #
  # check availabilities
  if (.Platform$OS.type == "windows") installCygwinWindowsTest()
  #
  if (!suppressWarnings(installFindBinary("php --version")))
    warning("php not found, ctrLoadQueryIntoDb() will not work.", call. = FALSE, immediate. = TRUE)
  #
  if (!suppressWarnings(installFindBinary("php -r 'simplexml_load_string(\"\");'")))
    warning("php xml not found, ctrLoadQueryIntoDb() will not work.", call. = FALSE, immediate. = TRUE)
  #
  if (!suppressWarnings(installFindBinary("echo x | sed s/x/y/")))
    warning("sed not found, ctrLoadQueryIntoDb() will not work.", call. = FALSE, immediate. = TRUE)
  #
  if (!suppressWarnings(installFindBinary("perl -V:osname")))
    warning("perl not found, ctrLoadQueryIntoDb() will not work.", call. = FALSE, immediate. = TRUE)
  #
  if (class(try(installMongoFindBinaries(), silent = TRUE)) == "try-error")
    warning("mongo / mongoimport not found, ctrLoadQueryIntoDb() will not work.\n", call. = FALSE, immediate. = TRUE)
  #
  packageStartupMessage("Completed testing helper binaries.")
  #
  invisible()
  #
}
