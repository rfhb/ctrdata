### ctrdata package
### initialising

# create environment private to this package
.privateEnv <- new.env()
# for caching the location of mongo binaries
assign("mongoBinaryLocation", NA, envir = .privateEnv)

.onAttach <- function(libname, pkgname) {
  #
  packageStartupMessage("\nPlease respect the requirements of the copyrights of \n",
                        "the clinical trial registers when using their information, \n",
                        "see ctrOpenSearchPagesInBrowser(copyright = TRUE).\n\n",
                        "More on this package and how to use it: https://github.com/rfhb/ctrdata/\n"
  )
  #
  # check availabilities
  if (.Platform$OS.type == "windows") installCygwinWindowsTest()
  #
  if(!suppressWarnings(findBinary("php --version")))                         warning("php not found, ctrLoadQueryIntoDb will not work.", call. = FALSE)
  if(!suppressWarnings(findBinary("php -r 'simplexml_load_string(\"\");'"))) warning("php xml not found, ctrLoadQueryIntoDb will not work.", call. = FALSE)
  if(!suppressWarnings(findBinary("echo x | sed s/x/y/")))                   warning("sed not found, ctrLoadQueryIntoDb will not work.", call. = FALSE)
  if(!suppressWarnings(findBinary("perl -V:osname")))                        warning("perl not found, ctrLoadQueryIntoDb will not work.", call. = FALSE)
  packageStartupMessage("Helper binaries tested.")
  #
  invisible()
  #
}
