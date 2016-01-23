### ctrdata package
### initialising

# create environment private to this package
.privateEnv <- new.env()
# for caching the location of mongo binaries
assign("mongoBinaryLocation", NA, envir = .privateEnv)

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("\nPlease respect the requirements of the copyrights of \n",
                        "the clinical trial registers when using their information, \n",
                        "see ctrOpenSearchPagesInBrowser(copyright = TRUE).\n\n",
                        "More on this package and how to use it: https://github.com/rfhb/ctrdata/\n\n"
  )

  invisible()
}
