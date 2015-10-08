.onAttach <- function(libname, pkgname) {

  packageStartupMessage("\nPlease respect the requirements of the copyrights of \n",
                        "the clinical trial registers when using their information, \n",
                        "see ctrOpenSearchPagesInBrowser(copyright = TRUE).\n\n",
                        "More on this package and how to use it: https://github.com/rfhb/ctrdata/\n\n",
                        "Note for this version: all functions were renamed for consistency and ease-of-use.\n"
  )

  invisible()
}
