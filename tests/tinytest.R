
if ( requireNamespace("tinytest", quietly = TRUE) ) {

  # at_home is for development version e.g. 0.20.1.9001
  home <- length(unclass(packageVersion("ctrdata"))[[1]]) == 4
  tinytest::test_package("ctrdata", at_home = home)
}
