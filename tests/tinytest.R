
if ( requireNamespace("tinytest", quietly = TRUE) &&
     utils::packageVersion("tinytest") >= "1.0.0") {

  # at_home is for development version e.g. 0.20.1.9001
  # home <- length(unclass(packageVersion("ctrdata"))[[1]]) == 4
  tinytest::test_package("ctrdata")
}
