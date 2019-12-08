
if ( requireNamespace("tinytest", quietly = TRUE) &&
     utils::packageVersion("tinytest") >= "1.0.0") {

  # home <- length(unclass(packageVersion("ctrdata"))[[1]]) == 4
  home <- Sys.getenv("AT_HOME") != ""
  tinytest::test_package("ctrdata", at_home = home)
}
