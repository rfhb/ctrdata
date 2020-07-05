# see section 4.4 in vignette("using_tinytest", "tinytest")
if (requireNamespace("tinytest", quietly = TRUE) &&
    utils::packageVersion("tinytest") >= "1.0.0") {

  # home <- length(unclass(packageVersion("ctrdata"))[[1]]) == 4
  home <- Sys.getenv("ATHOME") != ""
  tinytest::test_package("ctrdata", at_home = home)
}
