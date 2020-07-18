if (requireNamespace("tinytest", quietly = TRUE) &&
    utils::packageVersion("tinytest") >= "1.0.0") {

  # extended testing (at_home = TRUE) for CI
  # but not to be executed on CRAN (where
  # released versions have 3 components)

  # - see section 4.4 in vignette("using_tinytest", "tinytest")
  cran <- !length(unclass(packageVersion("ctrdata"))[[1]]) == 4
  # - alternative criterion
  # cran <- Sys.getenv("ATHOME") == ""

  # do testing
  tt <- tinytest::test_package(
    pkgname = "ctrdata",
    color = FALSE,
    at_home = cran)

  # unless on CRAN
  if (!cran) {
    saveRDS(
      object = tt,
      file = "tinytest.rds")
  }

}
