if (requireNamespace("tinytest", quietly = TRUE)) {

  # extended testing (at_home = TRUE) for CI
  # but not to be executed on CRAN (where
  # released versions have 3 components)

  # see section 4.4 in vignette("using_tinytest", "tinytest")
  cran <- length(unclass(packageVersion("ctrdata"))[[1]]) == 3
  cran <- TRUE

  # do testing unless on CRAN
  if (!cran) {

    tt <- tinytest::test_package(
      pkgname = "ctrdata",
      color = FALSE,
      at_home = !cran)

    saveRDS(
      object = tt,
      file = "tinytest.rds")
  }

}
