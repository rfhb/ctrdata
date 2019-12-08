## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

df <- data.frame(
  "var1" = 1:3,
  "var2" = 2:4,
  stringsAsFactors = FALSE)

statusvalues <- list(
  "Firstvalues" = c("12", "23"),
  "Lastvalue"   = c("34"))


#### dfMergeTwoVariablesRelevel ####

# test
expect_error(
  dfMergeTwoVariablesRelevel(list("var1", "var2")),
  "Need a data frame as input.",
  info = "ctrdata_euctr.R#22")

# test
expect_message(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = c("var1", "var2")),
  "Unique values returned: 12, 23, 34",
  info = "ctrdata_euctr.R#30")

# test
expect_true(
  "character" %in% class(
    suppressMessages(
      dfMergeTwoVariablesRelevel(
        df = df,
        colnames = c("var1", "var2")))),
  info = "ctrdata_euctr.R#39")

# test
expect_message(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = c("var1", "var2"),
    levelslist = statusvalues),
  "Unique values returned: Firstvalues, Lastvalue",
  info = "ctrdata_euctr.R#48")

# test
expect_error(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = 1:3),
  "Please provide exactly two column names.",
  info = "ctrdata_euctr.R#56")


#### ctrOpenSearchPagesInBrowser ####
if (!at_home()) exit_file("skipping")
if (!check_internet()) exit_file("Not available: internet connectivity")

expect_equal(
  suppressWarnings(ctrGetQueryUrlFromBrowser(
    "something_insensible")),
  NULL,
  info = "ctrdata_euctr.R#67")

q <- "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

tmp_test <- suppressMessages(
  ctrGetQueryUrlFromBrowser(
    content = q))

# test
expect_true("data.frame" %in% class(tmp_test),
            info = "ctrdata_euctr.R#77")

# test
expect_warning(
  ctrGetQueryUrlFromBrowser(
    content = "ThisDoesNotExist"),
  "no clinical trial register search URL found",
  info = "ctrdata_euctr.R#84")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(input = q),
  "Opening browser for search:",
  info = "ctrdata_euctr.R#90")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(input = tmp_test),
  "Opening browser for search:",
  info = "ctrdata_euctr.R#96")

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/",
            "search?query=&age=under-18&status=completed")

tmp <- suppressMessages(
  ctrGetQueryUrlFromBrowser(
    content = q))

# test
expect_true("data.frame" %in% class(tmp_test),
            info = "ctrdata_euctr.R#106")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(q),
  "Opening browser for search:",
  info = "ctrdata_euctr.R#112")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(tmp_test),
  "Opening browser for search:",
  info = "ctrdata_euctr.R#118")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    register = c("EUCTR", "CTGOV"),
    copyright = TRUE),
  TRUE,
  info = "ctrdata_euctr.R#126")
