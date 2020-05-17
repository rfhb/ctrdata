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
  "Need a data frame as input.")

# test
expect_message(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = c("var1", "var2")),
  "Unique values returned: 12, 23, 34")

# test
expect_true(
  "character" %in% class(
    suppressMessages(
      dfMergeTwoVariablesRelevel(
        df = df,
        colnames = c("var1", "var2")))))

# test
expect_message(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = c("var1", "var2"),
    levelslist = statusvalues),
  "Unique values returned: Firstvalues, Lastvalue")

# test
expect_error(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = 1:3),
  "Please provide exactly two column names.")


#### ctrOpenSearchPagesInBrowser ####
if (!at_home()) exit_file("Reason: not at_home")
if (Sys.getenv("ON_APPVEYOR") != "") exit_file("Reason: on Appveyor")
if (!check_internet()) exit_file("Reason: no internet connectivity")

expect_equal(
  suppressWarnings(ctrGetQueryUrlFromBrowser(
    "something_insensible")),
  NULL)

q <- "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

tmp_test <- suppressMessages(
  ctrGetQueryUrlFromBrowser(
    content = q))

# test
expect_true("data.frame" %in% class(tmp_test))

# test
expect_warning(
  ctrGetQueryUrlFromBrowser(
    content = "ThisDoesNotExist"),
  "no clinical trial register search URL found")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(input = q),
  "Found search query")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(input = tmp_test),
  "Opening browser for search:")

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/",
            "search?query=&age=under-18&status=completed")

tmp_test <- suppressMessages(
  ctrGetQueryUrlFromBrowser(
    content = q))

# test
expect_true("data.frame" %in% class(tmp_test))

# test
expect_message(
  ctrOpenSearchPagesInBrowser(q),
  "Found search query")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(q),
  "Opening browser for search:")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(tmp_test),
  "Opening browser for search:")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    register = c("EUCTR", "CTGOV"),
    copyright = TRUE),
  TRUE)
