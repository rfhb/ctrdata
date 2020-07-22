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


#### environment ####

if (.Platform$OS.type != "windows") {
  expect_error(
    installCygwinWindowsDoInstall(),
    "only for MS Windows")
}
if (.Platform$OS.type == "windows") {
  expect_error(
    installCygwinWindowsDoInstall(),
  "cygwin is already installed")
}


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

# test
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df,
    varnames = c("var1", "var2")),
  "Parameter varnames is deprecated, use colnames instead")

# test
expect_error(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = c("var1", "var2"),
    levelslist = 1:2),
  "Need list for parameter 'levelslist'")


#### ctrGetQueryUrlFromBrowser ####

expect_equal(
  suppressWarnings(ctrGetQueryUrlFromBrowser(
    "ThisDoesNotExist")),
  NULL)

# ctrGetQueryUrlFromBrowser(url = "type=Intr&age=0&intr=Drug&phase=0&phase=1&strd_e=12%2F31%2F2010")
# ctrGetQueryUrlFromBrowser(url = "type=Intr&age=0&intr=Drug&phase=0&phase=1&strd_e=12%2F31%2F2010", "CTGOV")
# ctrGetQueryUrlFromBrowser(url = "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0")
# ctrGetQueryUrlFromBrowser(url = "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0", "CTGOV")
# ctrGetQueryUrlFromBrowser(url = "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0", "EUCTR")
# ctrGetQueryUrlFromBrowser(url = "")
# ctrGetQueryUrlFromBrowser(url = NA)
# ctrGetQueryUrlFromBrowser(url = list())

q <- "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

tmp_test <- suppressMessages(
  ctrGetQueryUrlFromBrowser(
    url = q))

# test
expect_true("data.frame" %in% class(tmp_test))

# test
expect_warning(
  ctrGetQueryUrlFromBrowser(
    url = "ThisDoesNotExist"),
  "no clinical trial register search URL found")

# test if query= is added
expect_equal(
  suppressMessages(
    ctrGetQueryUrlFromBrowser(
      url = "query=cancer&status=completed",
      register = "EUCTR")),
  suppressMessages(
    ctrGetQueryUrlFromBrowser(
      url = "cancer&status=completed",
      register = "EUCTR"))
)

#### ctrOpenSearchPagesInBrowser ####

if (!at_home()) exit_file("Reason: not at_home")
if (Sys.getenv("ON_APPVEYOR") != "") exit_file("Reason: on Appveyor")
if (!check_internet()) exit_file("Reason: no internet connectivity")

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
    url = q))

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
