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
  expect_message(
    installCygwinWindowsDoInstall(),
    "cygwin is already installed")
}


#### dfMergeTwoVariablesRelevel ####

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(list("var1", "var2"))),
  "Need a data frame as input.")

# test
expect_message(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df,
      colnames = c("var1", "var2"))),
  "Unique values returned: 1, 2, 3")

# test
expect_true(
  "integer" %in% class(
    suppressWarnings(
      suppressMessages(
        dfMergeTwoVariablesRelevel(
          df = df,
          colnames = c("var1", "var2"))))))

# test
expect_message(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df,
      colnames = c("var1", "var2"),
      levelslist = statusvalues)),
  "Unique values returned: 1, 2, 3")

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df,
      colnames = 1:3)),
  "Please provide exactly two column names.")

# test
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df,
    varnames = c("var1", "var2")),
  "Parameter varnames is deprecated, use colnames instead")

# test
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df,
    varnames = c("var1", "var2")),
  "Some rows had values for both columns")

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df,
      colnames = c("var1", "var2"),
      levelslist = 1:2)),
  "Need list for parameter 'levelslist'")


#### ctrGetQueryUrl ####

expect_equal(
  suppressWarnings(ctrGetQueryUrl(
    "ThisDoesNotExist")),
  NULL)

q <- "https://clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

tmpTest <- suppressMessages(
  ctrGetQueryUrl(
    url = q))

# test
expect_true("data.frame" %in% class(tmpTest))

# test
expect_warning(
  ctrGetQueryUrl(
    url = "ThisDoesNotExist"),
  "no clinical trial register search URL found")

# test if query= is added
expect_equal(
  suppressMessages(
    ctrGetQueryUrl(
      url = "query=cancer&status=completed",
      register = "EUCTR")),
  suppressMessages(
    ctrGetQueryUrl(
      url = "cancer&status=completed",
      register = "EUCTR"))
)

# test
expect_warning(
  ctrGetQueryUrlFromBrowser(
    url = "ThisDoesNotExist"),
  "is deprecated")

# test
expect_error(
  ctrGetQueryUrl(
    url = LETTERS),
  "'url' and / or 'register' is not")

# test
expect_error(
  ctrGetQueryUrl(
    url = "LETTERS",
    register = LETTERS),
  "'url' and / or 'register' is not")

# test
expect_warning(
  ctrGetQueryUrl(
    url = "https://something",
    register = "CTGOV"),
  "no clinical trial register search URL")

#### ctrOpenSearchPagesInBrowser ####

if (!at_home()) exit_file("Reason: not at_home")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(input = q),
  "Found search query")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(input = tmpTest),
  "Opening browser for search:")

q <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/",
            "search?query=&age=under-18&status=completed")

tmpTest <- suppressMessages(
  ctrGetQueryUrl(
    url = q))

# test
expect_true("data.frame" %in% class(tmpTest))

# test
expect_message(
  ctrOpenSearchPagesInBrowser(q),
  "Found search query")

# test
expect_true(
  ctrOpenSearchPagesInBrowser(register = ""))

# test
expect_message(
  ctrOpenSearchPagesInBrowser(q),
  "Opening browser for search:")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(tmpTest),
  "Opening browser for search:")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    register = c("EUCTR", "CTGOV"),
    copyright = TRUE),
  TRUE)
