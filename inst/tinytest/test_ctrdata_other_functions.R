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

#### binaries ####

expect_message(
  ctrdata:::checkBinary(b = "notworking"),
  "nonexistingbinarytested not found")

expect_error(
  ctrdata:::installFindBinary(),
  "Empty argument: commandtest")


#### environment ####

if (.Platform$OS.type != "windows") {
  expect_error(
    installCygwinWindowsDoInstall(),
    "only for MS Windows")
  expect_message(
    ctrdata:::installCygwinWindowsTest(),
    "only for MS Windows")
}
if (.Platform$OS.type == "windows") {
  expect_message(
    installCygwinWindowsDoInstall(),
    "cygwin seems to work correctly")
  expect_message(
    installCygwinWindowsDoInstall(force = TRUE),
    "cygwin seems to work correctly")
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
    colnames = c("var1", "var2")),
  "Some rows had values for both columns, used first")

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df,
      colnames = c("var1", "var2"),
      levelslist = 1:2)),
  "Need list for parameter 'levelslist'")

# test
df <- data.frame(var1 = c("A", "B", "C", "D"),
                 var2 = c("D", "E", "F", "G"))
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df,
    colnames = c("var1", "var2")),
  "Some rows had values for both columns, concatenated")


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
  ctrGetQueryUrl(
    url = "LETTERS"),
  "no clinical trial register search URL found")

# test
expect_warning(
  ctrGetQueryUrl(
    url = "LETTERS",
    register = "LETTERS"),
  "no clinical trial register search URL found")

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
  ctrOpenSearchPagesInBrowser(url = q),
  "Found search query")

# test
expect_warning(
  ctrOpenSearchPagesInBrowser(input = tmpTest),
  "Parameter 'input' is deprecated, use 'url' instead")

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
expect_message(
  ctrOpenSearchPagesInBrowser(q),
  "Found search query")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(tmpTest), q)

# test
expect_null(
  ctrOpenSearchPagesInBrowser(
    copyright = TRUE))

#### dfFindUniqueEuctrRecord ####

# test
expect_error(
  ctrdata:::dfFindUniqueEuctrRecord(
    df = list(1L)),
  "Parameter df is not a data frame"
)
# test
expect_error(
  ctrdata:::dfFindUniqueEuctrRecord(
    df = iris),
  "Data frame does not include"
)
# test
expect_error(
  ctrdata:::dfFindUniqueEuctrRecord(
    df = data.frame(
      "_id" = "something",
      "a2_eudract_number" = "something",
      check.names = FALSE),
    prefermemberstate = "something"),
  "Value specified for prefermemberstate does not match"
)

#### df mangling ####

expect_error(
  dfName2Value(
    df = iris,
    valuename = "something"),
  "'df' does not seem to come from dfTrials2Long()")

expect_error(
  dfName2Value(
    df = iris,
    valuename = ""),
  "'valuename' must be specified")

expect_error(
  ctrdata:::typeField(
    dfi = iris),
  "Expect data frame with two columns, _id and a field")
