## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")


#### binaries ####

expect_message(
  ctrdata:::checkBinary(b = "notworking"),
  "nonexistingbinarytested not found")

expect_error(
  ctrdata:::checkCommand(),
  "Empty argument: commandtest")


#### cache ####

expect_equal(
  suppressMessages(
    ctrdata:::ctrCache(
      xname = "shouldNotExist",
      verbose = TRUE
    )), NULL)

expect_equal(
  suppressMessages(
    ctrdata:::ctrCache(
      xname = "shouldNotExist",
      xvalue = iris,
      verbose = TRUE
    )), iris)

expect_equal(
  suppressMessages(
    ctrdata:::ctrCache(
      xname = "shouldNotExist",
      verbose = TRUE
    )), iris)


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


#### typeField ####

df <- data.frame(
  "var2" = 2:5,
  "var1" = c(1, "2", "NA", NA),
  stringsAsFactors = FALSE)

expect_true(all(is.na(
  ctrdata:::typeField(df[[2]], "anyname")[3:4])))


#### dfMergeTwoVariablesRelevel ####

df1 <- data.frame(
  "var1" = 1:3,
  "var2" = 2:4,
  stringsAsFactors = FALSE)

statusvalues <- list(
  "Firstvalues" = c("12", "23"),
  "Lastvalue"   = c("34"))

df2 <- data.frame(var1 = c("A", "B", "C", "D"),
                  var2 = c("D", "E", "",  "G"))

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(list("var1", "var2"))),
  "Need a data frame as input.")

# test
expect_message(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df1,
      colnames = c("var1", "var2"))),
  "Unique values returned: 1, 2, 3")

# test
expect_true(
  "integer" %in% class(
    suppressWarnings(
      suppressMessages(
        dfMergeTwoVariablesRelevel(
          df = df1,
          colnames = c("var1", "var2"))))))

# test
expect_message(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df1,
      colnames = c("var1", "var2"),
      levelslist = statusvalues)),
  "Unique values returned: 1, 2, 3")

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df1,
      colnames = 1:3)),
  "Please provide exactly two column names.")

# test
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df1,
    varnames = c("var1", "var2")),
  "Parameter varnames is deprecated, use colnames instead")

# test
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df1,
    colnames = c("var1", "var2")),
  "Some rows had non-character values for both columns, used first")

# test
expect_error(
  suppressWarnings(
    dfMergeTwoVariablesRelevel(
      df = df1,
      colnames = c("var1", "var2"),
      levelslist = 1:2)),
  "Need list for parameter 'levelslist'")

# test
expect_warning(
  dfMergeTwoVariablesRelevel(
    df = df2,
    colnames = c("var1", "var2")),
  "Some rows had character values for both columns, concatenated")
expect_equal(
  sum(grepl(" / ", suppressWarnings(suppressMessages(
    dfMergeTwoVariablesRelevel(
      df = df2, colnames = c("var1", "var2")))))), 3L)


#### ctrGetQueryUrl ####

# see also test_ctrdata_param_checks.R

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

# test
expect_error(
  ctrGetQueryUrl(
    url = c("https://onething", "https://something"),
    register = "CTGOV"),
  "is not a single character string")

# test
expect_error(
  ctrGetQueryUrl(
    url = "",
    register = "EUCTR"),
  "no clinical trial register search URL found")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://clinicaltrials.gov/ct2/show/results/NCT00031447"),
  "Found search query from CTGOV")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://clinicaltrials.gov/ct2/show/NCT04372602?cond=COVID-19"),
  "but also had search")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://clinicaltrials.gov/ct2/show/results/NCT04372602?cond=COVID-19"),
  "but also had search")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://clinicaltrials.gov/ct2/show/record/NCT04372602?cond=COVID-19"),
  "but also had search")

# test
expect_error(
  ctrGetQueryUrl(
    url = "https://beta.clinicaltrials.gov/search?cond=NCT04412252,%20NCT04368728"),
  "beta website")


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
  ctrOpenSearchPagesInBrowser(tmpTest),
  paste0(q, "#tabs"))

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

#### dfName2Value ####

expect_error(
  suppressMessages(
    dfName2Value(
      df = iris,
      valuename = "something")),
  "'df' does not seem to come from dfTrials2Long()")

expect_error(
  suppressMessages(
    dfName2Value(
      df = iris,
      valuename = "")),
  "'valuename' must be specified")

#### dfTrials2Long ####

dF <- data.frame(
  "_id" = paste0("NCT1234567", 1:5),
  alpha1 = 1:5,
  beta = 1:5,
  gamma_1 = 1:5,
  check.names = FALSE
)
dL <- suppressMessages(
  dfTrials2Long(dF)
)

# test
expect_equal(nrow(dL), 15L)
expect_equal(unique(dL[["name"]]), names(dF)[-1])

dF <- suppressMessages(
  dfName2Value(
    df = dL,
    valuename = "alp.*"))

# test
expect_equal(
  nrow(dF), 5L)
expect_equal(
  names(dF),
  c("_id", "identifier", "name", "value"))

dF <- suppressMessages(
  dfName2Value(
    df = dL,
    valuename = "[mp]",
    wherename = "[a]",
    wherevalue = "[3-4]"))

# test
expect_equal(
  nrow(dF), 4L)
expect_equal(
  names(dF),
  c("_id", "identifier", "name", "value", "where"))
expect_true(
  all(df[["name"]] %in% c("alpha1", "gamma_1")))
expect_true(
  all(df[["value"]] %in% c(3, 4)))

# cleanup
rm(dF, dL)
