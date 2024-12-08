## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

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

#### typeField ####

df <- data.frame(
  "var2" = 2:5,
  "var1" = c(1, "2", "NA", NA),
  stringsAsFactors = FALSE)

expect_true(all(is.na(
  ctrdata:::typeField(df[[2]], "anyname")[3:4])))


#### dfMergeVariablesRelevel ####

df1 <- data.frame(
  "var1" = 1:3,
  "var2" = 2:4,
  stringsAsFactors = FALSE)

statusvalues <- list(
  "Firstvalues" = c("12", "23"),
  "Lastvalue"   = c("34"))

df2 <- data.frame(
  var1 = c("A", "B", "C", NA),
  var2 = c("D", "E", "",  "G"))

# test
expect_error(
  suppressWarnings(
    dfMergeVariablesRelevel(list("var1", "var2"))),
  "no appli")

# test
expect_message(
  suppressWarnings(
    dfMergeVariablesRelevel(
      df = df1,
      colnames = c("var1", "var2"))),
  "More than one column had values, returning")

# test
expect_true(
  "character" %in% class(
    suppressWarnings(
      suppressMessages(
        dfMergeVariablesRelevel(
          df = df1,
          colnames = c("var1", "var2"))))))

# test
expect_equal(
  suppressWarnings(
    nchar(dfMergeVariablesRelevel(
      df = df1,
      colnames = c("var1", "var2")))),
  c(5, 5, 5))

# test
expect_error(
  suppressWarnings(
    dfMergeVariablesRelevel(
      df = cbind(df1, df1),
      colnames = 1:3)))

# test
expect_error(
  suppressWarnings(
    dfMergeVariablesRelevel(
      df = df1,
      colnames = c("var1", "var2"),
      levelslist = 1:2)),
  "number of levels differs")

# test
expect_equal(
  sum(grepl(" / ", suppressWarnings(suppressMessages(
    dfMergeVariablesRelevel(
      df = df2, colnames = c("var1", "var2")))))), 2L)

# test
expect_equal(
  sum(grepl(" / ", suppressWarnings(suppressMessages(
    dfMergeVariablesRelevel(
      df = df2,
      colnames = 'matches("var")'))))), 2L)


#### ctrGetQueryUrl ####

try(clipr::clear_clip(allow_non_interactive = TRUE), silent = TRUE)

# see also test_ctrdata_param_checks.R

expect_equal(
  suppressWarnings(ctrGetQueryUrl(
    "ThisDoesNotExist")),
  NULL)

q <- "https://classic.clinicaltrials.gov/ct2/results?type=Intr&cond=cancer&age=0"

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
    url = "https://classic.clinicaltrials.gov/ct2/show/results/NCT00031447"),
  "Found search query from CTGOV")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://classic.clinicaltrials.gov/ct2/show/NCT04372602?cond=COVID-19"),
  "but also had search")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://classic.clinicaltrials.gov/ct2/show/results/NCT04372602?cond=COVID-19"),
  "but also had search")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://classic.clinicaltrials.gov/ct2/show/record/NCT04372602?cond=COVID-19"),
  "but also had search")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://clinicaltrials.gov/search?cond=neuroblastoma&intr=Investigational%20drug&aggFilters=ages:child,status:com&rank=8&sort=EnrollmentCount%3Adesc%2CNumArmGroups"),
  "REST API")

# test
expect_message(
  ctrGetQueryUrl(
    url = "cond=neuroblastoma&intr=Investigational%20Drug&aggFilters=ages%3Achild%2Cstatus%3Acom&sort=EnrollmentCount%3Adesc%2CNumArmGroups",
    register = "CTGOV"),
  "REST API")

# test
expect_message(
  ctrGetQueryUrl(
    url =
      "https://clinicaltrials.gov/study/NCT01467986?cond=neuroblastoma&intr=Investigational%20drug&aggFilters=ages:child,status:com&rank=2"),
  "REST API")

# test
expect_message(
  ctrGetQueryUrl(
    url = "https://classic.clinicaltrials.gov/ct2/results?cond=neuroblastoma&rslt=With&recrs=e&age=0&intr=Drug"),
  "Found search query from CTGOV")

# test
expect_message(
  ctrGetQueryUrl(
    url = "cond=Neuroblastoma&aggFilters=ages:child,results:with,studyType:int",
    register = "CTGOV"),
  "REST API")

# test
expect_equal(
  ctrdata:::ctgovVersion(
    url = data.frame(`no-query-term` = "something", check.names = FALSE),
    register = "inout"),
  "inout")

# test
expect_equal(
  ctrdata:::ctgovVersion(
    url = data.frame(`query-term` = "something", check.names = FALSE),
    register = "inout"),
  "inout")


#### ctrOpenSearchPagesInBrowser ####

if (!at_home()) exit_file("Reason: not at_home")
if (!checkInternet()) exit_file("Reason: no internet connectivity")

# test
expect_message(
  ctrOpenSearchPagesInBrowser(url = q),
  "Found search query")

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
expect_true(
  ctrOpenSearchPagesInBrowser(
    copyright = TRUE))

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "https://clinicaltrials.gov/expert-search?term=heart attack AND AREA[LocationCountry]United States AND AREA[LocationStatus]Recruiting"
  ), "https://clinicaltrials.gov/expert-search?term=heart attack AND AREA[LocationCountry]United States AND AREA[LocationStatus]Recruiting")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "https://www.clinicaltrials.gov/search?cond=neuroblastoma&intr=Investigational%20drug&aggFilters=ages:child,status:com&rank=200&sort=EnrollmentCount%3Adesc%2CNumArmGroups"
  ), "https://clinicaltrials.gov/search?cond=neuroblastoma&intr=Investigational drug&aggFilters=ages:child,status:com")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "https://www.clinicaltrials.gov/study/NCT01467986?cond=neuroblastoma&intr=Investigational%20drug&aggFilters=ages:child,status:com&rank=2"
  ), "https://clinicaltrials.gov/study/NCT01467986#main-content")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "https://euclinicaltrials.eu/ctis-public/view/2023-508508-39-01"
  ), "https://euclinicaltrials.eu/ctis-public/search?searchCriteria={\"number\":\"2023-508508-39-01\"}")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "https://www.clinicaltrialsregister.eu/ctr-search/trial/2015-005219-34/DE"
  ), "https://www.clinicaltrialsregister.eu/ctr-search/search?query=2015-005219-34#tabs")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "https://www.isrctn.com/ISRCTN54460428?q=alzheimer&filters=&sort=&offset=29&totalResults=286&page=3&pageSize=10"
  ), "https://www.isrctn.com/ISRCTN54460428")

# test
expect_equal(
  ctrOpenSearchPagesInBrowser(
    url = "",
    register = "CTGOV2"
  ), TRUE)


#### ctrLoadQueryIntoDb parameters ####

if (checkInternet()) {

  # test
  expect_warning(
    ctrLoadQueryIntoDb(
      queryterm = "somethingnonexisting",
      register = "EUCTR",
      euctrresultsfilespath = "something",
      only.count = TRUE
    ), "deprecated"
  )

  # test
  expect_warning(
    ctrLoadQueryIntoDb(
      queryterm = "somethingnonexisting",
      register = "EUCTR",
      euctrresultspdfpath = "something",
      only.count = TRUE
    ), "deprecated"
  )

  # test
  expect_warning(
    ctrLoadQueryIntoDb(
      queryterm = "somethingnonexisting",
      register = "EUCTR",
      parallelretrievals = 99L,
      only.count = TRUE
    ), "ignored"
  )

}

# test
expect_error(
  ctrLoadQueryIntoDb(
    queryterm = "somethingnonexisting",
    querytoupdate = 1L,
  ), "cannot"
)

# test
expect_error(
  ctrLoadQueryIntoDb(
    queryterm = "somethingnonexisting",
    register = 5L
  ), "does not seem"
)

# test
expect_error(
  ctrLoadQueryIntoDb(
    queryterm = "*",
    register = "CTGOV"
  ), "string"
)


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
dL2 <- dfTrials2Long(
  df = dF[, 4:1]
)

# test
expect_equivalent(
  dL[order(dL$name), ],
  dL2[order(dL2$name), ]
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

#### ctrShowOneTrial ####

id <- "NCT00617929"
id <- "2012-003632-23"
id <- "80181452"
id <- "2022-501142-30-00"

df <- ctrShowOneTrial(identifier = id)

expect_true(is.list(df))
expect_length(df, 1L)
