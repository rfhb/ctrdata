## RH 2023-08-05

# set server
httr::set_config(httr::timeout(seconds = 60))

#### ctrLoadQueryIntoDb ####

# test
tmp <- ctrLoadQueryIntoDb(
  queryterm = "cond=neuroblastoma&intr=Investigational%20Drug&aggFilters=ages%3Achild%2Cstatus%3Acom&sort=EnrollmentCount%3Adesc%2CNumArmGroups",
  register = "CTGOV",
  con = dbc
)
expect_true(tmp$n >= 6L)
expect_true(all(c("NCT00152126", "NCT00578864", "NCT01467986", "NCT01492673", "NCT02130869", "NCT03042429") %in% tmp$success))

# test
tmp <- ctrLoadQueryIntoDb(
  queryterm = "https://www.clinicaltrials.gov/search?cond=Cancer&aggFilters=phase:0,studyType:int",
  con = dbc
)
expect_true(tmp$n > 1400L)

#### documents.path ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

# test
expect_message(
  ctrLoadQueryIntoDb(
    queryterm = "cond=Cancer&aggFilters=phase:0,studyType:int",
    register = "CTGOV",
    documents.path = tmpDir,
    documents.regexp = NULL,
    verbose = TRUE,
    con = dbc
  ),
  "Newly saved [0-9]+ placeholder document[(]s[)] for [0-9]+ trial"
)

# test
expect_message(
  ctrLoadQueryIntoDb(
    queryterm = "cond=Cancer&aggFilters=phase:0,studyType:int",
    register = "CTGOV",
    documents.path = tmpDir,
    documents.regexp = "sap_",
    verbose = TRUE,
    con = dbc
  ),
  "Newly saved [0-9]+ document[(]s[)] for [0-9]+ trial"
)

#### ctrLoadQueryIntoDb update ####

# test
expect_warning(
  ctrLoadQueryIntoDb(
    querytoupdate = "last",
    con = dbc),
  "No trials found"
)

# test
tmp <- dbQueryHistory(con = dbc)
expect_equal(dim(tmp), c(5L, 4L))

#### dbFindFields ####

tmpFields <- dbFindFields(namepart = ".*", con = dbc)
expect_true(length(tmpFields) > 140L)

#### dbGetFieldsIntoDf ####

tmpData <- dbGetFieldsIntoDf(fields = tmpFields, con = dbc)
expect_true(object.size(tmpData) > 100000000L)

tmpData <- dbGetFieldsIntoDf(fields = tmpFields[grepl("date$",tmpFields)], con = dbc)
expect_true(all(sapply(tmpData[, -1, drop = FALSE], class, USE.NAMES = FALSE) == "Date"))
