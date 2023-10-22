## RH 2023-08-05

# set server
httr::set_config(httr::timeout(seconds = 60))

#### url to api translation ####

queryterm <- "
https://clinicaltrials.gov/search?
&locn=MyFacility
&locStr=USA
&country=United%20States
&distance=50
&ageRange=18d_64y
&cond=cancer
&term=krebs
&intr=Investigational%20drug
&start=1990-01-01_2030-01-01
&primComp=1990-01-01_2030-01-01
&firstPost=1990-01-01_2030-01-01
&aggFilters=
funderType:industry,
phase:2,
results:without,
status:rec%20act,
studyType:int%20exp%20exp_indiv,
violation:y
&resFirstPost=1990-01-01_2030-01-01
&lastUpdPost=1990-01-01_2030-01-01
&studyComp=1990-01-01_2030-01-01
&titles=MyAcronym
&outc=MyOutComeMeasure
&spons=MySponsor
&lead=MySponsorLead
&id=NCT05429502
"
queryterm <- gsub("\n", "", queryterm)
# utils::browseURL(queryterm)

tmp1 <-
  capture.output(
    tmp2 <- ctrLoadQueryIntoDb(
      queryterm = queryterm,
      only.count = TRUE,
      verbose = TRUE
    ),
    type = "message"
  )
tmp1 <- tmp1[startsWith(tmp1, "API call: ")]

refapicall <- "API call: https://www.clinicaltrials.gov/api/v2/studies?format=json&countTotal=true&pageSize=1&filter.advanced=AREA[MinimumAge]RANGE[18d, MAX] AND AREA[MaximumAge]RANGE[MIN, 64y] AND AREA[StudyFirstPostDate]RANGE[1990-01-01,2030-01-01] AND AREA[LastUpdatePostDate]RANGE[1990-01-01,2030-01-01] AND AREA[PrimaryCompletionDate]RANGE[1990-01-01,2030-01-01] AND AREA[ResultsFirstPostDate]RANGE[1990-01-01,2030-01-01] AND AREA[StartDate]RANGE[1990-01-01,2030-01-01] AND AREA[CompletionDate]RANGE[1990-01-01,2030-01-01]&query.locn=AREA[LocationCountry]United States,AREA[LocationFacility]MyFacility,AREA[LocationCity]USA&aggFilters=funderType:industry,phase:2,results:without,status:rec act,studyType:int exp exp_indiv,violation:y&query.cond=cancer&query.id=NCT05429502&query.intr=Investigational drug&query.lead=MySponsorLead&query.outc=MyOutComeMeasure&query.spons=MySponsor&query.term=krebs&query.titles=MyAcronym"
# utils::browseURL(sub("API call: ", "", refapicall))

hlpSplit <- function(x) strsplit(x, "&| AND |,[^0-9]", fixed = FALSE)[[1]]

# test
expect_equal(setdiff(hlpSplit(tmp1), hlpSplit(refapicall)), character(0L))

# test
expect_equal(tmp2$n, 1L)


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
  queryterm = "https://www.clinicaltrials.gov/search?cond=Cancer&aggFilters=phase:0,status:ter,studyType:int&studyComp=_2015-12-31",
  con = dbc
)
expect_true(tmp$n > 130L)

#### documents.path ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

# test
expect_message(
  ctrLoadQueryIntoDb(
    queryterm = "cond=Cancer&aggFilters=phase:0,status:ter,studyType:int&studyComp=_2015-12-31",
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
    queryterm = "cond=Cancer&aggFilters=phase:0,status:ter,studyType:int&studyComp=_2015-12-31",
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

tmpData <- suppressMessages(dbGetFieldsIntoDf(fields = tmpFields, con = dbc))
expect_true(object.size(tmpData) > 10000000L)

tmpData <- suppressMessages(dbGetFieldsIntoDf(fields = tmpFields[grepl("date$",tmpFields)], con = dbc))
expect_true(all(sapply(tmpData[, -1, drop = FALSE], class, USE.NAMES = FALSE) == "Date"))
