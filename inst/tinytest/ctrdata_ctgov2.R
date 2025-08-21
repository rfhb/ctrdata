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
&resFirstPost=_2030-01-01
&lastUpdPost=1990-01-01_2030-01-01
&studyComp=1990-01-01_
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
tmp1 <- tmp1[grepl("API call: ", tmp1)]
tmp1 <- sub("^.*(API call: )", "\\1", tmp1)

refapicall <- "API call: https://www.clinicaltrials.gov/api/v2/studies?format=json&countTotal=true&pageSize=1&filter.advanced=AREA[MinimumAge]RANGE[18d, MAX] AND AREA[MaximumAge]RANGE[MIN, 64y] AND AREA[StudyFirstPostDate]RANGE[1990-01-01,2030-01-01] AND AREA[LastUpdatePostDate]RANGE[1990-01-01,2030-01-01] AND AREA[PrimaryCompletionDate]RANGE[1990-01-01,2030-01-01] AND AREA[ResultsFirstPostDate]RANGE[MIN,2030-01-01] AND AREA[StartDate]RANGE[1990-01-01,2030-01-01] AND AREA[CompletionDate]RANGE[1990-01-01,MAX]&query.locn=AREA[LocationFacility]MyFacility,AREA[LocationCountry]United States&aggFilters=funderType:industry,phase:2,results:without,status:rec act,studyType:int exp exp_indiv,violation:y&query.cond=cancer&query.id=NCT05429502&query.intr=Investigational drug&query.lead=MySponsorLead&query.outc=MyOutComeMeasure&query.spons=MySponsor&query.term=krebs&query.titles=MyAcronym"
# utils::browseURL(sub("API call: ", "", refapicall))

hlpSplit <- function(x) {strsplit(x, "&| AND ")[[1]]}

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
expect_true(tmp$n > 40L && tmp$n < 50L)

#### documents.path ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "cond=Cancer&aggFilters=phase:0,status:ter,studyType:int&studyComp=2015-12-31_2020-12-31",
      register = "CTGOV",
      documents.path = tmpDir,
      documents.regexp = NULL,
      verbose = TRUE,
      con = dbc
    )),
  "Newly saved [0-9]+ placeholder document[(]s[)] for [0-9]+ trial"
)

# test
expect_true(
  length(
    dir(pattern = ".pdf", path = tmpDir, recursive = TRUE)) > 10L
)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "cond=Cancer&aggFilters=phase:0,status:ter,studyType:int&studyComp=2015-12-31_2020-12-31",
      register = "CTGOV2",
      documents.path = tmpDir,
      documents.regexp = "sap_",
      verbose = TRUE,
      con = dbc
    )),
  "Newly saved [0-9]+ document[(]s[)] for [0-9]+ trial"
)

#### ctgov2history ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://clinicaltrials.gov/search?cond=neuroblastoma&aggFilters=phase:3,status:com",
      ctgov2history = 3,
      con = dbc
    )),
  "processing historic versions"
)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://clinicaltrials.gov/search?cond=neuroblastoma&aggFilters=phase:3,status:com",
      ctgov2history = "2:4",
      con = dbc
    )),
  "processing historic versions"
)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://clinicaltrials.gov/search?cond=neuroblastoma&aggFilters=phase:3,status:com",
      ctgov2history = -1,
      con = dbc
    )),
  "processing historic versions"
)

#### ctrLoadQueryIntoDb update ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = 2L,
      con = dbc)),
  "Rerunning query")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "cond=Neuroblastoma&lastUpdPost=2022-01-01_2023-12-31&aggFilters=phase:1,results:with,studyType:int",
      register = "CTGOV2",
      con = dbc,
      verbose = TRUE
    )),
  "Imported or updated [0-9]+ trial"
)

# test
expect_warning(
  ctrLoadQueryIntoDb(
    querytoupdate = "last",
    con = dbc
  ),
  "running again with these limits"
)

# test
tmp <- dbQueryHistory(con = dbc)
expect_equal(dim(tmp), c(10L, 4L))

#### dbFindFields ####

tmpFields <- dbFindFields(namepart = ".*", con = dbc, sample = FALSE)
expect_true(length(tmpFields) > 340L)

#### dbGetFieldsIntoDf ####

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[seq_along(tmpFields)]

expect_message(
  dbGetFieldsIntoDf(fields = tmpFields[1:50], con = dbc),
  "specify fewer than 50"
)

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

tmpFields <- tmpFields[grepl("date$", tmpFields, ignore.case = TRUE)]
tmpFields <- tmpFields[1:min(seq_along(tmpFields), 49L)] # 36

tmpData <- dbGetFieldsIntoDf(fields = tmpFields, con = dbc)
expect_true(nrow(tmpData) > 0L)
expect_true(ncol(tmpData) > 0L)

expect_true(all(
  unique(unlist(
    lapply(
      tmpData[, -1, drop = FALSE],
      function(i) sapply(i, function(ii) class(ii)))
  )) %in% c("Date", "POSIXct", "POSIXt")
))

expect_error(
  dbGetFieldsIntoDf(fields = c("nonexistingfield", "anothernonexisting"), con = dbc),
  "No records with values"
)
