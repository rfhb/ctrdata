## RH 2019-09-28

# set server
httr::set_config(httr::timeout(seconds = 60))

#### ctrLoadQueryIntoDb ####

# test
expect_equal(
  suppressMessages(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "CTGOV",
      only.count = TRUE))[["n"]], 1L)

# test
nodbi::docdb_create(
  src = dbc,
  key = dbc$collection,
  value = data.frame()
)
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        querytoupdate = "last",
        con = dbc))),
  "no previous queries")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "SHOULDNOTEXISTATALL",
      register = "CTGOV",
      con = dbc)),
  "found 0 trials")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "CTGOV",
      con = dbc)),
  "Imported or updated 1 trial")

# test
expect_equal(tmpTest$n, 1L)

# test
expect_equal(tmpTest$success, "NCT01471782")

# test
expect_true(length(tmpTest$failed) == 0L)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "NCT01471782",
      register = "CTGOV",
      con = dbc)),
  "Imported or updated 1 trial")


# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = paste0(
          "https://classic.clinicaltrials.gov/ct2/results?cond=Cancer&type=Intr&phase=0",
          "&strd_s=01%2F02%2F2005&strd_e=12%2F31%2F2017"),
        con = dbc))),
  "more than 10,000) trials")

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "someQueryForErrorTriggering",
      register = "CTGOV",
      verbose = TRUE,
      only.count = TRUE,
      con = dbc)),
  "term=someQueryForErrorTriggering")


#### ctrLoadQueryIntoDb update ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      forcetoupdate = TRUE,
      con = dbc)),
  "Imported or updated ")

# test
expect_error(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = 999L,
      con = dbc)),
  "'querytoupdate': specified query number.*not found")

# new query
q <- paste0(
  "https://classic.clinicaltrials.gov/ct2/results?",
  "term=osteosarcoma&type=Intr&phase=0&age=0&lup_e=")

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = paste0(q, "12%2F31%2F2008"),
      con = dbc)),
  "Imported or updated ")

#### ctrLoadQueryIntoDb results ####

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  RSQLite::SQLITE_RO)

# get results
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c(
        "primary_outcome.measure",
        "start_date",
        "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
        "clinical_results.baseline.group_list.group",
        "clinical_results.baseline.analyzed_list.analyzed.units",
        "clinical_results.outcome_list.outcome",
        "clinical_results",
        "study_design_info.allocation",
        "eligibility.maximum_age",
        "location.facility.name",
        "location"
      ),
      con = dbc)
  ))

# test
expect_equal(
  rev(sort(sapply(
    result[["location"]],
    function(x) {
      if (all(is.na(x))) return(0L) else
        length(x[["facility"]][["name"]])
    })))
  [1:2], c(12, 7))

# test
expect_true("character" == class(result[[
  "study_design_info.allocation"]]))

# test
expect_true("character" == class(result[[
  "primary_outcome.measure"]]))

# test
expect_true(
  any(grepl(" / ", result[["primary_outcome.measure"]])))

# test
expect_true("Date" == class(result[[
  "start_date"]]))

# test
expect_true("difftime" == class(result[[
  "eligibility.maximum_age"]]))

# test
expect_true(
  any(grepl(" / ", result[["location.facility.name"]])))

# test
expect_true(
  length(unlist(strsplit(
    result[["location.facility.name"]], " / "))) >= 22L)

# test
expect_true("list" == class(result[[
  "clinical_results.baseline.group_list.group"]]))

# convert to long
df <- suppressMessages(
  dfTrials2Long(
    df = result
  ))

# test
expect_identical(
  names(df),
  c("_id", "identifier", "name", "value")
)

# test
expect_true(
  nrow(df) > 1000L
)

# test
expect_error(
  suppressWarnings(
    suppressMessages(
      ctrLoadQueryIntoDb(
        queryterm = "term=ET743OVC3006",
        register = "CTGOV",
        annotation.text = "something",
        annotation.mode = "WRONG",
        con = dbc))),
  "'annotation.mode' incorrect")

#### documents.path ####

if (!length(dbc$url) || grepl("localhost", dbc$url)) {
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "cond=Neuroblastoma&type=Intr&recrs=e&phase=0&u_prot=Y&u_sap=Y&u_icf=Y",
        register = "CTGOV",
        documents.path = newTempDir(),
        documents.regexp = ".*",
        con = dbc
      )),
    "Newly saved [0-9]+ document"
  )
}

#### dbFindFields ####

# test
expect_equal(
  suppressMessages(
    suppressWarnings(
      dbFindFields(
        namepart = "thisdoesnotexist",
        con = dbc))),
  "")

# get all field names
tmpFields <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = ".*",
      con = dbc)))

# test
expect_true(
  length(tmpFields) > 150L)

#### dbGetFieldsIntoDf ####

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[seq_along(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

tmpFields <- tmpFields[grepl("date$", tmpFields, ignore.case = TRUE)]
tmpFields <- tmpFields[1:min(seq_along(tmpFields), 49L)]

tmpData <- dbGetFieldsIntoDf(fields = tmpFields, con = dbc)
expect_true(nrow(tmpData) > 0L)
expect_true(ncol(tmpData) > 0L)

expect_true(all(
  unique(unlist(
    lapply(
      tmpData[, -1, drop = FALSE],
      function(i) sapply(i, function(ii) class(ii)))
  )) %in%
    c("Date", "POSIXct", "POSIXt")
))

# determine all classes
# tmpr <- names(result)
# tmpr <- tmpr[tmpr != "_id"]
# tmpc <- sapply(result, class, USE.NAMES = FALSE)
# tmpc <- unlist(tmpc)
# tmpc <- table(tmpc)
