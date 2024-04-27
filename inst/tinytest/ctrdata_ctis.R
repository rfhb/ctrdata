## RH 2023-03-25

#### ctrLoadQueryIntoDb ####

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "ageGroupCode=2",
      register = "CTIS",
      only.count = TRUE,
      verbose = TRUE,
      con = dbc)[["n"]] >= 13L))

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "ageGroupCode=2",
      register = "CTIS",
      verbose = TRUE,
      con = dbc)),
  "Imported / updated ")

# test
expect_true(tmpTest$n >= 13L)

# test
expect_true(all(c("2023-504071-24-00", "2022-502144-12-00") %in% tmpTest$success))

# test
expect_true(length(tmpTest$failed) == 0L)

# clean up
rm(tmpTest)

# test
expect_true(suppressWarnings(
  ctrLoadQueryIntoDb(
    queryterm = "https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup?basicSearchInputAND=cancer&msc=528",
    con = dbc))[["n"]] >= 9L)

# test
expect_true(suppressWarnings(
  ctrLoadQueryIntoDb(
    queryterm = "https://euclinicaltrials.eu/app/#/search?basicSearchInputAND=cancer&msc=528",
    con = dbc))[["n"]] >= 9L)

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "basicSearchInputAND=infection&status=Ended",
      register = "CTIS",
      con = dbc))[["n"]] >= 2L)

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      con = dbc))[["n"]] >= 2L)

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      querytoupdate = "last",
      only.count = TRUE,
      con = dbc)),
  "[0-9]+ trials have been updated")

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://euclinicaltrials.eu/app/#/search?number=2022-500271-31-00",
      con = dbc))[["n"]] == 1L)

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://euclinicaltrials.eu/app/#/search?number=2022-501559-99-00",
      con = dbc))[["n"]] == 1L)


#### documents.path ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

if (!length(dbc$url) || grepl("localhost", dbc$url)) {
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = "basicSearchInputAND=leukemia",
        register = "CTIS",
        documents.path = tmpDir,
        con = dbc
      )),
    "Newly saved [1-9][0-9]+ document"
  )
}

#### ctrLoadQueryIntoDb update ####

#### annotating ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup?basicSearchInputAND=cancer&msc=528",
      annotation.text = "just_this",
      annotation.mode = "replace",
      con = dbc)),
  "Annotated retrieved records [(][0-9]+")

#### dbGetFieldsIntoDf ####

res <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = c("annotation"),
      verbose = TRUE,
      con = dbc)
  ))

# test
expect_true(
  all(res[, "annotation", drop = TRUE] == "just_this"))

# clean up
rm(res)

# test
suppressWarnings(
  suppressMessages(
    tmpDf <- dbGetFieldsIntoDf(
      fields = c(
        "totalNumberEnrolled"
      ), con = dbc)))
#
expect_equivalent(
  sapply(tmpDf, typeof),
  c("character", "integer")
)

# clean up
rm(tmpDf)

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
      con = dbc,
      sample = FALSE)))

# test
expect_true(
  length(tmpFields) > 3000L)

#### dbGetFieldsIntoDf ####

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[1:length(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

tmpFields <- tmpFields[
  grepl("date$", tmpFields, ignore.case = TRUE) &
    !grepl("update$", tmpFields, ignore.case = TRUE) &
    !grepl("^decisionDate$", tmpFields, ignore.case = TRUE)
]

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[1:length(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
  #
  tmpClass <- lapply(
    tmpData[ , -1, drop = FALSE],
    function(i) sapply(i, function(ii) class(ii))[1])
  tmpClass <- names(tmpClass[sapply(tmpClass, function(c) c == "character")])
  if (length(tmpClass)) print(tmpClass)
  #
  expect_true(all(
    unique(unlist(
      lapply(
        tmpData[ , -1, drop = FALSE],
        function(i) sapply(i, function(ii) class(ii)))
    )) %in% c("Date", "POSIXct", "POSIXt")
  ))
}

#### dbFindIdsUniqueTrials ####

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 20L)
