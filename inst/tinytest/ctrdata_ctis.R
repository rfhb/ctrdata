## RH 2023-03-25

#### ctrLoadQueryIntoDb ####

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"","containAny":"neuroblastoma","containNot":""}',
      only.count = TRUE,
      verbose = TRUE,
      con = dbc)[["n"]] >= 10L))

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"","containAny":"neuroblastoma","containNot":""}',
      verbose = TRUE,
      con = dbc)),
  "Imported .* updated ")

# test
expect_true(tmpTest$n >= 10L)

# test
expect_true(all(c("2023-503684-42-00", "2024-512095-35-00") %in% tmpTest$success))

# test
expect_true(length(tmpTest$failed) == 0L)

# clean up
rm(tmpTest)

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


#### documents.path ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

if (!length(dbc$url) || grepl("localhost", dbc$url)) {
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"","containAny":"cancer","containNot":""}',
        documents.path = tmpDir,
        documents.regexp = ".*",
        con = dbc
      )),
    "Newly saved [0-9]+ document"
  )
}

#### ctrLoadQueryIntoDb update ####

#### annotating ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"","containAny":"neuroblastoma","containNot":""}',
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
  length(tmpFields) > 800L)

# debug
if (FALSE){

  # debug
  View(data.frame(
    register = names(tmpFields),
    field = tmpFields))

  # debug
  for (f in sort(tmpFields[grepl("[.]inclu", tmpFields)])) message(
    '"', f, '" = "ctrFalseTrue",')
  for (f in sort(tmpFields[grepl("[.]has", tmpFields)])) message(
    '"', f, '" = "ctrFalseTrue",')
  for (f in sort(tmpFields[grepl("[.]is", tmpFields)])) message(
    '"', f, '" = "ctrFalseTrue",')

  # debug
  for (f in sort(tmpFields[grepl("number", tmpFields, ignore.case = TRUE)])) message(
    '"', f, '" = "ctrInt",')

  # debug
  for (f in sort(tmpFields[
    grepl("count", tmpFields, ignore.case = TRUE) &
    !grepl("country|countries", tmpFields, ignore.case = TRUE)])) message(
      '"', f, '" = "ctrInt",')

  # debug list top level fields
  tmpFields[!grepl("[.]", tmpFields)]

}

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

if (FALSE) {
  # debug
  for (f in sort(tmpFields)) message('"', f, '" = "ctrDate",')
}

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
  # unique(unlist(
  #   lapply(
  #     tmpData[ , -1, drop = FALSE],
  #     function(i) sapply(i, function(ii) class(ii))[1])
  # )) %in% c("Date", "POSIXct", "POSIXt")
  #
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
