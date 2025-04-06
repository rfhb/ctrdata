## RH 2023-03-25

#### ctrLoadQueryIntoDb ####

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAny":"neuroblastoma"}',
      only.count = TRUE,
      verbose = TRUE,
      con = dbc)[["n"]] >= 10L))

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAny":"neuroblastoma"}',
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
  "Imported .* trial")

#### ctrLoadQueryIntoDb update ####

hist <- dbQueryHistory(con = dbc)

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = TRUE,
      only.count = TRUE,
      con = dbc)),
  "Imported .* trial")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = TRUE,
      only.count = FALSE,
      con = dbc)),
  "updating")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = FALSE,
      only.count = TRUE,
      con = dbc)),
  "Imported .* trial")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = FALSE,
      only.count = FALSE,
      con = dbc)),
  "updating")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

# test full load
hist <- hist[nrow(hist), ]
hist[1L, "query-timestamp"] <- "2025-01-01 00:00:00"
json <- jsonlite::toJSON(list("queries" = hist))
expect_equal(
  nodbi::docdb_update(
    src = dbc,
    key = dbc$collection,
    value = as.character(json),
    query = '{"_id": "meta-info"}'), 1L)

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = TRUE,
      only.count = TRUE,
      con = dbc)),
  "Imported .* trial")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = FALSE,
      only.count = TRUE,
      con = dbc)),
  "Imported .* trial")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

expect_warning(
  suppressMessages(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = TRUE,
      only.count = FALSE,
      con = dbc)),
  "updating")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == hist[1L, 4L])

dF <- dbGetFieldsIntoDf(c(
  "lastUpdated",
  "history.history_version.version_date"), con = dbc)
expect_inherits(dF[[2]], "Date")
expect_inherits(dF[[3]], "Date")

rm(tmpTest, dF)

#### annotating ####

# test
expect_message(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAny":"neuroblastoma"}',
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

# get some more trials
tmp <- ctrLoadQueryIntoDb(
  queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAny":"cancer","status":[8]}',
  con = dbc
)
# test
expect_true(tmp$n > 125L)

# get all field names
tmpFields <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = ".*",
      con = dbc,
      sample = FALSE)))

# test
expect_true(
  length(tmpFields) > 950L)

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
groupsNo <- groupsNo[seq_along(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

# dates
tmpFields <- tmpFields[
  (grepl("[.]date$", tmpFields, ignore.case = TRUE) |
     grepl("^startDateEU$", tmpFields, ignore.case = TRUE) |
     grepl("Date$", tmpFields, ignore.case = FALSE)) &
    !grepl("^decisionDate$", tmpFields, ignore.case = FALSE)
]

if (FALSE) {
  # debug
  for (f in sort(tmpFields)) message('"', f, '" = "ctrDate",')
}

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[seq_along(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
  #
  tmpClass <- lapply(
    tmpData[, -1, drop = FALSE],
    function(i) sapply(i, function(ii) class(ii))[1])
  tmpClass <- names(tmpClass[sapply(tmpClass, function(c) c == "character")])
  if (length(tmpClass)) print(tmpClass)
  #
  expect_true(all(
    unique(unlist(
      lapply(
        tmpData[, -1, drop = FALSE],
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
expect_true(length(res) >= 150L)


#### documents.path ####

tmpDir <- newTempDir()
on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

if (!length(dbc$url) || grepl("localhost", dbc$url)) {
  expect_message(
    suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={%22containAny%22:%22antibody%22,%22status%22:[8],%22ageGroupCode%22:[2]}',
        documents.path = tmpDir,
        documents.regexp = "icf",
        con = dbc
      )),
    "Newly saved [0-9]+ document"
  )
}


# overview of types of documents
if (FALSE) {

  library(dplyr)
  tmp <- dir(path = tmpDir, full.names = TRUE, recursive = TRUE)
  tmp <- as_tibble(tmp)
  tmp %>%
    mutate(
      size = file.size(value),
      value = sub(paste0(tmpDir, "/"), "", value),
      title = sub("^[0-9-]+/", "", value),
      ctrnumber = sub("^(.+?)/.+", "\\1", value),
      part = sub("(.+?) - .+", "\\1", title)
    ) -> tmp

  tmp %>%
    count(part) %>%
    arrange(desc(n)) %>%
    print(n = 100L)

}
