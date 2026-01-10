## RH 2023-03-25

# remove any existing database
nodbi::docdb_delete(dbc, dbc$collection)

#### ctrLoadQueryIntoDb ####

# test
expect_true(
  suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAny":"neuroblastoma"}',
      only.count = TRUE,
      verbose = TRUE,
      con = dbc)[["n"]] >= 29))

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAny":"neuroblastoma"}',
      verbose = TRUE,
      con = dbc)),
  "Imported .* updated ")

# test
expect_true(tmpTest$n >= 29L)

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
      con = dbc))[["n"]] >= 0L)

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
expect_true(tmpTest$queryterm == rev(hist[["query-term"]])[1L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = TRUE,
      only.count = FALSE,
      con = dbc)),
  "updat")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == rev(hist[["query-term"]])[1L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = FALSE,
      only.count = TRUE,
      con = dbc)),
  "Imported .* trial")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == rev(hist[["query-term"]])[1L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = FALSE,
      only.count = FALSE,
      con = dbc)),
  "updat")
expect_true(tmpTest$n >= 0L)
expect_true(tmpTest$queryterm == rev(hist[["query-term"]])[1L])

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
expect_true(tmpTest$n >= 10L)
expect_true(tmpTest$queryterm == hist[["query-term"]][1L])

expect_message(
  suppressWarnings(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = FALSE,
      only.count = TRUE,
      con = dbc)),
  "Imported .* trial")
expect_true(tmpTest$n >= 10L)
expect_true(tmpTest$queryterm == hist[["query-term"]][1L])

dF <- dbGetFieldsIntoDf(c(
  "lastUpdated",
  "history.history_version.version_date"), con = dbc)
expect_inherits(dF[[2]], "Date")
expect_inherits(dF[[3]], "Date")

expect_warning(
  suppressMessages(
    tmpTest <- ctrLoadQueryIntoDb(
      querytoupdate = 1L,
      ctishistory = TRUE,
      only.count = FALSE,
      con = dbc)),
  "iteratively")
expect_true(tmpTest$n >= 30L)
expect_true(tmpTest$queryterm == hist[["query-term"]][1L])

dF <- dbGetFieldsIntoDf(c(
  "lastUpdated",
  "history.history_version.version_date"), con = dbc)
expect_inherits(dF[[2]], "Date")

rm(tmpTest, dF, hist)

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

#
expect_true(sum(tmpDf$totalNumberEnrolled, na.rm = TRUE) > 2900L)

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
expect_true(tmp$n > 200L)

# get all field names
tmpFields <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = ".*",
      con = dbc,
      sample = FALSE)))

# test
expect_true(
  length(tmpFields) >= 960L)

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

# tmpFields include fields from historic versions, for
# which however no typing is provided at this time, thus
# these fields are excluding from type testing below
#
# View(data.frame(tmpFields))

expect_true(sum(grepl("^history", tmpFields)) > 800L)
tmpFields <- tmpFields[!grepl("^history", tmpFields)]

groupsNo <- (length(tmpFields) %/% 49L) + 1L
groupsNo <- rep(seq_len(groupsNo), 49L)
groupsNo <- groupsNo[seq_along(tmpFields)]

for (i in unique(groupsNo)) {
  message(i, " ", appendLF = FALSE)
  tmpData <- dbGetFieldsIntoDf(fields = tmpFields[groupsNo == i], con = dbc)
  expect_true(nrow(tmpData) > 0L)
  expect_true(ncol(tmpData) > 0L)
}

# continue with dates only
tmpFields <- tmpFields[
  (grepl("[.]date$", tmpFields, ignore.case = TRUE) |
     grepl("^startDateEU$", tmpFields, ignore.case = TRUE) |
     grepl("Date$", tmpFields, ignore.case = FALSE)) &
    # "decisionDate" cannot be typed, is a string concatenating dates
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

# prepare testing

if (FALSE) {

  ctrLoadQueryIntoDb(
    'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"status":[11]}',
    con = dbc)

  ids <- nodbi::docdb_query(
    src = dbc,
    key = dbc$collection,
    query = '{}',
    fields = '{"_id":1}'
  )$`_id`

  ids <- sort(ids)

  base <- sub("-[0-9][0-9]$", "", ids[grepl("[1-9]$", ids)])

  ids[sapply(ids, function(i) any(sub("-[0-9][0-9]$", "", i) == base))]

  #  [1] "2022-501312-34-00" "2022-501312-34-01" "2022-501417-31-00"
  #  [4] "2022-501417-31-01" "2022-501694-39-01" "2022-502177-42-01"
  #  [7] "2022-502968-20-01" "2023-503362-24-01" "2023-503373-37-01"
  # [10] "2023-503617-30-01" "2023-503813-31-01" "2023-504246-64-00"
  # [13] "2023-504246-64-02" "2023-504439-42-00" "2023-504439-42-01"
  # [16] "2023-505244-18-01" "2023-505916-40-01" "2023-506019-16-01"
  # [19] "2023-507344-36-01" "2023-507573-17-01" "2024-510792-38-00"
  # [22] "2024-510792-38-01" "2024-511889-36-01" "2024-512754-16-00"
  # [25] "2024-512754-16-01" "2024-514586-20-01" "2024-514794-22-01"
  # [28] "2024-514979-17-04" "2024-515420-37-01" "2024-516004-42-01"
  # [31] "2024-518036-36-03" "2024-518228-63-01" "2024-519089-32-02"
  # [34] "2025-523374-17-01"

}

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 440L)

# test
expect_false(any(
  res %in% c(
    "2022-501312-34-00", "2022-501417-31-00", "2023-504246-64-00",
    "2023-504439-42-00", "2024-510792-38-00", "2024-512754-16-00"
  )
))


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

  ctrLoadQueryIntoDb(
    queryterm = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria={%22containAny%22:%22antibody%22,%22status%22:[8],%22ageGroupCode%22:[2]}',
    documents.path = tmpDir,
    documents.regexp = ".*",
    con = dbc
  )

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
  # # A tibble: 8 × 2
  # part             n
  # <chr>        <int>
  #   1 Protocol       168
  # 2 SbjctInfaICF   130
  # 3 SynpssofthPr    37
  # 4 RcrtmntArrng    21
  # 5 LyprsnsSmmoR    16
  # 6 SmmryofPrdcC     8
  # 7 SmmryofRslts     6
  # 8 ClnclStdyRpr     4

}
