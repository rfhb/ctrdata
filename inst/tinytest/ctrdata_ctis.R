## RH 2023-03-25

#### ctrLoadQueryIntoDb ####

# test
expect_message(
  tmpTest <- suppressWarnings(
    ctrLoadQueryIntoDb(
      queryterm = "",
      register = "CTIS",
      verbose = TRUE,
      con = dbc)),
  "Imported / updated ")

# test
expect_true(tmpTest$n >= 130L)

# test
expect_true(all(c("2022-500657-17-00", "2022-501537-23-00") %in% tmpTest$success))

# test
expect_true(length(tmpTest$failed) == 0L)

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
expect_true(suppressWarnings(
  ctrLoadQueryIntoDb(
    queryterm = "basicSearchInputAND=infection&status=Ended",
    register = "CTIS",
    con = dbc))[["n"]] >= 2L)

# clean up
rm(tmpTest)

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
expect_error(
  suppressMessages(
    suppressWarnings(
      dbGetFieldsIntoDf(
        fields = c("doesnotexist"),
        con = dbc))),
  "No data could be extracted for")

# test
expect_warning(
  suppressMessages(
    dbGetFieldsIntoDf(
      fields = c("doesnotexist"),
      stopifnodata = FALSE,
      con = dbc)
  ),
  "No records with values for any specified field")

# test
suppressWarnings(
  suppressMessages(
    tmpDf <- dbGetFieldsIntoDf(
      fields = c(
        "totalNumberEnrolled"
        ),
      stopifnodata = FALSE,
      con = dbc)))
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
tmpf <- suppressMessages(
  suppressWarnings(
    dbFindFields(
      namepart = ".*",
      con = dbc)))

# test
expect_true(
  length(tmpf) > 1500L)

# get all data
result <- suppressMessages(
  suppressWarnings(
    dbGetFieldsIntoDf(
      fields = sample(tmpf, 20),
      con = dbc,
      verbose = FALSE,
      stopifnodata = FALSE)
  ))

# test
expect_true(
  length(names(result)) > 20L)

rm(tmpf, result)

#### dbFindIdsUniqueTrials ####

expect_message(
  res <- suppressWarnings(
    dbFindIdsUniqueTrials(con = dbc)),
  " [0-9]+ records")

# test
expect_true(length(res) >= 135L)
rm(res)

