## RH 2023-08-11

# setup
source("setup_ctrdata.R")
if (!at_home()) exit_file("Reason: not at_home")

# tests
tf <- function() {

  tmpDir <- tempdir()

  fileFrom <- dir(
    # path = "./inst/tinytest", # only for direct debugging
    pattern = "test_error_trials_euctr.txt",
    full.names = TRUE)

  fileTo <- tempfile(
    pattern = "euctr_trials_",
    tmpdir = tmpDir,
    fileext = ".txt")

  on.exit(suppressWarnings(file.remove(fileTo)), add = TRUE)
  on.exit(suppressWarnings(file.remove(
    dir(path = tmpDir, pattern = ".ndjson", full.names = TRUE))), add = TRUE)

  if (!file.copy(fileFrom, fileTo, overwrite = TRUE))
    exit_file("Could not copy trial text file")

  i <- fileTo
  ct <- V8::v8()
  ct$eval(readr::read_file(system.file("js/euctr2ndjson.js", package = "ctrdata")))

  # start copy from ctrLoadQueryIntoEuctr.R but
  # change .ctrdataenv$ct$call to ct$call

  readr::write_file(
    ct$call("euctr2ndjson", readr::read_file(i),
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    file = sub("[.]txt$", ".ndjson", i))

  # end copy from ctrLoadQueryIntoEuctr.R

  # introduce error at JSON_ERROR_HERE in the NDJSON file
  i <- fileTo
  o <- sub("[.]txt$", ".ndjson_error", i)
  i <- sub("[.]txt$", ".ndjson", i)
  i <- file(i, open = "rt", blocking = TRUE)
  unlink(o)
  o <- file(o, open = "at")
  while (TRUE) {
    l <- readLines(con = i, n = 1L, warn = FALSE)
    if (length(l) == 0L || !nchar(l)) break
    l <- sub("JSON_ERROR_HERE", ' ERROR_TRIGGER-->" ', l)
    writeLines(l, con = o)
  }
  close(i)
  close(o)
  i <- fileTo
  o <- sub("[.]txt$", ".ndjson_error", i)
  i <- sub("[.]txt$", ".ndjson", i)
  file.copy(from = o, to = i, overwrite = TRUE)

  # setup
  if (!checkSqlite()) exit_file("Reason: no SQLite")

  dbc <- suppressWarnings(
    nodbi::src_sqlite(
      dbname = ":memory:",
      collection = "mycoll")
  )
  on.exit(expr = {
    try({
      if (DBI::dbExistsTable(conn = dbc$con, name = dbc$collection))
        DBI::dbRemoveTable(conn = dbc$con, name = dbc$collection)
      RSQLite::dbDisconnect(conn = dbc$con)
      rm(dbc)
    },
    silent = TRUE)
  }, add = TRUE)

  # test
  expect_warning(
    res <- ctrdata:::dbCTRLoadJSONFiles(
      dir = tmpDir, con = dbc, verbose = FALSE),
    "Failed to load")

  # test
  expect_equivalent(sort(res$failed), sort(c("2022-002568-62-ES", "2021-002179-21-FR")))
  expect_true(res$n == length(res$success))
  expect_true(res$n == length(res$annotations))
  expect_true(res$n == 8L)

  # test
  expect_warning(
    res <- ctrdata:::dbCTRLoadJSONFiles(
      dir = tmpDir, con = dbc, verbose = FALSE),
    "Multiple trial records.*2022-002568-62-SE.*2021-002179-21-PL")

  # test
  suppressWarnings(
    expect_message(
      ctrdata:::dbCTRAnnotateQueryRecords(
        recordnumbers = res$success,
        recordannotations = res$annotations,
        annotation.text = "my_anno",
        annotation.mode = "append",
        con = dbc,
        verbose = FALSE
      ), "Annotated retrieved records [(]8 records[)]"
    )
  )

  res <- suppressWarnings(
    suppressMessages(
      dbGetFieldsIntoDf(
        c("_id", "annotation"),
        con = dbc)
    )
  )

  # test
  expect_true(all(res$annotation == "my_anno"))
  # since two trials are twice each (duplicates)
  expect_true(length(res$annotation) == 8L - 2L)
  expect_true(nrow(res) == 8L - 2L)

  # end tests
}
tf()
