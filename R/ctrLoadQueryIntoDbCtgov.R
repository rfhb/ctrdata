### ctrdata package

#' ctrLoadQueryIntoDbCtgov
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON stream_in
#' @importFrom httr content GET status_code
#' @importFrom nodbi docdb_query
#' @importFrom curl multi_download
#' @importFrom jqr jq jq_flags
#'
ctrLoadQueryIntoDbCtgov <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    documents.path,
    documents.regexp,
    annotation.text,
    annotation.mode,
    only.count,
    con,
    verbose,
    queryupdateterm) {

  ## ctgov api ----------------------------------------------------------------

  # CTGOV standard identifiers
  # updated 2017-07 with revised ctgov website links, e.g.
  # "https://classic.clinicaltrials.gov/ct2/results/download_studies?
  # rslt=With&cond=Neuroblastoma&age=0&draw=3"
  queryUSRoot   <- "https://classic.clinicaltrials.gov/"
  queryUSType1  <- "ct2/results/download_studies?"
  queryUSType2  <- "ct2/results?"

  ## inform user and prepare url for downloading
  message("* Checking trials in CTGOV classic...")
  ctgovdownloadcsvurl <- paste0(
    queryUSRoot, queryUSType1, "&", queryterm, queryupdateterm)
  #
  if (verbose) message("DEBUG: ", ctgovdownloadcsvurl)

  ## checks -------------------------------------------------------------------

  # check number of trials to be downloaded
  ctgovdfirstpageurl <- paste0(
    queryUSRoot, queryUSType2, "&", queryterm, queryupdateterm)
  #
  # do get
  tmp <- try(httr::GET(
    url = utils::URLencode(ctgovdfirstpageurl)),
    silent = TRUE)
  #
  if (inherits(tmp, "try-error") ||
      !any(httr::status_code(tmp) == c(200L, 404L))) {
    stop("Host ", queryUSRoot, " not working as expected, ",
         "cannot continue: ", tmp[[1]], call. = FALSE)
  }
  #
  tmp <- httr::content(tmp, as = "text")
  tmp <- sub(".*[> ](.*?) Stud(y|ies) found for: .*", "\\1", tmp)
  tmp <- sub("^No$", "0", tmp)

  # safeguard against no or unintended large numbers
  tmp <- suppressWarnings(as.integer(tmp))
  if (is.na(tmp) || !length(tmp) || !tmp) {
    message("Search result page empty - no (new) trials found?")
    return(invisible(emptyReturn))
  }

  # inform user
  message("Retrieved overview, records of ", tmp, " ",
          "trial(s) are to be downloaded (estimate: ",
          format(tmp * 0.008, digits = 2), " MB)")

  # only count?
  if (only.count) {

    # return
    return(list(n = tmp,
                success = NULL,
                failed = NULL))
  }

  # exit if too many records
  if (tmp > 10000L) {
    stop("These are ", tmp, " (more than 10,000) trials, this may be ",
         "unintended. Downloading more than 10,000 trials may not be supported ",
         "by the register; consider correcting or splitting queries")
  }

  ## download -----------------------------------------------------------------

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  # prepare a file handle for temporary directory
  f <- file.path(
    tempDir, paste0("ctgov_",
    # include query in file name for potential re-download
    sapply(ctgovdownloadcsvurl, digest::digest, algo = "crc32"),
    ".zip"))

  # inform user
  message("(1/3) Downloading trial file...")

  # get (download) trials in single zip file f
  tmp <- ctrMultiDownload(ctgovdownloadcsvurl, f, verbose = verbose)

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    stop("No studies downloaded. Please check 'queryterm' or run ",
         "again with verbose = TRUE", call. = FALSE)
  }

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## convert to json ----------------------------------------------------------

  if (length(.ctrdataenv$ct) == 0L) initTranformers()

  # run in batches of 25
  xmlFileList <- dir(path = tempDir, pattern = "NCT.+xml", full.names = TRUE)
  numInterv <- 1L + ((length(xmlFileList) - 1L) %/% 25)
  if (numInterv > 1L) {
    xmlFileList <- split(xmlFileList, cut(seq_along(xmlFileList), numInterv))
  } else {
    xmlFileList <- list(xmlFileList)
  }

  ## run conversion (235 trial records in ~10 s)
  message("(2/3) Converting to NDJSON (estimate: ",
          signif(length(unlist(xmlFileList)) * 12 / 235, 1L), " s)...")

  for (f in seq_along(xmlFileList)) {

    fNdjsonCon <- file(file.path(tempDir, paste0("ctgov_trials_", f, ".ndjson")), open = "at")
    on.exit(try(close(fNdjsonCon), silent = TRUE), add = TRUE)

    for (i in xmlFileList[[f]]) {

      jqr::jq(
        # input
        textConnection(
          .ctrdataenv$ct$call(
            "parsexml",
            # read source xml file
            paste0(readLines(i, warn = FALSE), collapse = ""),
            # https://www.npmjs.com/package/xml2js#options
            V8::JS('{trim: true, ignoreAttrs: false, mergeAttrs: true,
                   explicitRoot: true, explicitArray: false}'))
        ),
        # processing
        paste0(
          # extract trial record(s)
          " .clinical_study ",
          # mangle _ and attributes which only some study records have, e.g.
          # {"start_date":{"_":"March 15, 2004","type":"Actual"}} becomes
          # {"start_date":"March 15, 2004"}, dropping the information if
          # the field holds "Estimated" or "Actual" information
          '| "completion_date"         as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          '| "enrollment"              as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          '| "last_update_posted"      as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          '| "primary_completion_date" as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          '| "results_first_posted"    as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          '| "start_date"              as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          '| "study_first_posted"      as $p | if(.[$p] | type == "object") then .[$p] = .[$p]._ else . end',
          # add elements
          '| .["_id"] = .id_info.nct_id
           | .["ctrname"] = "CTGOV"
           | .["record_last_import"] = "', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '"'
        ),
        flags = jqr::jq_flags(pretty = FALSE),
        out = fNdjsonCon
      )

    } # for i

    close(fNdjsonCon)

  } # for f

  ## import -------------------------------------------------------------------

  ## run import
  message("(3/3) Importing records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## documents ----------------------------------------------------------------

  if (!is.null(documents.path)) {

    # temporary file for trial ids and file names
    downloadsNdjson <- file.path(tempDir, "ctgov_downloads.ndjson")
    suppressMessages(unlink(downloadsNdjson))
    downloadsNdjsonCon <- file(downloadsNdjson, open = "at")
    on.exit(try(close(downloadsNdjsonCon), silent = TRUE), add = TRUE)
    on.exit(try(unlink(downloadsNdjson), silent = TRUE), add = TRUE)

    # extract trial ids and file name and save in temporary file
    for (ndjsonFile in dir(
      path = tempDir, pattern = "^ctgov_trials_[0-9]+[.]ndjson$", full.names = TRUE)) {
      jqr::jq(
        file(ndjsonFile),
        "._id as $trialid |
        .provided_document_section.provided_document[] |
        { _id: $trialid, url: .document_url }",
        flags = jqr::jq_flags(pretty = FALSE),
        out = downloadsNdjsonCon)
      message(". ", appendLF = FALSE)
    }
    close(downloadsNdjsonCon)

    # get document trial id and file name
    dlFiles <- jsonlite::stream_in(file(downloadsNdjson), verbose = FALSE)

    # check if any documents
    if (!nrow(dlFiles)) {
      message("= No documents identified for downloading.")
    } else {

      # calculate filename
      dlFiles$filename <- sub("^.+/(.+?)$", "\\1", dlFiles$url)

      # do download
      resFiles <- ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path, documents.regexp, verbose)

    } # if (!nrow(dlFiles))

  } # !is.null(documents.path)

  ## delete for any re-downloads
  try(unlink(dir(
    path = tempDir, pattern = "ctgov_trials_[0-9]+.ndjson",
    full.names = TRUE)), silent = TRUE)

  ## inform user -----------------------------------------------------

  # find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtgov
