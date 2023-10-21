### ctrdata package

#' ctrLoadQueryIntoDbCtgov
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON stream_in
#' @importFrom httr content GET status_code config set_config
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

  # set configuration option
  httr::set_config(httr::config(forbid_reuse = 1))

  # check number of trials to be downloaded
  ctgovdfirstpageurl <- paste0(
    queryUSRoot, queryUSType2, "&", queryterm, queryupdateterm)
  #
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
  f <- file.path(tempDir, "ctgov.zip")

  # inform user
  message("(1/3) Downloading trial file...")

  # get (download) trials in single zip file f
  tmp <- ctrMultiDownload(ctgovdownloadcsvurl, f)

  # inform user
  if (!file.exists(f) || file.size(f) == 0L) {
    stop("No studies downloaded. Please check 'queryterm' or run ",
         "again with verbose = TRUE", call. = FALSE)
  }

  ## extract all from downloaded zip file
  utils::unzip(f, exdir = tempDir)

  ## run conversion
  message("(2/3) Converting to JSON...", appendLF = FALSE)
  tmp <- ctrConvertToJSON(tempDir, "ctgov2ndjson.php", verbose)

  ## run import
  message("(3/3) Importing JSON records into database...")
  if (verbose) message("DEBUG: ", tempDir)
  imported <- dbCTRLoadJSONFiles(dir = tempDir,
                                 con = con,
                                 verbose = verbose)

  ## find out number of trials imported into database
  message("= Imported or updated ", imported$n, " trial(s)")


  ## documents -----------------------------------------------------------------

  ## save any documents
  if (!is.null(documents.path)) {

    # check and create directory
    createdDir <- try(
      dir.create(documents.path, recursive = TRUE, showWarnings = FALSE),
      silent = TRUE)
    if (inherits(createdDir, "try-errror")) {
      warning("Directory could not be created for 'documents.path' ",
              documents.path, ", cannot download files", call. = FALSE)
    } else {

      # continue after if
      message("Downloading documents into 'documents.path' = ", documents.path)

      # canonical directory path
      documents.path <- normalizePath(documents.path, mustWork = TRUE)
      if (createdDir) message("- Created directory ", documents.path)

      # get documents urls, file names
      fDocsOut <- file.path(tempDir, "ctgov_docs.ndjson")
      unlink(fDocsOut)
      for (f in dir(path = tempDir, pattern = "^ctgov_trials_[0-9]+[.]ndjson$", full.names = TRUE)) {
        cat(jqr::jq(
          file(f),
          ' { _id: ._id, docs: [ .provided_document_section.provided_document[].document_url ] } ',
          flags = jqr::jq_flags(pretty = FALSE)
        ),
        sep = "\n",
        file = fDocsOut,
        append = TRUE)
      }

      # create directory per trial
      dlFiles <- jsonlite::stream_in(file(fDocsOut), verbose = FALSE)
      invisible(sapply(
        dlFiles[["_id"]], function(i) {
          d <- file.path(documents.path, i)
          if (!dir.exists(d))
            dir.create(d, showWarnings = FALSE, recursive = TRUE)
        }))

      if (!nrow(dlFiles)) {

        message("No documents for downloading identified.")

      } else {

        # create data frame with file info
        dlFiles <- apply(dlFiles, 1, function(r) {
          data.frame(url = unlist(r[-1], use.names = TRUE), r[1],
                     check.names = FALSE, stringsAsFactors = FALSE)
        })
        dlFiles <- do.call(rbind, dlFiles)
        dlFiles$filename <- sub("^.+/(.+?)$", "\\1", dlFiles$url)
        dlFiles$destfile <- file.path(
          documents.path, dlFiles$`_id`, dlFiles$filename)
        dlFiles$exists <- file.exists(dlFiles$destfile) &
          file.size(dlFiles$destfile) > 10L

        if (is.null(documents.regexp)) {

          message("Creating empty document placeholders (max. ", nrow(dlFiles), ")")

          # create empty files
          tmp <-
            sapply(
              dlFiles$destfile,
              function(i) if (!file.exists(i))
                file.create(i, showWarnings = TRUE),
              USE.NAMES = FALSE)

          tmp <- sum(unlist(tmp), na.rm = TRUE)

        } else {

          message("Applying 'documents.regexp' to ",
                  nrow(dlFiles), " documents")
          dlFiles <- dlFiles[
            grepl(documents.regexp, dlFiles$filename, ignore.case = TRUE), ,
            drop = FALSE]

          # download and save
          message("Downloading ", nrow(dlFiles), " documents:")

          tmp <- ctrMultiDownload(dlFiles$url[!dlFiles$exists],
                                  dlFiles$destfile[!dlFiles$exists])

          if (!nrow(tmp)) tmp <- 0L else {

            # handle failures despite success is true
            invisible(sapply(
              tmp[tmp$status_code != 200L, "destfile", drop = TRUE], unlink
            ))

            tmp <- nrow(tmp[tmp$status_code == 200L, , drop = FALSE])

          }

        } # if documents.regexp

        message(sprintf(paste0(
          "Newly saved %i ",
          ifelse(is.null(documents.regexp), "placeholder ", ""),
          "document(s) for %i trial(s); ",
          "%i document(s) for %i trial(s) already existed in %s"),
          tmp,
          length(unique(dlFiles$`_id`)),
          sum(dlFiles$fileexists),
          length(unique(dlFiles$`_id`[dlFiles$fileexists])),
          documents.path
        ))

      } # if !nrow

    } # if documents.path available

  } # if documents.path

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtgov
