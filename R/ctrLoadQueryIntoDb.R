### ctrdata package

#' Load and store register trial information
#'
#' Retrieves information on clinical trials from registers
#' and stores it in a collection in a database. Main function
#' of \link{ctrdata} for accessing registers.
#' A collection can store trial information from different queries
#' or different registers. Query details are stored in the
#' collection and can be accessed using \link{dbQueryHistory}.
#' A previous query can be re-run, which replaces or adds trial
#' records while keeping any user annotations of trial records.
#'
#' @param queryterm Either a string with the full URL of a search
#' query in a register, or the data frame returned by
#' \link{ctrGetQueryUrl} or \link{dbQueryHistory},
#' or an `_id` in the format of one of the trial registers,
#' or, together with \code{register}, a string with query
#' elements of a search URL. The query details are recorded in the
#' \code{collection} for later use, e.g. to update records.
#' For "CTIS", \code{queryterm} can be an empty string to obtain
#' all trial records. For automatically copying the user's
#' query of a register in a web browser to the clipboard, see
#' \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}
#'
#' @param register String with abbreviation of register to query,
#' either "EUCTR", "CTGOV2", "ISRCTN" or "CTIS". Not needed
#' if \code{queryterm} has a full URL to query results,
#' or has a single trial identifier, or comes from
#' \link{ctrGetQueryUrl} or \link{dbQueryHistory}.
#'
#' @param querytoupdate Either the word "last", or the row number of
#' a query in the data frame returned by \link{dbQueryHistory} that
#' should be run to retrieve any new or update trial records since
#' this query was run the last time.
#' This parameter takes precedence over \code{queryterm}.
#' For "EUCTR" and "CTIS", updates are available only for the last seven days;
#' the query is run again if more time has passed since it was run last.
#'
#' @param forcetoupdate If \code{TRUE}, run again the query
#' given in \code{querytoupdate}, irrespective of when it was
#' run last. Default is \code{FALSE}.
#'
#' @param documents.path If this is a relative or absolute
#' path to a directory that exists or can be created,
#' save any documents into it that are directly available from
#' the register ("EUCTR", "CTGOV2", "ISRCTN", "CTIS")
#' such as PDFs on results, analysis plans, spreadsheets,
#' patient information sheets, assessments or product information.
#' Default is \code{NULL}, which disables saving documents.
#' For "EUCTR", sets \code{euctrresults = TRUE} since documents
#' are available only with results.
#'
#' @param documents.regexp Regular expression, case insensitive,
#' to select documents by filename, if saving documents is requested
#' (see \code{documents.path}).
#' If set to \code{NULL}, empty placeholder files are saved for
#' every document that could be saved, which is useful to get an overview
#' on the number and types of documents available for download.
#' Default is
#' \code{"prot|sample|statist|sap_|p1ar|p2ars|icf|ctalett|lay|^[0-9]+ "}.
#' Used with "CTGOV2", "ISRCTN" and "CTIS" (for "EUCTR", all documents
#' are downloaded since they are few and have non-canonical filenames.)
#'
#' @param euctrresults If \code{TRUE}, also load available
#' results when retrieving and loading trials from EUCTR. This
#' slows down this function. (For "CTGOV2" and "CTIS",
#' available results are always retrieved and loaded into the
#' collection.)
#'
#' @param euctrresultshistory If \code{TRUE}, load results and also
#' the available history of results publication in "EUCTR."
#' This somewhat time-consuming. Default is \code{FALSE}.
#'
#' @param euctrprotocolsall If \code{TRUE}, load all available records of
#' protocol-related data (that is, versions from all EU Member States and any
#' third country where the trial is conducted); if \code{FALSE}, only a single
#' record per trial is loaded, to accelerate loading. Default is \code{TRUE}
#'
#' @param ctgov2history For trials from CTGOV2, retrieve historic
#' versions of the record. Default is \code{FALSE}, because this
#' is a time-consuming operation. Use
#' \code{n} for n from all versions (recommended),
#' \code{1} for the first (original) version,
#' \code{-1} for the last-but-one version,
#' \code{"n:m"} for the nth to the mth versions, or
#' \code{TRUE} for all versions
#' of the trial record to be retrieved. Note that for register
#' CTIS, historic versions were available in the `applications`
#' field only before the register's relaunch on 2024-06-17.
#'
#' @param ctishistory If \code{TRUE}, and only when using \code{querytoupdate},
#' move the current CTIS record into an array \code{history} with the record
#' which holds one or more historic versions, before updating the rest of the
#' record from CTIS. Default is \code{FALSE}, because this is a time-consuming
#' operation. See "Historic versions" in \code{vignette("ctrdata_summarise")}.
#'
#' @param annotation.text Text to be including into the field
#' "annotation" in the records retrieved with the query
#' that is to be loaded into the collection.
#' The contents of the field "annotation" for a trial record
#' are preserved e.g. when running this function again and
#' loading a record of a with an annotation, see parameter
#' \code{annotation.mode}.
#'
#' @param annotation.mode One of "append" (default), "prepend"
#' or "replace" for new annotation.text with respect to any
#' existing annotation for the records retrieved with the query
#' that is to be loaded into the collection.
#'
#' @param only.count Set to \code{TRUE} to return only the
#' number of trial records found in the register for the query.
#' Does not load trial information into the database.
#' Default is \code{FALSE}.
#'
#' @inheritParams ctrDb
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @returns A list with elements
#' `n` (number of trial records newly imported or updated),
#' `success` (a vector of _id's of successfully loaded records),
#' `failed` (a vector of identifiers of records that failed to load)
#' and `queryterm` (the query term used).
#' The returned list has several attributes (including database and
#' collection name, as well as the query history of this database
#' collection) to facilitate documentation.
#'
#' @export
#'
#' @importFrom utils packageVersion URLencode
#'
#' @examples
#' \dontrun{
#'
#' dbc <- nodbi::src_sqlite(collection = "my_collection")
#'
#' # Retrieve protocol- and results-related information
#' # on two specific trials identified by their EU number
#' ctrLoadQueryIntoDb(
#'   queryterm = "2005-001267-63+OR+2008-003606-33",
#'   register = "EUCTR",
#'   euctrresults = TRUE,
#'   con = dbc
#' )
#'
#' # Count ongoing interventional cancer trials involving children
#' # Note this query is a classical CTGOV query and is translated
#' # to a corresponding query for the current CTGOV2 webinterface
#' ctrLoadQueryIntoDb(
#'   queryterm = "cond=cancer&recr=Open&type=Intr&age=0",
#'   register = "CTGOV",
#'   only.count = TRUE,
#'   con = dbc
#' )
#'
#' # Retrieve all information on more than 40 trials
#' # that are labelled as phase 3 and that mention
#' # either neuroblastoma or lymphoma from ISRCTN,
#' # into the same collection as used before
#' ctrLoadQueryIntoDb(
#'   queryterm = paste0(
#'     "https://www.isrctn.com/search?",
#'     "q=neuroblastoma+OR+lymphoma&filters=phase%3APhase+III"),
#'   con = dbc
#' )
#'
#' # Retrieve information trials in CTIS mentioning neonates
#' ctrLoadQueryIntoDb(
#'   queryterm = paste0("https://euclinicaltrials.eu/ctis-public/",
#'   "search#searchCriteria={%22containAll%22:%22%22,",
#'   "%22containAny%22:%22neonates%22,%22containNot%22:%22%22}"),
#'   con = dbc
#' )
#' }
#'
ctrLoadQueryIntoDb <- function(
    queryterm = NULL,
    register = "",
    querytoupdate = NULL,
    forcetoupdate = FALSE,
    euctrresults = FALSE,
    euctrresultshistory = FALSE,
    euctrprotocolsall = TRUE,
    ctgov2history = FALSE,
    ctishistory = FALSE,
    documents.path = NULL,
    documents.regexp = "prot|sample|statist|sap_|p1ar|p2ars|icf|ctalett|lay|^[0-9]+ ",
    annotation.text = "",
    annotation.mode = "append",
    only.count = FALSE,
    con = NULL,
    verbose = FALSE) {

  ## check params

  # - parameters consistent
  if (!is.null(querytoupdate) && !is.null(queryterm)) {
    stop("only one of 'queryterm' and 'querytoupdate' should be ",
         "specified, cannot continue",
         call. = FALSE
    )
  }
  if (euctrresultshistory) euctrresults <- TRUE


  ## obtain queryterm register --------------------------------------------

  # if not querytoupdate
  if (is.null(querytoupdate)) {

    # - check queryterm
    if (!is.data.frame(queryterm)) {
      # obtain url and register
      queryterm <- try(
        ctrGetQueryUrl(
          url = ifelse(is.null(queryterm), "", queryterm),
          register = register
        ),
        silent = TRUE
      )
    }

    # - deal with data frame as returned from
    #   ctrQueryHistoryInDb and ctrGetQueryUrl
    if (inherits(queryterm, "try-error") ||
        !all(substr(names(queryterm), 1, 6) == "query-") ||
        !is.data.frame(queryterm)) {
      stop("'queryterm' does not seem to result from ctrQueryHistoryInDb() ",
           "or ctrGetQueryUrl(): ", queryterm,
           call. = FALSE
      )
    }

    # - process queryterm dataframe
    nr <- nrow(queryterm)
    if (nr > 1L) {
      warning(
        "Using last row of queryterm parameter",
        call. = FALSE, immediate. = TRUE
      )
    }
    register <- queryterm[nr, "query-register", drop = TRUE]
    queryterm <- queryterm[nr, "query-term", drop = TRUE]

    # check register
    if (length(register) != 1L ||
        !all(class(register) %in% "character") ||
        is.na(register)) {
      stop("'register' has to be a non-empty string: ",
           register,
           call. = FALSE
      )
    }

    # check queryterm
    if (register != "CTIS" &&
        (length(queryterm) != 1L ||
         !all(class(queryterm) %in% "character") ||
         is.na(queryterm) ||
         nchar(queryterm) == 0L)) {
      stop("'queryterm' has to be a non-empty string: ",
           deparse(queryterm),
           call. = FALSE
      )
    }

    ## sanity checks
    if (grepl(regQueryterm, gsub(
      "\\[", "", gsub("\\]", "", utils::URLencode(queryterm))))) {
      stop("Parameter 'queryterm' has unexpected characters: ",
           queryterm, ", expected are: ",
           gsub("^\\[\\^(.+)\\]$", "\\1", regQueryterm),
           call. = FALSE
      )
    }

    # remove trailing or leading whitespace, line breaks
    queryterm <- gsub("^\\s+|\\s+$|\n|\r", "", queryterm)

  } # if not querytoupdate

  # check annotation parameters
  annotation.text <- as.character(annotation.text)
  if (annotation.text != "" &&
      !any(annotation.mode == c("append", "prepend", "replace"))) {
    stop("'annotation.mode' incorrect", call. = FALSE)
  }

  ## handle querytoupdate -----------------------------------------------------

  # initialise variable that is filled if an update is to be made
  queryupdateterm <- ""

  # rewrite parameters for running as update
  querytermoriginal <- queryterm
  if (!is.null(querytoupdate)) {
    #
    rerunparameters <- ctrRerunQuery(
      querytoupdate = querytoupdate,
      forcetoupdate = forcetoupdate,
      ctishistory = ctishistory,
      only.count = only.count,
      con = con,
      verbose = verbose,
      queryupdateterm = queryupdateterm
    )
    #
    # set main parameters
    querytermoriginal <- rerunparameters$querytermoriginal
    queryupdateterm <- rerunparameters$queryupdateterm
    queryterm <- rerunparameters$queryterm
    register <- rerunparameters$register
    failed <- rerunparameters$failed
    #
    # early exit if ctrRerunQuery failed
    if (!is.null(failed)) return(failed)
    #
  } # if querytermtoupdate

  # continue with database if needed
  if (!only.count) con <- ctrDb(con)

  ## . main function -----------------------------------------------------

  # parameters for core functions
  params <- list(
    queryterm = queryterm,
    register = register,
    euctrresults = euctrresults,
    euctrresultshistory = euctrresultshistory,
    euctrprotocolsall = euctrprotocolsall,
    ctgov2history = ctgov2history,
    ctishistory = ctishistory,
    documents.path = documents.path,
    documents.regexp = documents.regexp,
    annotation.text = annotation.text,
    annotation.mode = annotation.mode,
    only.count = only.count,
    con = con,
    verbose = verbose,
    queryupdateterm = queryupdateterm
  )

  # call core functions
  imported <- switch(
    as.character(params$register),
    "CTGOV2" = do.call(ctrLoadQueryIntoDbCtgov2, params),
    "EUCTR" = do.call(ctrLoadQueryIntoDbEuctr, params),
    "ISRCTN" = do.call(ctrLoadQueryIntoDbIsrctn, params),
    "CTIS" = do.call(ctrLoadQueryIntoDbCtis, params)
  )

  ## annotate records ---------------------------------------------------------

  # add annotations
  if (!only.count &&
      (annotation.text != "") &&
      (length(imported$success) > 0L)) {
    # dispatch
    dbCTRAnnotateQueryRecords(
      recordnumbers = imported$success,
      recordannotations = imported$annotations,
      annotation.text = annotation.text,
      annotation.mode = annotation.mode,
      con = con,
      verbose = verbose
    )
  }

  # add query used for function
  imported <- c(
    imported[c("n", "success", "failed")],
    "queryterm" = querytermoriginal
  )

  ## finalise

  # early exit if only count
  if (only.count) return(imported)

  # add query parameters to database
  if (imported$n > 0L || !is.null(querytoupdate)) {
    dbCTRUpdateQueryHistory(
      register = register,
      queryterm = querytermoriginal,
      recordnumber = imported$n,
      con = con,
      verbose = verbose
    )

    # add metadata
    imported <- addMetaData(x = imported, con = con)
  }

  # return some useful information or break
  if (imported$n == 0L) {
    message(
      "Function did not result in any trial records having been imported"
    )
  }

  # inform user
  if (verbose) {
    message(
      "DEBUG: \n'queryterm'=", queryterm,
      "\n'queryupdateterm'=", queryupdateterm,
      "\n'imported'=", imported$n,
      "\n'register'=", register,
      "\n'collection'=", con$collection
    )
  }

  ## return
  return(imported)
}
# end ctrLoadQueryIntoDb
