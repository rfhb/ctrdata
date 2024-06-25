### ctrdata package

#' Find names of fields in the database collection
#'
#' Given part of the name of a field of interest to the user, this
#' function returns the full field names used in records that were
#' previously loaded into a collection
#' (using \link{ctrLoadQueryIntoDb}). Only names of fields that have
#' a value in the collection can be returned.
#' Set \code{sample = FALSE} to force screening all records in the
#' collection for field names, see below.
#'
#' The full names of child fields are returned in dot notation (e.g.,
#' \code{clinical_results.outcome_list.outcome.measure.class_list.class.title})
#' In addition, names of parent fields (e.g.,
#' \code{clinical_results}) are returned.
#' Data in parent fields is typically complex (nested), see
#' \link{dfTrials2Long} for easily handling it.
#' For field definitions of the registers, see
#' "Definition" in \link{ctrdata-registers}.
#' Note: When \code{dbFindFields} is first called after
#' \link{ctrLoadQueryIntoDb}, it will take a moment.
#'
#' @param namepart A character string (can be a regular expression,
#' including Perl-style) to be searched among all field names (keys)
#' in the collection, case-insensitive. The default `".*"` lists all fields.
#'
#' @param sample If \code{TRUE} (default), uses a sample of only 5 trial
#' records per register to identify fields, to rapidly return a possibly
#' incomplete set of field names.
#' If \code{FALSE}, uses all trial records in the collection, which will
#' take more time with more trials but ensures to returns all names of
#' all fields in the collection.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#'
#' @inheritParams ctrDb
#'
#' @return Vector of strings with full names of field(s) found,
#' ordered by register and alphabet, see examples. Names of the vector
#' are the names of the register holding the respective fields. The field
#' names can be fed into \link{dbGetFieldsIntoDf} to extract the
#' data for the field(s) from the collection into a data frame.
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'     dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'     collection = "my_trials"
#' )
#'
#' dbFindFields(namepart = "date", con = dbc)[1:5]
#'
#' # view all 3350+ fields from all registers:
#'
#' allFields <- dbFindFields(con = dbc, sample = FALSE)
#'
#' if (interactive()) View(data.frame(
#'   register = names(allFields),
#'   field = allFields))
#'
dbFindFields <- function(namepart = ".*",
                         con,
                         sample = TRUE,
                         verbose = FALSE) {
  ## sanity checks
  if (!is.atomic(namepart)) stop("'namepart' should be atomic.", call. = FALSE)
  if (length(namepart) > 1) stop("'namepart' should have one element.", call. = FALSE)
  if (namepart == "") stop("Empty 'namepart' parameter.", call. = FALSE)

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  ## check if cache environment has entry for the database
  keyslist <- ctrCache(
    xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp")
  )

  ## get sample reference value
  cacheSmpl <- ctrCache(
    xname = paste0("keyslist_", con$db, "/", con$collection, "_sample")
  )

  ## get cache reference value
  cacheRef <- as.character(rev(unlist(try(nodbi::docdb_query(
    src = con, key = con$collection, query = '{"_id": "meta-info"}',
    fields = '{"queries.query-timestamp": 1, "_id": 0}'
  ), silent = TRUE)))[1])

  ## invalidate cache
  cacheOutdated <- is.null(keyslist) || (cacheRef != keyslist) || (!sample & cacheSmpl)

  ## get keyslist
  if (cacheOutdated) {
    # inform user
    message("Finding fields in database collection", appendLF = FALSE)

    queries <- paste0('{"ctrname": "', sort(registerList), '"}')
    names(queries) <- sort(registerList)

    # queries by sample
    if (sample) {

      # adding queries for records with results data
      queries <- c('{"trialInformation": {"$regex": ".+"}}',
                   '{"clinical_results": {"$regex": ".+"}}',
                   '{"resultsSection.outcomeMeasuresModule": {"$regex": ".+"}}',
                   queries)
      names(queries)[1:3] <- c("EUCTR", "CTGOV", "CTGOV2")
      n <- 5L
      message(" (sampling ", n, " trial records per register) ", appendLF = FALSE)

    } else {

      message(" (may take some time) ", appendLF = FALSE)
      n <- -1L

    }

    # get names
    keyslist <- NULL

    # iterate over registers
    for (i in seq_along(queries)) {

      message(" . ", appendLF = FALSE)

      # iterate over query items
      # get fields from register
      keysAdd <- nodbi::docdb_query(
        src = con,
        key = con$collection,
        query = queries[i],
        listfields = TRUE,
        limit = n
      )

      # give keys name of register
      if (!is.null(keysAdd)) names(keysAdd) <- rep(names(queries)[i], length(keysAdd))

      # accumulate keys
      keyslist <- c(keyslist, keysAdd)
    }
    message()

    # clean empty entries and exclude _id for consistency
    keyslist <- keyslist[keyslist != "_id" & keyslist != ""]

    ## store keyslist to environment (cache)
    if (length(keyslist) > 1) {
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection),
        xvalue = keyslist
      )
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
        xvalue = cacheRef
      )
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection, "_sample"),
        xvalue = sample
      )
      message("Field names cached for this session.")
    }

  } else {

    message("Using cache of fields. ")

    if (cacheSmpl) message(
      "Sample used, specify 'sample = FALSE' to digest all records.", "")

    keyslist <- ctrCache(
      xname = paste0("keyslist_", con$db, "/", con$collection),
      verbose = verbose
    )

  } # generate keyslist

  ## inform user of unexpected situation
  if ((length(keyslist) == 0) || all(keyslist == "")) {
    warning("No keys could be extracted, please check database ",
            "and collection: ", con$db, "/", con$collection,
            call. = FALSE
    )
  }

  ## now do the actual search and find for key name parts
  fields <- keyslist[grepl(
    pattern = namepart, x = keyslist,
    ignore.case = TRUE, perl = TRUE
  )]

  ## to remove duplicates
  fieldsDf <- unique(data.frame(
    register = names(fields),
    field = fields,
    stringsAsFactors = FALSE))

  fieldsTbl <- table(fieldsDf[["register"]])

  # user info
  if (verbose) message(
    paste0(names(fieldsTbl), collapse = " / "), ": ",
    paste0(fieldsTbl, collapse = " / "))

  fields <- fieldsDf[["field"]]
  names(fields) <- fieldsDf[["register"]]

  # user info
  if (verbose) message("Found ", length(fields), " fields.")

  # return value if no fields found
  if (!length(fields)) fields <- ""

  # return the match(es)
  return(fields)

} # end dbFindFields
