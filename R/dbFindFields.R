### ctrdata package

#' Find names of fields in the database collection
#'
#' Given part of the name of a field of interest to the user, this
#' function returns the full field names used in records that were
#' previously loaded into a collection
#' (using \link{ctrLoadQueryIntoDb}). Only names of fields that have
#' a value in the collection can be returned.
#'
#' In addition to the full names of all child fields (e.g.,
#' \code{clinical_results.outcome_list.outcome.measure.class_list.class.title})
#' this function returns names of parent fields (e.g.,
#' \code{clinical_results}).
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
#' allFields <- dbFindFields(con = dbc)
#'
#' if (interactive()) View(data.frame(
#'   register = names(allFields),
#'   field = allFields))
#'
dbFindFields <- function(namepart = ".*",
                         con,
                         verbose = FALSE) {
  ## sanity checks
  if (!is.atomic(namepart)) stop("'namepart' should be atomic.", call. = FALSE)
  if (length(namepart) > 1) stop("'namepart' should have one element.", call. = FALSE)
  if (namepart == "") stop("Empty 'namepart' parameter.", call. = FALSE)

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  ## check if cache environment has entry for the database
  keyslist <- ctrCache(
    xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
    verbose = verbose
  )

  ## get cache reference value
  cacheRef <- as.character(rev(unlist(try(nodbi::docdb_query(
    src = con, key = con$collection, query = '{"_id": "meta-info"}',
    fields = '{"queries.query-timestamp": 1, "_id": 0}'
  ), silent = TRUE)))[1])

  ## invalidate cache
  cacheOutdated <- is.null(keyslist) || (cacheRef != keyslist)

  ## get keyslist
  if (cacheOutdated) {
    # inform user
    message("Finding fields in database collection ",
            "(may take some time) ", appendLF = FALSE)

    # get names
    keyslist <- NULL

    # iterate over registers
    for (q in registerList) {
      message(" . ", appendLF = FALSE)
      # iterate over query items
      # get fields from register
      keysAdd <- nodbi::docdb_query(
        src = con,
        key = con$collection,
        query = paste0('{"ctrname": "', q, '"}'),
        listfields = TRUE
      )
      # give keys name of register
      names(keysAdd) <- rep(q, length(keysAdd))
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
        xvalue = keyslist, verbose = verbose
      )
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
        xvalue = cacheRef, verbose = verbose
      )
      message("Field names cached for this session.")
    }
  } else {

    message("Using cache of fields.")

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

  # user info
  if (verbose) message("Found ", length(fields), " fields.")

  # return value if no fields found
  if (!length(fields)) fields <- ""

  # return the match(es)
  return(fields)

} # end dbFindFields
