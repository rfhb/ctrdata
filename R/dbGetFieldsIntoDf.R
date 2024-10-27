### ctrdata package

#' Create data frame of specified fields from database collection
#'
#' Fields in the collection are retrieved from all records into a data
#' frame (or tibble). Within a given trial record, a fields can be
#' hierarchical and structured, that is, nested.
#' Th function uses the field names to appropriately type the values
#' that it returns, harmonising original values (e.g. "Information not present
#' in EudraCT" to `NA`, "Yes" to `TRUE`, "false" to `FALSE`,
#' date strings to dates or time differences, number strings to numbers).
#' The function simplifies the structure of nested data and
#' may concatenate multiple strings in a field using " / " (see example)
#' and may have widened the returned data frame with additional columns that
#' were recursively expanded from simply nested data (e.g., "externalRefs"
#' to columns "externalRefs.doi", "externalRefs.eudraCTNumber" etc.).
#' For an alternative way for handling the complex nested data, see
#' \link{dfTrials2Long} followed by \link{dfName2Value} for extracting
#' the sought variable(s).
#'
#' @param fields Vector of one or more strings, with names of sought fields.
#' See function \link{dbFindFields} for how to find names of fields.
#' Dot path notation ("field.subfield") without indices is supported.
#' If compatibility with `nodbi::src_postgres()` is needed, specify fewer
#' than 50 fields, consider also using parent fields e.g., `"a.b"` instead
#' of `c("a.b.c.d", "a.b.c.e")`, accessing sought fields with
#' \link{dfTrials2Long} followed by \link{dfName2Value} or other R functions.
#'
#' @param verbose Printing additional information if set to \code{TRUE};
#' (default \code{FALSE}).
#'
#' @inheritParams ctrDb
#'
#' @param ... Do not use (captures deprecated parameter \code{stopifnodata})
#'
#' @return A data frame (or tibble, if \code{tibble} is loaded)
#' with columns corresponding to the sought fields.
#' A column for the records' `_id` will always be included.
#' The maximum number of rows of the returned data frame is equal to,
#' or less than the number of trial records in the database collection.
#'
#' @importFrom nodbi docdb_query
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' # get fields that are nested within another field
#' # and can have multiple values with the nested field
#' dbGetFieldsIntoDf(
#'   fields = "b1_sponsor.b31_and_b32_status_of_the_sponsor",
#'   con = dbc)
#'
#' # fields that are lists of string values are
#' # returned by concatenating values with a slash
#' dbGetFieldsIntoDf(
#'   fields = "keyword",
#'   con = dbc)
#'
dbGetFieldsIntoDf <- function(fields = "",
                              con, verbose = FALSE, ...) {

  # check fields
  if (!is.vector(fields) ||
      !all(class(fields) %in% "character")) {
    stop("Input should be a vector of strings of field names.", call. = FALSE)
  }

  # deprecated
  params <- list(...)
  if (!is.null(params[["stopifnodata"]])) warning(
    'Parameter "stopifnodata" is deprecated.'
  )

  # remove NA, NULL if included in fields
  fields <- fields[!is.null(fields) & !is.na(fields)]

  # remove _id if included in fields
  fields <- unique(fields["_id" != fields])

  # check if valid fields
  if (any(fields == "") || (length(fields) == 0)) {
    stop("'fields' contains empty elements; ",
         "please provide a vector of strings of field names. ",
         "Function dbFindFields() can be used to find field names. ",
         call. = FALSE)
  }
  if (length(fields) > 49L) {
    message(
      "If compatibility with nodbi::src_postgres() is needed, specify fewer ",
      "than 50 (was: ", length(fields), ") fields; parent fields, ",
      'e.g., "a.b" instead of c("a.b.c.d", "a.b.c.e"), can also be used ',
      "and sought fields can be extracted with dfTrials2Long() followed by ",
      "dfName2Value() or other R functions.")
  }

  # check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # get the data
  dfi <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = "{}",
    fields = paste0('{"', paste0(fields, collapse = '": 1, "'), '": 1}')
  )

  # early exit
  if (is.null(dfi) || !ncol(dfi)) stop(
    "No records with values for any specified field."
  )

  # user info
  if (verbose) message("Retrieved ", nrow(dfi), " rows of data.")

  # warn
  notFound <- setdiff(fields, names(dfi))
  if (length(notFound)) warning("No data could be extracted for '",
                                paste0(notFound, collapse = "', '"), "'.")

  # recursively widen results if a column is a data frame
  # by adding the data frame's columns as result columns.
  # this is a convenience function and historically users
  # may expect such behaviour from package ctrdata.
  doneNmnt <- NULL
  while (TRUE) {

    nm <- names(dfi)
    nt <- sapply(dfi, is.data.frame)
    if (!any(nt)) break

    doNmnt <- nm[nt]
    if (!length(doNmnt) || setequal(doNmnt, doneNmnt)) break

    for (colN in doNmnt) {

      expDf <- dfi[[colN]]
      if (!is.data.frame(expDf) || !ncol(expDf) || !nrow(expDf)) next
      names(expDf) <- paste0(colN, ".", names(expDf))

      # handle any data frames within expDf
      colK <- sapply(expDf, function(c) ifelse(is.null(ncol(c)) || ncol(c) > 0L, 1L, 0L))
      colK <- names(colK)[sapply(colK, function(c) !is.integer(c) || c > 0L)]
      if (length(colK)) expDf <- expDf[, colK, drop = FALSE]

      # merge with dfi
      colD <- na.omit(match(c(colN, names(expDf)), names(dfi)))
      dfi <- cbind(dfi[, -colD, drop = FALSE], expDf)
    }

    doneNmnt <- doNmnt

  } # while

  # for date time conversion
  lct <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
  Sys.setlocale("LC_TIME", "C")

  # iterates over columns for typing
  nm <- names(dfi)
  for (i in seq_len(ncol(dfi))) {

    if (nm[i] == "_id") next
    if (inherits(dfi[[i]], "data.frame") && !ncol(dfi[[i]])) next

    # simplify and replace NULL with NA
    dfi[[i]][!sapply(dfi[[i]], length)] <- NA

    # type column (NOTE this should not be a data frame because
    # the extraction would result in a data frame and this would
    # break the typing approach, thus the recursion above)
    dfi[[i]] <- typeField(dfi[[i]], nm[i])

    if (verbose) message(nm[i], " of type ", typeof(dfi[[i]]), ", ", appendLF = FALSE)

  }

  # remove rows with only NAs
  naout <- is.na(dfi)
  nc <- length(setdiff(attr(naout, "dimnames")[[2]], "_id"))
  dfi <- dfi[rowSums(naout) < nc, , drop = FALSE]

  # sort, add meta data
  dfi <- addMetaData(
    dfi[order(dfi[["_id"]]), , drop = FALSE],
    con = con)
  
  # reset row numbering
  row.names(dfi) <- NULL

  # return
  if (any("tibble" == .packages())) return(tibble::as_tibble(dfi))
  return(dfi)

} # end dbGetFieldsIntoDf
