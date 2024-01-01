### ctrdata package

#' Create data frame of specified fields from database collection
#'
#' Fields in the collection are retrieved into a data frame (or tibble).
#' Note that fields within the record of a trial can be hierarchical
#' and structured, that is, nested.
#' Names of fields can be found with \link{dbFindFields}.
#' The function uses the field names to appropriately type the values
#' that it returns, harmonising original values (e.g. "Information not present
#' in EudraCT" to `NA`, "Yes" to `TRUE`, "false" to `FALSE`,
#' date strings to class Date, number strings to numbers).
#' The function also attempts to simplify the structure of nested data and
#' may concatenate multiple strings in a field using " / " (see example).
#' For full handling of complex nested data, use function \link{dfTrials2Long}
#' followed by \link{dfName2Value} to extract the sought nested variable(s).
#'
#' @param fields Vector of one or more strings, with names of sought fields.
#' See function \link{dbFindFields} for how to find names of fields.
#' "item.subitem" notation is supported.
#'
#' @param stopifnodata Stops with an error (default \code{TRUE}) or with
#' a warning (\code{FALSE}) if the sought field is empty in all,
#' or not available in any of the records in the database collection.
#'
#' @param verbose Printing additional information if set to \code{TRUE};
#' (default \code{FALSE}).
#'
#' @inheritParams ctrDb
#'
#' @return A data frame (or tibble, if \code{tibble} is loaded)
#' with columns corresponding to the sought fields.
#' A column for the records' `_id` will always be included.
#' Each column can be either a simple data type (numeric, character, date)
#' or a list (typically for nested data, see above). For complicated lists,
#' use function \link{dfTrials2Long} followed by function \link{dfName2Value}
#' to extract values for sought nested variables.
#' The maximum number of rows of the returned data frame is equal to,
#' or less than the number of records of trials in the database
#' collection.
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
                              con, verbose = FALSE,
                              stopifnodata = TRUE) {

  # check parameters
  if (!is.vector(fields) ||
      !all(class(fields) %in% "character")) {
    stop("Input should be a vector of strings of field names.", call. = FALSE)
  }

  # remove NA, NULL if included in fields
  fields <- fields[!is.null(fields) & !is.na(fields)]

  # remove _id if included in fields
  fields <- fields["_id" != fields]

  # check if valid fields
  if (any(fields == "") || (length(fields) == 0)) {
    stop("'fields' contains empty elements; ",
         "please provide a vector of strings of field names. ",
         "Function dbFindFields() can be used to find field names. ",
         call. = FALSE)
  }

  # check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # get the data
  dfi <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{}',
    fields = paste0('{"', paste0(fields, collapse = '": 1, "'), '": 1}')
  )

  # iterate over columns
  nm <- names(dfi)

  # for date time conversion
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)

  # type columns
  for (i in seq_len(ncol(dfi))) {

    # TODO
    message("\n", nm[i], ": ", typeof(dfi[[i]]), " -> ", appendLF = FALSE)

    # simplify and replace NULL with NA
    dfi[[i]][!sapply(dfi[[i]], length)] <- NA

    # type column
    if (nm[i] == "_id") next
    # TODO
    # if (typeof(dfi[, i]) == "character")
    dfi[[i]] <- typeField(dfi[[i]], nm[i])

    # TODO
    message(typeof(dfi[[i]]), appendLF = FALSE)

  }

  # return
  if (any("tibble" == .packages())) return(tibble::as_tibble(dfi))
  return(dfi)

} # end dbGetFieldsIntoDf

if (FALSE) {

  # get all ids to enable Reduce which would fail
  # due to holes from NULLs from the merge step
  dft <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{}',
    fields = paste0('{"_id": 1}'))

  # early exit if no records
  if (!nrow(dft)) stop(
    "No data found in collection \"", con$collection, "\"", call. = FALSE)

  # continue with data frame of _id's
  dft <- dft[dft[["_id"]] != "meta-info", "_id", drop = FALSE]

  # initialise output
  nFields <- length(fields)
  accumNames <- NULL

  # iterate over fields so that we can
  # use a custom function to merge results
  result <- lapply(
    seq_len(nFields),
    function(i) {
      #
      item <- fields[i]

      # user info
      message(ifelse(i > 1L, "\n", ""), item, "... ", appendLF = FALSE)
      #
      tmpItem <- try({

        # execute query
        dfi <- nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = '{"_id": {"$ne": "meta-info"}}',
          fields = paste0('{"_id": 1, "', item, '": 1}'))
        message("\b\b\b\b   \b\b\b ", appendLF = FALSE)

        # leave try() early if no results
        if (!nrow(dfi) || ncol(dfi) == 1L) stop(simpleError("No data"))

        # remove any rows without index variable
        dfi <- dfi[!is.na(dfi[["_id"]]), , drop = FALSE]

        # simplify and replace NULL with NA
        dfi[[2]][!sapply(dfi[[2]], length)] <- NA

        # simplify by extracting recursively any requested subitem
        itemSegments <- strsplit(item, "[.]")[[1]]
        itemSegments <- setdiff(itemSegments, names(dfi))
        for (iS in itemSegments) {
          message(". ", appendLF = FALSE)
          if ((length(names(dfi[[2]])) == 1L) &&
              (iS == names(dfi[[2]]))) {
            dfi[[2]] <- dfi[[2]][[iS]]
          } else {
            # e.g. for "primary_outcome.measure" from MongoDB
            tn <- unlist(sapply(dfi[[2]], names))
            if (length(unique(tn)) == 1L && (iS == tn[1])) {
              dfi[[2]] <- sapply(dfi[[2]], function(i)
                if (length(i)) i[[1]] else NA,
                simplify = FALSE, USE.NAMES = FALSE)
            } else {
              # no more predictable simplification possible:
              # break to leave for loop over itemSegments
              break
            }
          }
        }

        # simplify by expanding a resulting data frame
        if (length(unique(names(dfi[[2]]))) > 1L) {
          item <- paste0(item, ".", names(dfi[[2]]))
          dfi <- cbind("_id" = dfi[["_id"]], as.data.frame(dfi[[2]]))
          message(". ", appendLF = FALSE)
          emptyCols <- sapply(dfi, function(c) all(is.na(c)))
          emptyCols <- seq_along(emptyCols)[emptyCols]
          if (length(emptyCols)) dfi <- dfi[, -emptyCols, drop = FALSE]
          if (length(emptyCols)) item <- item[-(emptyCols - 1L)]
        }

        # name result set
        names(dfi) <- c("_id", item)

        # create NA output from template
        dfo <- dft

        # simplify by processing columns
        for (c in seq_len(ncol(dfi))[-1]) {

          # inform user
          if (c > 2L) message(". ", appendLF = FALSE)

          # data frames with single rows are lists
          # turn such lists back into data frames
          # e.g. location.facility but not location
          # thus check names per row, data frame should
          # have more than one column name
          tmpDfs <- sapply(dfi[[c]], class) == "data.frame"
          tmpLst <- sapply(dfi[[c]], class) == "list"
          tmpLen <- sapply(dfi[[c]][ !sapply(dfi[[c]], is.null) ], length)
          if (any(tmpDfs) && any(tmpLst) &&
              all(tmpLen > 1L) && length(unique(tmpLen)) == 1L) {
            dfi[[c]][tmpLst] <- lapply(
              dfi[[c]][tmpLst], function(i) data.frame(
                do.call(rbind, i), check.names = FALSE))
          }

          # special case: column is one-column data frame
          if (is.data.frame(dfi[[c]]) && (ncol(dfi[[c]]) == 1L) &&
              (nrow(dfi[[c]]) == nrow(dfi))) {
            tn <- names(dfi[[c]])
            dfi[[c]] <- dfi[[c]][, 1, drop = TRUE]
            names(dfi)[c] <- paste0(names(dfi)[c], ".", tn)
          }

          # mangle column if not simply character
          if (typeof(dfi[[c]]) != "character") {

            # simplify and replace NULL with NA
            dfi[[c]][!sapply(dfi[[c]], length)] <- NA

            # simplify column with one-column data frames or
            # one-item list e.g. "primary_outcome.measure"
            if (!is.data.frame(dfi[[c]]) &&
                all(sapply(dfi[[c]], function(r)
                  (!is.atomic(r)) &&
                  ((length(unlist(r)) <= 1L) ||
                   (is.data.frame(r) && ncol(r) == 1L && nrow(r) > 0L))
                ))) {
              dfi[[c]] <- sapply(
                dfi[[c]], function(i) {
                  if (length(i))
                    if (!is.null(ncol(i[[1]])) && ncol(i[[1]]) > 1L)
                      i[1] else i[[1]]
                  else NA},
                USE.NAMES = FALSE, simplify = TRUE)
            }

            # concatenate data if any rows are of type character
            # and if there is no more complex structure
            # (thus, vector of types is not a named vector)
            rowName <- sapply(dfi[[c]], function(i) is.null(names(i)))
            rowName2 <- sapply(names(rowName), function(i) is.null(i))
            rowType <- sapply(
              dfi[[c]], function(i) typeof(unlist(i, recursive = FALSE)))
            #
            if (all(rowName) & all(rowName2) &
                length(unique(rowName)) <= 1L &
                any(rowType == "character")) {
              #
              dfi[[c]] <- sapply(dfi[[c]], function(i)
                if (length(i) > 1L) {
                  rowI <- paste0(i[!is.na(i)], collapse = " / ")
                  if (nchar(rowI)) rowI else NA
                } else if (length(i) && !is.na(i)) i else NA)
            }

            # list of one-element lists such as dates
            if (any(sapply(dfi[[c]], class) == "Date")) {
              dfi[[c]] <- unlist(dfi[[c]], recursive = FALSE, use.names = FALSE)
              dfi[[c]] <- as.Date(dfi[[c]], origin = "1970-01-01")
            }

          } # if typeof

          # type after if typeof
          if (typeof(dfi[[c]]) == "character") {
            dfi[[c]] <- typeField(dfi[[c]], names(dfi)[c])
          }

          # add a column into copy of NA template
          dfo[[c]] <- switch(
            class(dfi[[c]])[1],
            "Date" = as.Date(NA),
            "numeric" = as.numeric(NA),
            "character" = as.character(NA),
            "data.frame" = NA,
            "integer" = as.integer(NA),
            "list" = NA,
            "logical" = as.logical(NA),
            NA
          )

        } # for processing columns

        # add NA where dfi has no data to avoid NULL when
        # merging with Reduce below, which otherwise raises
        #  Error in `[<-.data.frame`(`*tmp*`, value, value = NA) :
        #  new columns would leave holes after existing columns
        names(dfo) <- names(dfi)
        dfi <- suppressWarnings(
          rbind(dfo[!(dfo[["_id"]] %in% dfi[["_id"]]), , drop = FALSE], dfi))
        # suppressing the following which is related to adding a list into a
        # column that has NAs from dfo; warning does not occur with reversing
        # to dfi, dfo[] so that it seems acceptable to suppress warnings
        # Warning messages:
        # 1: In value[[jj]][ri] <- if (is.factor(xij)) as.vector(xij) else xij :
        #   number of items to replace is not a multiple of replacement length
        # 2: In names(value[[jj]])[ri] <- nm :
        #   number of items to replace is not a multiple of replacement length

      },
      silent = TRUE) # tmpItem try

      # inform user
      if (inherits(tmpItem, "try-error") ||
          !nrow(dfi) || (ncol(dfi) == 1L) ||
          is.null(dfi[[2]]) || all(is.na(dfi[[2]]))) {

        # try-error occurred or no data retrieved
        if (stopifnodata) {
          if (inherits(tmpItem, "try-error") &&
              !attr(tmpItem, "condition")["message"] == "No data") message(
                "\nProcessing error: '", trimws(tmpItem[[1]]), "'\nThank you ",
                "for reporting it at https://github.com/rfhb/ctrdata/issues")
          message("")
          stop("No data could be extracted for '", paste0(item, collapse = "', '"), "'.",
               "\nUse dbGetFieldsIntoDf(..., stopifnodata = FALSE) to ignore the error.",
               "\nUse dbFindFields() to find fields that exist in the collection.",
               call. = FALSE)
        } else {
          message("* no data or extraction error *")
          # create empty data set
          dfi <- cbind(dft, NA)
          names(dfi) <- c("_id", fields[i])
        } # stopifnodata
      } # if

      # add to result unless item was
      # previously specified in fields
      if (i > 1L) {
        dna <- names(dfi)
        dni <- intersect(dna, accumNames)
        dnd <- setdiff(dna, accumNames)
        if (length(dni)) {
          message("(not included again: ",
                  paste0(dni, collapse = ", "), ") ",
                  appendLF = FALSE)
          dfi <- dfi[, dnd, drop = FALSE]
        }
      }
      accumNames <<- c(accumNames, names(dfi)[-1])
      dfi

    }) # end lapply

  # bring result lists into data frame, by record _id
  result <- Reduce(function(...) merge(..., all = TRUE, by = "_id"), result)

  # prune rows without _id
  result <- result[!is.na(result[["_id"]]), , drop = FALSE]

  # remove rows with only NAs
  naout <- is.na(result)
  nc <- length(setdiff(attr(naout, "dimnames")[[2]], "_id"))
  result <- result[rowSums(naout) < nc, , drop = FALSE]

  # inform user
  if (is.null(result) || !nrow(result)) {
    warning("No records with values for any specified field. ",
            call. = FALSE)
    return(NULL)
  }

  # remove row names
  row.names(result) <- NULL

  # sort, add meta data
  result <- addMetaData(
    result[order(result[["_id"]]), , drop = FALSE],
    con = con)

  # return
  if (any("tibble" == .packages())) return(tibble::as_tibble(result))
  return(result)

}
