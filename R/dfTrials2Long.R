### ctrdata package

#' Convert data frame with trial records into long format
#'
#' The function works with procotol- and results- related information.
#' It converts lists and other values that are in a data frame returned
#' by \link{dbGetFieldsIntoDf} into individual rows of a long data frame.
#' From the resulting long data frame, values of interest can be selected
#' using \link{dfName2Value}.
#' The function is particularly useful for fields with complex content,
#' such as node field "\code{clinical_results}" from EUCTR, for which
#' \link{dbGetFieldsIntoDf} returns as a multiply nested list and for
#' which this function then converts every observation of every (leaf)
#' field into a row of its own.
#'
#' @param df Data frame (or tibble) with columns including
#'  the trial identifier (\code{_id}) and
#'  one or more variables as obtained from
#'  \link{dbGetFieldsIntoDf}
#'
#' @returns A data frame  (or tibble, if \code{tibble} is loaded)
#' with the four columns: `_id`, `identifier`, `name`, `value`
#'
#' @importFrom stringi stri_extract_all_charclass stri_extract_first stri_replace_first
#' @importFrom rvest html_text read_html
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials",
#'   flags = RSQLite::SQLITE_RO)
#'
#' dfwide <- dbGetFieldsIntoDf(
#'   fields = "clinical_results.participant_flow",
#'   con = dbc)
#'
#' dfTrials2Long(df = dfwide)
#'
dfTrials2Long <- function(df) {

  # get names
  dfn <- names(df)

  # check parameters
  if (!any("_id" == dfn) ||
      ncol(df) == 1L) stop(
        "Missing _id column or other variables in 'df'",
        call. = FALSE
      )
  if (any(c("identifier", "name", "value") %in% dfn)) stop(
    "Unexpected columns; 'df' should not come from dfTrials2Long",
    call. = FALSE
  )

  # make _id the first column
  if (dfn[1] != "_id") {
    dfn <- c("_id", dfn[dfn != "_id"])
    df <- df[, dfn, drop = FALSE]
  }

  # helper function
  flattenDf <- function(x) {
    while (any(vapply(x, is.list, logical(1L)))) {
      x <- lapply(x, function(x) if (is.list(x)) x else list(x))
      x <- unlist(x, recursive = FALSE, use.names = TRUE)
    }
    return(x)
  }

  # columns that are not compatible with the
  # later operations are converted to character
  conv <- sapply(df, class) == "Date"
  conv <- seq_len(ncol(df))[conv]
  for (c in conv) df[, c] <- as.character(df[, c, drop = TRUE])

  # protect numbers in names
  dfn <- gsub("([0-9]+)", "#\\1#", dfn)

  # get trial _id
  id <- df[["_id"]]

  # iterative unnesting, by column, by trial
  out <- lapply(
    seq_len(ncol(df))[-1],
    function(cc) {

      # get column name
      tn <- dfn[cc]

      # inform users
      message(tn, rep(" ", 200L - nchar(tn)), "\r", appendLF = FALSE)

      # get column as list,
      # one item is one trial
      ci <- df[[cc]]

      # by trial (_id)
      o <- lapply(seq_along(ci), function(ct) {

        o <- unlist(flattenDf(ci[ct]))
        o <- na.omit(o)
        if (is.null(o) || !length(o)) return(NULL)

        # check identifiers added by unlist
        tst <- stringi::stri_extract_first_regex(names(o), "[0-9]+")

        # construct new identifiers from column name and item name
        tst[is.na(tst)] <- "0"
        if (is.null(names(o))) {tnn <- paste0(tn, ".0")

        } else {tnn <- paste0(
          tn, ".", tst, ".", stringi::stri_replace_first_regex(
            names(o), "[0-9]+", ""))
        }

        # construct tall df by _id by col
        data.frame(
          "_id" = id[ct],
          "name" = tnn,
          "value" = o,
          check.names = FALSE,
          stringsAsFactors = FALSE,
          row.names = NULL)

      })

      # bind trials within column
      as.data.frame(do.call(rbind, o), stringsAsFactors = FALSE)
    }
  )

  # bind list items (were columns) into long df
  out <- do.call(rbind, c(out, stringsAsFactors = FALSE))
  message(rep(" ", 200L), "\r", appendLF = FALSE)

  # convert html entities
  htmlEnt <- grepl("&[#a-zA-Z]+;", out[["value"]])
  if (any(htmlEnt)) out[["value"]][htmlEnt] <-
    sapply(out[["value"]][htmlEnt], function(i) {
      rvest::html_text(rvest::read_html(charToRaw(i)))
    }, USE.NAMES = FALSE)
  message(". ", appendLF = FALSE)

  # name can include from 0 to about 6 number groups, get all
  # and concatenate to oid-like string such as "1.2.3.4.5.6",
  # e.g. "9.8.2" which should be extracted from an example name:
  # clinical...class9.analyzed...count8.@attributes.value2

  # except where name is exactly one of dfn
  onlyHere <- vapply(
    out[["name"]], function(i) !any(i == dfn),
    logical(1L), USE.NAMES = FALSE)

  # collect all identifiers
  out[["identifier"]][onlyHere] <- vapply(
    stringi::stri_extract_all_regex(out[["name"]][onlyHere], "[#]?[0-9]+([.#]|$)"),
    function(i) paste0(gsub("[.]", "", i[!grepl("^#", i)]), collapse = "."),
    character(1L))
  message(". ", appendLF = FALSE)

  # remove numbers from name
  regExps <- c(
    "[.]?[0-9]+([.])",
    "[.]?[0-9]+$",
    "[.]+$",
    "[.]?@attributes",
    "#"
  )
  for (i in regExps) {
    out[["name"]][onlyHere] <- gsub(
      i, "\\1", out[["name"]][onlyHere])
  }

  # remove protection from numbers
  out[["name"]][onlyHere] <- gsub(
    "[#]([0-9]+)", "\\1",
    out[["name"]][onlyHere], perl = TRUE)

  # remove any duplicate rows
  out <- unique(out)

  # helper to expand identifier into columns
  sortByOid <- function(oid) {

    oid <- strsplit(oid$identifier, ".", fixed = TRUE)
    maxLen <- max(sapply(oid, length))
    oid <- lapply(oid, function(i) c(as.numeric(i), rep(0, maxLen - length(i))))
    oid <- do.call(rbind, oid)
    oid <- data.frame(oid)
    return(oid)

  }

  # add oid columns for subsequent sorting
  out <- data.frame(out, sortByOid(oid = out), check.names = FALSE)

  # sort on _id, then name, then all oid columns (since number of
  # columns in oid varies, have to exclude columns unused for sort)
  oo <- with(out, do.call(order, out[, -match(c("value", "identifier"), names(out))]))
  out <- out[oo, ]

  # keep and sort columns
  out <- out[c("_id", "identifier", "name", "value")]

  # reset row numbers
  row.names(out) <- NULL

  # inform
  message(
    "\nTotal ", nrow(out), " rows, ",
    length(unique(out[["name"]])),
    " unique names of variables")

  # output
  return(dfOrTibble(out))

} # end dfTrials2Long
