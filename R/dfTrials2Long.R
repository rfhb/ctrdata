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
#' @return A data frame  (or tibble, if \code{tibble} is loaded)
#' with the four columns: `_id`, `identifier`, `name`, `value`
#'
#' @importFrom stringi stri_extract_all_charclass stri_extract_first stri_replace_first
#' @importFrom tibble as_tibble
#' @importFrom xml2 xml_text read_html
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
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

  # iterative unnesting, by column
  out <- lapply(
    seq_len(ncol(df))[-1],
    function(cc) {
      # get item
      ci <- df[[cc]]
      # get item name
      tn <- dfn[cc]
      # inform user
      message(tn, rep(" ", 200L - nchar(tn)), "\r", appendLF = FALSE)
      # handle case when column is data frame, turn into list by row
      if (is.data.frame(ci)) ci <- split(ci, seq_len(nrow(ci)))
      # and by cell in column
      lapply(ci, function(c) {
        if (is.data.frame(c)) {
          # unlist is numbering repeat item names
          x <- unlist(flattenDf(c))
          if (!is.null(names(x))) tn <- paste0(tn, ".", names(x))
          if (is.null(x)) x <- NA
          # compose
          data.frame(
            "name" = tn,
            "value" = x,
            check.names = FALSE,
            stringsAsFactors = FALSE,
            row.names = NULL)
        } else {
          # initialise
          xx <- NULL
          tnn <- NULL
          # need to iterate since there may be repeats, e.g, 1, 2, 3.1, 3.2
          sapply(seq_len(length(c)), function(i) {
            # unlist is numbering repeat item names
            x <- unlist(flattenDf(c[i]))
            if (!is.null(names(x))) {
              # any first numeric identifier?
              tst <- stringi::stri_extract_first_regex(names(x), "[0-9]+")
              if (all(!is.na(tst))) {
                # if yes bring into middle
                tn <- paste0(
                  tn, ".", tst, ".",
                  stringi::stri_replace_first_regex(names(x), "[0-9]+", ""))
              } else {
                # if no add using i
                tn <- paste0(tn, ".", i, ".", names(x))
              }
            }
            if (is.null(x)) x <- NA
            xx <<- c(xx, x)
            tnn <<- c(tnn, tn)
          }, USE.NAMES = FALSE)
          # compose
          data.frame(
            "name" = tnn,
            "value" = xx,
            check.names = FALSE,
            stringsAsFactors = FALSE,
            row.names = NULL)
        } # if is.data.frame
      })})
  message(rep(" ", 200L), "\r", appendLF = FALSE)

  # add _id to list elements and
  # simplify into data frames
  out <- lapply(
    out, function(e) {
      message(". ", appendLF = FALSE)
      names(e) <- df[["_id"]]
      do.call(rbind, c(e, stringsAsFactors = FALSE))
    })
  out <- do.call(rbind, c(out, stringsAsFactors = FALSE))
  message(". ", appendLF = FALSE)

  # remove rows where value is NA
  out <- out[!is.na(out[["value"]]), , drop = FALSE]

  # convert html entities
  htmlEnt <- grepl("&[#a-zA-Z]+;", out[["value"]])
  if (any(htmlEnt)) out[["value"]][htmlEnt] <-
    sapply(out[["value"]][htmlEnt], function(i) {
      xml2::xml_text(xml2::read_html(charToRaw(i)))
    }, USE.NAMES = FALSE)
  message(". ", appendLF = FALSE)

  # generate new data frame with target columns and order
  out <- data.frame(
    # process row.names to obtain trial id
    "_id" = stringi::stri_extract_first(
      str = row.names(out),
      regex = c(paste0(regCtgov, "|", regIsrctn, "|",
                       regEuctr, "-[3]?[A-Z]{2}|", regCtis))),
    "identifier" = NA,
    "name" = out[["name"]],
    "value" = out[["value"]],
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE)
  message(". ", appendLF = FALSE)

  # name can include from 0 to about 6 number groups, get all
  # and concatenate to oid-like string such as "1.2.3.4.5.6",
  # e.g. "9.8.2" which should be extracted from the this name
  # clinical...class9.analyzed...count8.@attributes.value2
  #
  # except where name is exactly one of dfn
  onlyHere <- vapply(out[["name"]], function(i) !any(i == dfn),
                     logical(1L), USE.NAMES = FALSE)
  #
  out[["identifier"]][onlyHere] <- vapply(
    stringi::stri_extract_all_regex(out[["name"]][onlyHere], "[0-9]+([.]|$)"),
    function(i) paste0(gsub("[.]", "", i), collapse = "."), character(1L))
  # defaults
  out[["identifier"]] [out[["identifier"]] == "NA"] <- "0"
  out[["identifier"]] [is.na(out[["identifier"]])]  <- "0"
  message(". ", appendLF = FALSE)
  #
  # remove numbers from variable name
  out[["name"]][onlyHere] <- gsub(
    "[0-9]+([.])|[0-9]+$|[.]?@attributes", "\\1",
    out[["name"]][onlyHere], perl = TRUE)
  #
  # remove any double separators
  out[["name"]] <- gsub("[.][.]+", ".", out[["name"]], perl = TRUE)

  # remove double rows from duplicating e above
  out <- unique(out)

  # inform
  message("\nTotal ", nrow(out), " rows, ",
          length(unique(out[["name"]])),
          " unique names of variables")

  # output
  if (any("tibble" == .packages())) return(tibble::as_tibble(out))
  return(out)

} # end dfTrials2Long
