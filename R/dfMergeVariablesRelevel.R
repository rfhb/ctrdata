### ctrdata package

#' Merge variables, keeping type, and optionally relevel factors
#'
#' Merge variables in a data frame such as returned by \link{dbGetFieldsIntoDf}
#' into a new variable, and optionally also map its values to new levels.
#'
#' @param df A \link{data.frame} with the variables (columns) to be merged into
#' one vector.
#'
#' @param colnames A vector of names of columns in `df` that hold the variables
#' to be merged, or a selection of columns as per \code{\link[dplyr]{select}}.
#'
#' @param levelslist A names list with one slice each for a new value to be
#' used for a vector of old values (optional).
#'
#' @return A vector, with the type of the columns to be merged
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr c_across mutate rowwise
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
#' df <- dbGetFieldsIntoDf(
#'     fields = c("overall_status", "x5_trial_status"),
#'     con = dbc
#' )
#'
#' statusvalues <- list(
#'     "ongoing" = c("Recruiting", "Active", "Ongoing"),
#'     "completed" = c("Completed", "Prematurely Ended", "Terminated"),
#'     "other" = c("Withdrawn", "Suspended", "No longer available")
#' )
#'
#' dfMergeVariablesRelevel(
#'     df = df,
#'     colnames = 'contains("status")',
#'     levelslist = statusvalues
#' )
#'
dfMergeVariablesRelevel <- function(
    df = NULL,
    colnames = "",
    levelslist = NULL) {
  # initialise
  env <- new.env()
  evalq(warned <- FALSE, env)

  # helper function
  getValuesOrNa <- function(x) {
    x <- na.omit(x)
    if (!length(x)) {
      return(NA)
    }

    if (length(x) > 1L) {
      x <- as.character(x)
      x <- paste0(x[nchar(x) > 0L], collapse = " / ")

      if (!get("warned", envir = env)) {
        message("More than one column had values, returning e.g. '", x, "'")
        evalq(warned <- TRUE, env)
      }
    }

    return(x)
  }

  # merge columns
  if (length(colnames) == 1L && grepl("[()]", colnames)) {
    identifiedColumns <- names(
      dplyr::select(df, eval(parse(text = colnames)))
    )

    message(
      "Columns identified to be merged: ",
      paste0(identifiedColumns, collapse = ", ")
    )

    out <- dplyr::mutate(
      dplyr::rowwise(df),
      out = getValuesOrNa(dplyr::c_across(eval(parse(text = colnames))))
    )[["out"]]
  } else {
    out <- dplyr::mutate(
      dplyr::rowwise(df),
      out = getValuesOrNa(dplyr::c_across(colnames))
    )[["out"]]
  }

  # merge levels
  if (!is.null(levelslist)) {
    out <- factor(out)
    levels(out) <- levelslist
  }

  # return
  return(out)
}
# end dfMergeVariablesRelevel
