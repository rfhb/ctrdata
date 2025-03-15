### ctrdata package

#' Merge variables, keeping type, and optionally relevel factors
#'
#' Merge variables in a data frame such as returned by \link{dbGetFieldsIntoDf}
#' into a new variable, and optionally also map its values to new levels.
#' See \link{ctrdata-trial-concepts} for pre-defined cross-register concepts
#' that are already implemented based on merging fields from different
#' registers and calculating a new field.
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
#' @importFrom dplyr select
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials",
#'   flags = RSQLite::SQLITE_RO)
#' df <- dbGetFieldsIntoDf(
#'   fields = c(
#'     "protocolSection.eligibilityModule.healthyVolunteers",
#'     "f31_healthy_volunteers",
#'     "eligibility.healthy_volunteers"
#'   ),
#'   con = dbc
#' )
#'
#' table(
#'   dfMergeVariablesRelevel(
#'     df = df,
#'     colnames = 'matches("healthy")'
#' ))
#'
dfMergeVariablesRelevel <- function(
    df = NULL,
    colnames = "",
    levelslist = NULL) {

  # check
  stopifnot(is.data.frame(df))
  stopifnot(is.character(colnames))

  # initialise
  env <- new.env()
  evalq(warned <- FALSE, env)

  # identify columns
  if (length(colnames) == 1L && grepl("[()]", colnames)) {

    colnames <- names(
      dplyr::select(df, eval(parse(text = colnames)))
    )
    message(
      "Columns identified to be merged: ",
      paste0(colnames, collapse = ", ")
    )
  }

  # early exit
  stopifnot(all(colnames %in% names(df)))
  if (length(colnames) == 1L) return(df[[colnames]])

  # mapply helper
  merge2Cols <- function(x, y) {
    mapply(function(x, y) {
      x <- x[x != ""]; y <- y[y != ""]
      if (!length(x) && !length(y)) return(NA)
      if (length(x) && !is.na(x) && length(y) && !is.na(y)) {
        if (!get("warned", envir = env)) {message(
          "More than one column had values, returning e.g. '", x, " / ", y, "'")
          evalq(warned <- TRUE, env)}
        return(paste(c(x, y), collapse = " / "))}
      if (!is.na(x)) return(x) else return(y)},
      x, y, USE.NAMES = FALSE)
  }

  # merge
  out <- Reduce(
    f = merge2Cols,
    x = as.list(df[, colnames[-1], drop = FALSE]),
    init = df[, colnames[1], drop = TRUE]
  )

  # label levels
  if (!is.null(levelslist)) {
    out <- factor(out)
    levels(out) <- levelslist
  }

  # return
  return(out)
}
# end dfMergeVariablesRelevel
