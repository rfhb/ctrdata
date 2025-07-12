#### history ####
# 2025-01-27 first version

#' Calculate if record is unique for a study
#'
#' Trial concept calculated: Applies function dbFindIdsUniqueTrials() with
#' its defaults.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.isUniqueTrial`, a logical.
#'
#' @export
#'
#' @examples
#' # fields needed
#' f.isUniqueTrial()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.isUniqueTrial",
#'   con = dbc)
#' trialsDf
#'
f.isUniqueTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  #### fields ####

  # need at least one field
  fldsNeeded <- "ctrname"

  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  df <- fctChkFlds(df, fldsNeeded)

  # apply function, access object con in calling environment
  vct <- dbFindIdsUniqueTrials(con = parent.frame()$con)

  # calculate result
  df[[".isUniqueTrial"]] <- df[["_id"]] %in% vct

  # keep only outcome columns
  df <- df[, c("_id", ".isUniqueTrial"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".isUniqueTrial"]], "logical"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.isUniqueTrial
