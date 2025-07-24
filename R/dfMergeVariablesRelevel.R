### ctrdata package

#' Merge variables, keeping type where possible, optionally relevel factors
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
#' @returns A vector, with the type of the columns to be merged
#'
#' @importFrom dplyr select coalesce
#' @importFrom tidyr unite
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
#' df <- dbGetFieldsIntoDf(
#'   fields = c(
#'     "ctrname",
#'     "protocolSection.eligibilityModule.healthyVolunteers",
#'     "authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.clinicalTrialGroups.name",
#'     "authorizedApplication.authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.clinicalTrialGroups.name",
#'     "f31_healthy_volunteers",
#'     "eligibility.healthy_volunteers",
#'     "participants.participantType"
#'   ),
#'   con = dbc
#' )
#'
#' df$ctis1healthy <- "Healthy volunteers" ==
#'   df$authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.clinicalTrialGroups.name
#'
#' df$ctis2healthy <- "Healthy volunteers" ==
#'   df$authorizedApplication.authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.clinicalTrialGroups.name
#'
#' df$isrctnhealthy <- "Healthy volunteer" ==
#'   df$participants.participantType
#'
#' table(
#'   df$ctrname,
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

  # check
  if (all(apply(df[, colnames, drop = FALSE], 1,
                function(r) length(na.omit(r))) <= 1L)) {

    out <- do.call(
      dplyr::coalesce, as.list(df[, colnames, drop = FALSE]))

  } else {

    out <- tidyr::unite(
      df[, colnames, drop = FALSE],
      out, na.rm = TRUE, sep = " / "
    )[["out"]]

    out <- trimws(gsub(" / ($)|(^) / |( / ) */ ", "\\1", out))

    message(
      "More than one column had values, returning e.g. '",
      out[grepl(" / ", out)][1], "'"
    )

  }

  # label levels
  if (!is.null(levelslist)) {
    out <- factor(out)
    levels(out) <- levelslist
  }

  # return
  return(out)
}
# end dfMergeVariablesRelevel
