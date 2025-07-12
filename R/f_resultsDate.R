#### history ####
# 2025-01-26 first version
# 2025-02-08 simplified

#' Calculate date of results of a study
#'
#' Trial concept calculated: earliest date of results as recorded in the
#' register. At that date, results may have been incomplete and may have
#' been changed later.
#' For EUCTR, requires that results and preferrably also their history of
#' publication have been included in the collection, using
#' ctrLoadQueryIntoDb(queryterm = ..., euctrresultshistory = TRUE, con = ...).
#' Cannot be calculated for ISRCTN, which does not have a corresponding field.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.resultsDate`, a date.
#'
#' @export
#'
#' @importFrom dplyr mutate pull coalesce `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.resultsDate()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.resultsDate",
#'   con = dbc)
#' trialsDf
#'
f.resultsDate <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "firstreceived_results_date",
      "trialInformation.analysisStageDate"
    ),
    "ctgov" = c(
      "results_first_posted"
    ),
    "ctgov2" = c(
      "protocolSection.statusModule.resultsFirstPostDateStruct.date"
    ),
    "isrctn" = c(
      "results.intentToPublish"
    ),
    "ctis" = c(
      "results.summaryResults.submissionDate",
      "results.clinicalStudyReports.submitDate"
      # not using this, could be a list
      # "results.laypersonResults.submissionDate"
    ))


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### CTIS ####

  # all registers
  df %>%
    dplyr::mutate(
      out = dplyr::coalesce(
        # sequence matters
        .data$firstreceived_results_date,
        .data$trialInformation.analysisStageDate,
        .data$results_first_posted,
        .data$protocolSection.statusModule.resultsFirstPostDateStruct.date,
        .data$results.intentToPublish,
        .data$results.clinicalStudyReports.submitDate,
        .data$results.summaryResults.submissionDate,
        .ptype = as.Date(character(0L))
      )
    ) %>%
    dplyr::pull("out") -> df[[".resultsDate"]]

  # keep only outcome columns
  df <- df[, c("_id", ".resultsDate"), drop = FALSE]


  #### checks ####
  stopifnot(
    inherits(df[[".resultsDate"]], "Date") ||
      inherits(df[[".resultsDate"]], "POSIXct"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.resultsDate
