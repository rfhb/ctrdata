# function definition for dfCalculate

#### history ####
# 2025-01-26 first version
# 2025-02-08 simplified


#' @noRd
#' @export
#' @importFrom dplyr mutate pull coalesce `%>%`
.resultsDate <- function(df = NULL) {

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

    txt <- '
Calculates the earliest date of results as recorded in the register.
At that date, results may have been incomplete and may have been changed later.
For EUCTR, requires that results and preferrably also their history of publication
have been included in the collection, using
ctrLoadQueryIntoDb(queryterm = ..., euctrresults{history} = TRUE, con = ...).
Cannot be calculated for ISRCTN, which does not have a corresponding field.

Returns a date.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

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
        firstreceived_results_date,
        trialInformation.analysisStageDate,
        results_first_posted,
        protocolSection.statusModule.resultsFirstPostDateStruct.date,
        results.intentToPublish,
        results.clinicalStudyReports.submitDate,
        results.summaryResults.submissionDate
      )
    ) %>%
    dplyr::pull(out) -> df[[".resultsDate"]]

  # keep only outcome columns
  df <- df[, c("_id", ".resultsDate"), drop = FALSE]


  #### checks ####
  # stopifnot(inherits(vct, "Date") || inherits(vct, "POSIXct"))
  # stopifnot(length(vct) == nrow(df))
  # TODO
  stopifnot(
    inherits(df[[".resultsDate"]], "Date") ||
      inherits(df[[".resultsDate"]], "POSIXct"))

  # return
  return(df)

} # end .resultsDate
