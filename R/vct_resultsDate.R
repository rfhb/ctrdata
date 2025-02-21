# function definition for dfCalculate

#### history ####
# 2025-01-26 first version
# 2025-02-08 simplified


#' @noRd
#' @export
#' @importFrom dplyr mutate pull select `%>%`
.resultsDate <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "trialInformation.analysisStageDate"
      # https://www.clinicaltrialsregister.eu/ctr-search/trial/2020-004272-17/results
      # unfortunately this table is only imported with euctrhistory = TRUE
      # This version publication date 12 Jan 2025
      # First version publication date 12 Jan 2025
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
      "results.clinicalStudyReports.submitDate",
      "results.summaryResults.submissionDate"
      # not using this, could be a list
      # "results.laypersonResults.submissionDate"
    ))


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the earliest date of results as recorded in the register.
At that date, results may have been incomplete and may have been changed later.
Requires that EUCTR results have been included in the collection, using
ctrLoadQueryIntoDb(queryterm = ..., euctrresults = TRUE, con = ...).
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

  # helper function
  pminInt <- function(...) pmin(..., na.rm = TRUE)


  #### CTIS ####

  # all registers
  df %>%
    dplyr::select(
      unlist(fldsNeeded, use.names = FALSE)
    ) %>%
    dplyr::mutate(
      out = do.call(pminInt, (.))
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
