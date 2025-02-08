# function definition for dfCalculate

#### history ####
# 2025-01-26 first version
# 2025-02-08 simplified


#' @noRd
#' @export
#' @importFrom dplyr mutate pull select `%>%`
.startDate <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "n_date_of_competent_authority_decision",
      "n_date_of_ethics_committee_opinion",
      "trialInformation.recruitmentStartDate"
    ),
    "ctgov" = c(
      "start_date"
    ),
    "ctgov2" = c(
      "protocolSection.statusModule.startDateStruct.date"
    ),
    "isrctn" = c(
      "participants.recruitmentStart",
      "trialDesign.overallStartDate"
    ),
    "ctis" = c(
      "startDateEU",
      "authorizationDate",
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate"
    ))


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the date of start of the trial, based on the documented or planned
start of recruitment, or on the date of opinion of the competent authority.
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
  pmaxInt <- function(...) pmax(..., na.rm = TRUE)

  # all registers
  df %>%
    dplyr::select(
      unlist(fldsNeeded, use.names = FALSE)
    ) %>%
    dplyr::mutate(
      out = do.call(pmaxInt, (.))
    ) %>%
    dplyr::pull(out) -> vct


  #### checks ####
  stopifnot(inherits(vct, "Date") || inherits(vct, "POSIXct"))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .startDate
