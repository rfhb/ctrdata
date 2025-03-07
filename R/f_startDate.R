#### history ####
# 2025-01-26 first version
# 2025-02-08 simplified

#' Calculate start date of a study
#'
#' Trial concept calculated: start of the trial, based on the
#' documented or planned start of recruitment, or on the date of opinion
#' of the competent authority.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.startDate`, a date.
#'
#' @export
#'
#' @importFrom dplyr mutate pull select `%>%`
#'
#' @examples
#' # fields needed
#' f.startDate()
#'
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.startDate",
#'   con = dbc)
#' }
#'
f.startDate <- function(df = NULL) {

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

    # generic, do not edit
    return(fldsNeeded)

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
    dplyr::pull(out) -> df[[".startDate"]]

  # keep only outcome columns
  df <- df[, c("_id", ".startDate"), drop = FALSE]


  #### checks ####
  stopifnot(
    inherits(df[[".startDate"]], "Date") ||
      inherits(df[[".startDate"]], "POSIXct"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.startDate
