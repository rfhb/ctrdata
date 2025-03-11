#### history ####
# 2025-03-09 first version

#' Calculate the title of a study
#'
#' Trial concept calculated: scientific or full title of the study.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.trialTitle`, a string.
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
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.trialTitle",
#'   con = dbc)
#' }
#'
f.trialTitle <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      # "trialInformation.fullTitle",
      "a3_full_title_of_the_trial" #,
      # "a31_title_of_the_trial_for_lay_people_in_easily_understood_ie_nontechnical_language"
    ),
    "ctgov" = c(
      "official_title" #,
      # "brief_title"
    ),
    "ctgov2" = c(
      "protocolSection.identificationModule.officialTitle" #,
      # "protocolSection.identificationModule.briefTitle"
    ),
    "isrctn" = c(
      "trialDescription.scientificTitle" #,
      # "trialDescription.title"
    ),
    "ctis" = c(
      "authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle", # ctis1
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle" # ctis2
      # "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      # "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.shortTitle"
    ))

  # only use first field
  # fldsNeeded <- lapply(fldsNeeded, "[[", 1)


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

  # helper function similar to unite
  pasteCols <- function(...) apply(
    ..., 1, function(i) paste(na.omit(i)[1], collapse = " "))


  #### CTIS ####

  # first column for all registers
  df[[".trialTitle"]] <- pasteCols(
    dplyr::select(
      df, unlist(fldsNeeded, use.names = FALSE)
    )
  )

  # keep only outcome columns
  df <- df[, c("_id", ".trialTitle"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".trialTitle"]], "character"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.trialTitle
