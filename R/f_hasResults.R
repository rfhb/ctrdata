#### history ####
# 2025-07-18 first version

#' Calculate if a study's results are available
#'
#' Trial concept calculated: Calculates if results have been recorded in the
#' register, as structured data, reports or publications, for example.
#' Requires loading results-related information for EUCTR.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and new column `.hasResults` (logical).
#'
#' @export
#'
#' @importFrom dplyr mutate select `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.hasResults()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.hasResults",
#'   con = dbc)
#' trialsDf
#'
f.hasResults <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "endPoints.endPoint.readyForValues"
    ),
    "ctgov" = c(
      "results_reference.citation",
      "clinical_results.outcome_list.outcome.type"
    ),
    "ctgov2" = c(
      "hasResults",
      "protocolSection.referencesModule.references.type",
      "protocolSection.statusModule.resultsFirstSubmitDate",
      "resultsSection.outcomeMeasuresModule.outcomeMeasures.type"
    ),
    "isrctn" = c(
      "results.publicationStage"
      # "results.publicationDetails"
    ),
    "ctis" = c(
      "results.clinicalStudyReports.id",
      "results.laypersonResults.id",
      "results.summaryResults.id",
      "resultsFirstReceived"
    )
  )


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  df <- fctChkFlds(df, fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      #
      .hasResults = !is.na(.data$endPoints.endPoint.readyForValues)
      #
    ) -> df


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      .hasResults = .data$.hasResults |
        !is.na(.data$clinical_results.outcome_list.outcome.type) |
        !is.na(.data$results_reference.citation)
      #
    ) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      .hasResults = .data$.hasResults |
        !is.na(.data$hasResults) |
        !is.na(.data$protocolSection.statusModule.resultsFirstSubmitDate) |
        !is.na(.data$resultsSection.outcomeMeasuresModule.outcomeMeasures.type) | (
          !is.na(.data$protocolSection.referencesModule.references.type) &
            grepl("RESULT", .data$protocolSection.referencesModule.references.type))
      #
    ) -> df


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      #
      .hasResults = .data$.hasResults | (
        !is.na(.data$results.publicationStage) &
          grepl("Results", .data$results.publicationStage))
      #
    ) -> df


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      .hasResults = .data$.hasResults |
        !is.na(.data$results.clinicalStudyReports.id) |
        !is.na(.data$results.laypersonResults.id) |
        !is.na(.data$results.summaryResults.id) |
        (!is.na(.data$resultsFirstReceived) &
           .data$resultsFirstReceived)
      #
    ) -> df


  #### merge ####

  # keep only outcome columns
  df <- df[, c("_id", ".hasResults"), drop = FALSE]


  #### checks ####
  stopifnot(ncol(df) == 2L)
  stopifnot(inherits(df[[".hasResults"]], "logical"))

  # return
  return(df)

} # end f.hasResults
