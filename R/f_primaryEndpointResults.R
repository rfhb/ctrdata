#### history ####
# 2025-01-26 first version

#' Calculate details of a study's primary endpoint analysis and testing
#'
#' Trial concept calculated: Calculates several results-related elements of
#' the primary analysis of the primary endpoint. Requires loading
#' results-related information.
#' For CTIS and ISRCTN, such information is not available in structured format.
#' Recommended to be combined with .controlType, .sampleSize etc. for analyses.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and new columns:
#' `.primaryEndpointFirstPvalue` (discarding any inequality indicator, e.g. <=),
#' `.primaryEndpointFirstPmethod` (normalised string, e.g. chisquared),
#' `.primaryEndpointFirstPsize` (number included in test, across assignment groups).
#'
#' @export
#'
#' @importFrom dplyr mutate select coalesce `%>%`
#' @importFrom stringi stri_split_fixed
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.primaryEndpointResults()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.primaryEndpointResults",
#'   con = dbc)
#' trialsDf
#'
f.primaryEndpointResults <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  # four estimand components
  #
  # - *variable* (or outcome) to be obtained or measured for each individual
  #   participant that is required to address the scientific question,
  #   e.g. visual analogue score (VAS) at pre-specified visit times
  #
  # - *population*, referring to participants targeted with the scientific
  #   question, e.g. adults suffering from acute pain
  #
  # - *population-level summary* for the variable which provides a basis for a
  #   comparison between treatment conditions, e.g. the difference in VAS
  #   means between experimental and control arm at week 12
  #
  # - specification to account for *intercurrent events* to reflect the
  #   scientific question of interest; with strategies exemplified in E9(R1)
  #   (1) treatment policy, (2) hypothetical, (3) composite,
  #   (4) while on treatment and (5) principal stratum


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value", # ".0134 / 0.0031 / 0.0205 / 0.0053"
      "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value", # HYPOTHESIS_METHOD.other / HYPOTHESIS_METHOD.other
      "endPoints.endPoint.type.value", # ENDPOINT_TYPE.primary / ENDPOINT_TYPE.primary
      "endPoints.endPoint"
    ),
    "ctgov" = c(
      "clinical_results.outcome_list.outcome.analysis_list.analysis.method", # Chi-squared / Chi-squared
      "clinical_results.outcome_list.outcome.analysis_list.analysis.p_value", # "&gt;0.1 / =0.1"
      "clinical_results.outcome_list.outcome.type",
      "clinical_results.outcome_list.outcome"
    ),
    "ctgov2" = c(
      "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValue", # "0.0044 / 0.0919 / 0.2364"
      "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.statisticalMethod", # "Chi-squared / t-test, 1 sided / t-test, 1 sided"
      "resultsSection.outcomeMeasuresModule.outcomeMeasures.type",
      "resultsSection.outcomeMeasuresModule.outcomeMeasures"
    ),
    "isrctn" = c(
      # No relevant structured data
    ),
    "ctis" = c(
      # No relevant structured data
    ))


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

  # helper function
  normalise_string <- function(x) {

    # this is quite drastic but minimises
    # ambiguities and different spellings
    trimws(
    gsub("hypothesis|method", "",
    gsub("[^a-z1-2 ]", "",
    gsub("[ ]+", "",
    gsub("two", "2",
    tolower(x)
    )))))

  }


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      #
      firstPvalueEuctr = vapply(
        .data$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value,
        FUN = function(x) as.numeric(trimws(gsub("[^0-9.,]", "", stringi::stri_split_fixed(x, " / ")[[1]][1]))),
        numeric(1L), USE.NAMES = FALSE),
      #
      firstPmethodEuctr = vapply(
        .data$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value,
        FUN = function(x) normalise_string(stringi::stri_split_fixed(x, " / ")[[1]][1]),
        character(1L), USE.NAMES = FALSE),
      #
      # only use information from the first primary endpoint
      isPrimEpsEuctr = sapply(
        .data$endPoints.endPoint.type.value,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "ENDPOINT_TYPE.primary")[1],
        USE.NAMES = FALSE),
      #
      firstPsizeEuctr = mapply(
        function(x, y) {
          if (!is.data.frame(x)) return(NA_integer_)
          x <- x[y, ]
          # "armReportingGroups" or "subjectAnalysisSetReportingGroups"
          if (any(grepl("ReportingGroups", names(x)))) {
            x <- unlist(x)
            y <- grepl("armReportingGroups.+subjects[0-9]*", names(x))
            if (!length(y) || !any(y)) {
              y <- grepl("subjectAnalysisSetReportingGroups.+subjects[0-9]*", names(x))
            }
            if (!length(y) || !any(y)) return(NA_integer_)
            return(sum(as.numeric(x[y])))
          } else return(NA_integer_)
        },
        x = .data$endPoints.endPoint,
        y = .data$isPrimEpsEuctr,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      )
      #
    ) -> df


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      firstPvalueCtgov = vapply(
        .data$clinical_results.outcome_list.outcome.analysis_list.analysis.p_value,
        FUN = function(x) as.numeric(trimws(gsub("[^0-9.,]", "", stringi::stri_split_fixed(x, " / ")[[1]][1]))),
        numeric(1L), USE.NAMES = FALSE),
      #
      firstPmethodCtgov = vapply(
        .data$clinical_results.outcome_list.outcome.analysis_list.analysis.method,
        FUN = function(x) normalise_string(stringi::stri_split_fixed(x, " / ")[[1]][1]),
        character(1L), USE.NAMES = FALSE),
      #
      # only use information from the first primary endpoint
      isPrimEpsCtgov = sapply(
        .data$clinical_results.outcome_list.outcome.type,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "Primary")[1],
        USE.NAMES = FALSE),
      #
      firstPsizeCtgov = mapply(
        function(x, y) {
          if (!is.data.frame(x)) return(NA_integer_)
          x <- x[y, ]
          # "measure.analyzed_list.analyzed.count_list.count.@attributes.value"
          if (any(grepl("measure", names(x)))) {
            x <- unlist(x)
            y <- grepl("measure.analyzed_list.analyzed.count_list.count.@attributes.value[0-9]*", names(x))
            if (!length(y) || !any(y)) return(NA_integer_)
            return(sum(as.numeric(x[y])))
          } else return(NA_integer_)
        },
        x = .data$clinical_results.outcome_list.outcome,
        y = .data$isPrimEpsCtgov,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      )
      #
    ) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      firstPvalueCtgov2 = vapply(
        .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValue,
        FUN = function(x) as.numeric(trimws(gsub("[^0-9.,]", "", stringi::stri_split_fixed(x, " / ")[[1]][1]))),
        numeric(1L), USE.NAMES = FALSE),
      #
      firstPmethodCtgov2 = vapply(
        .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.statisticalMethod,
        FUN = function(x) normalise_string(stringi::stri_split_fixed(x, " / ")[[1]][1]),
        character(1L), USE.NAMES = FALSE),
      #
      # only use information from the first primary endpoint
      isPrimEpsCtgov2 = sapply(
        .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.type,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "PRIMARY")[1],
        USE.NAMES = FALSE),
      #
      firstPsizeCtgov2 = mapply(
        function(x, y) {
          if (!is.data.frame(x)) return(NA_integer_)
          x <- x[y, ]
          # "resultsSection.outcomeMeasuresModule.outcomeMeasures.denoms.counts.value"
          if (any(grepl("denoms", names(x)))) {
            x <- unlist(x)
            y <- grepl("denoms.counts.value[0-9]*", names(x))
            if (!length(y) || !any(y)) return(NA_integer_)
            return(sum(as.numeric(x[y])))
          } else return(NA_integer_)
        },
        x = .data$resultsSection.outcomeMeasuresModule.outcomeMeasures,
        y = .data$isPrimEpsCtgov2,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      )
      #
    ) -> df


  #### . ISRCTN ####


  #### . CTIS ####


  #### merge ####
  df %>%
    dplyr::mutate(
      .primaryEndpointFirstPvalue = dplyr::coalesce(
        .data$firstPvalueEuctr, .data$firstPvalueCtgov2, .data$firstPvalueCtgov, .ptype = double()),
      .primaryEndpointFirstPmethod = dplyr::coalesce(
        .data$firstPmethodEuctr, .data$firstPmethodCtgov2, .data$firstPmethodCtgov, .ptype = character()),
      .primaryEndpointFirstPsize = dplyr::coalesce(
        .data$firstPsizeEuctr, .data$firstPsizeCtgov2, .data$firstPsizeCtgov, .ptype = numeric())
    ) %>%
    # keep only outcome columns
    dplyr::select(c(
      "_id",
      ".primaryEndpointFirstPvalue",
      ".primaryEndpointFirstPmethod",
      ".primaryEndpointFirstPsize"
    )) -> df


  #### checks ####
  stopifnot(ncol(df) == 4L)
  stopifnot(inherits(df[[".primaryEndpointFirstPvalue"]], "numeric"))
  stopifnot(inherits(df[[".primaryEndpointFirstPmethod"]], "character"))
  stopifnot(inherits(df[[".primaryEndpointFirstPsize"]], "numeric"))

  # return
  return(df)

} # end f.primaryEndpointResults
