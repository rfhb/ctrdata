#### history ####
# 2025-01-26 first version
# 2026-02-28 ensure primary EP even if not first in json, add euctr other method

#' Calculate details of a study's primary endpoint statistical testing
#'
#' Trial concept calculated: Calculates several results-related elements of
#' the primary statistical analysis of the primary endpoint. Requires loading
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
      "endPoints.endPoint.type.value",
      "endPoints.endPoint"
    ),
    "ctgov" = c(
      "clinical_results.outcome_list.outcome.type",
      "clinical_results.outcome_list.outcome"
    ),
    "ctgov2" = c(
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
      gsub("ionmial","inomial",
      tolower(x)
      ))))))

  }


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      #
      # only use information from the first primary endpoint
      isPrimEpsEuctr = sapply(
        .data$endPoints.endPoint.type.value,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "ENDPOINT_TYPE.primary")[1],
        USE.NAMES = FALSE),
      #
      # only use information from the first analysis
      primStatsEuctr = mapply(
        function(x, y) if (is.na(y)) NA else {
          o <- x[y, ]$statisticalAnalyses[[1]]
          if (is.atomic(o)) NA else o$statisticalAnalysis
        },
        x = .data$endPoints.endPoint,
        y = .data$isPrimEpsEuctr,
        SIMPLIFY = TRUE, USE.NAMES = FALSE),
      #
      #
      firstPvalueEuctr = sapply(
        .data$primStatsEuctr,
        FUN = function(x) if (is.atomic(x)) NA_real_ else {
          o <- x$statisticalHypothesisTest$value[1]
          as.numeric(o)
        }),
      #
      firstPmethodEuctr = sapply(
        .data$primStatsEuctr,
        FUN = function(x) if (is.atomic(x)) NA_character_ else {
          o <- x$statisticalHypothesisTest$method$value[1]
          if (o != "HYPOTHESIS_METHOD.other") o else {
            o <- x$statisticalHypothesisTest$otherMethod[1]
            normalise_string(o)
        }}),
      #
      firstPvalueGroups = sapply(
        .data$primStatsEuctr,
        FUN = function(x) if (is.atomic(x)) NA_character_ else {
          o <- x$subjectAnalysisSetComparisonGroupId[[1]]
          unlist(o)
        }),
      #
      firstPsizeEuctr = mapply(
        function(x, y, z) {
          #
          if (!is.data.frame(x)) return(NA_integer_) else x <- x[y, ]
          if (!any(grepl("ReportingGroups", names(x)))) return(NA_integer_)
          #
          o <- x$subjectAnalysisSetReportingGroups
          if (!is.null(o)) {
            o <- o[1][[1]][[1]] # subjectAnalysisSetReportingGroup
            if (is.data.frame(o)) {
              o <- o[o$id %in% z, ]
              o <- o[["subjects"]]
              return(sum(as.numeric(o)))
            } else return(NA_integer_)
          }
          #
          o <- x$armReportingGroups
          if (!is.null(o)) {
            browser() # TODO
            o <- o[1][[1]][[1]] # TODO armReportingGroup
            if (is.data.frame(o)) {
              o <- o[o$id %in% z, ]
              o <- o[["subjects"]]
              return(sum(as.numeric(o)))
            } else return(NA_integer_)
          }
          #
          return(NA_integer_) # just in case
        },
        x = .data$endPoints.endPoint,
        y = .data$isPrimEpsEuctr,
        z = .data$firstPvalueGroups,
        SIMPLIFY = TRUE, USE.NAMES = FALSE)
      #
    ) %>%
    select(!c("endPoints.endPoint", "endPoints.endPoint.type.value",
              "isPrimEpsEuctr", "primStatsEuctr",
              "firstPvalueGroups")) -> df


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      # only use information from the first primary endpoint
      isPrimEpsCtgov = sapply(
        .data$clinical_results.outcome_list.outcome.type,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "Primary")[1],
        USE.NAMES = FALSE),
      #
      primStatsCtgov = mapply(
        function(x, y) if (is.na(y)) NA else x[y, ],
        x = .data$clinical_results.outcome_list.outcome,
        y = .data$isPrimEpsCtgov,
        SIMPLIFY = TRUE, USE.NAMES = FALSE),
      #
      firstPvalueCtgov = sapply(
        .data$primStatsCtgov,
        function(x) {
          if (is.null(x)) return(NA_real_)
          o <- unlist(x)
          o <- unname(o["analysis_list.analysis.p_value"])
          as.numeric(trimws(gsub("[^0-9.,]", "", o)))
        }),
      #
      firstPmethodCtgov = sapply(
        .data$primStatsCtgov,
        function(x) {
          if (is.null(x)) return(NA_character_)
          o <- unlist(x)
          o <- unname(o["analysis_list.analysis.method"])
          normalise_string(o)
          }),
      #
      firstPsizeCtgov = sapply(
        .data$primStatsCtgov,
        function(x, y) {
          if (is.atomic(x)) return(NA_integer_)
          if (!any(grepl("measure", names(x)))) return(NA_integer_)
          x <- unlist(x)
          y <- grepl("measure.analyzed_list.analyzed.count_list.count.@attributes.value[0-9]*", names(x))
          if (!length(y) || !any(y)) return(NA_integer_)
          return(sum(as.integer(gsub(",", "", x[y]))))

        }
      )
      #
      # firstPvalueCtgov = lapply(
      #   .data$clinical_results.outcome_list.outcome.analysis_list.analysis.p_value,
      #   FUN = function(x) as.numeric(trimws(gsub("[^0-9.,]", "", stringi::stri_split_fixed(x, " / ")[[1]])))),
      # #
      # firstPvalueCtgov = mapply(
      #   function(x, y) if_else(length(x) < y, x[y], NA_real_),
      #   x = .data$firstPvalueCtgov,
      #   y = .data$isPrimEpsCtgov,
      #   SIMPLIFY = TRUE, USE.NAMES = FALSE),
      # #
      # #
      # firstPmethodCtgov = lapply(
      #   .data$clinical_results.outcome_list.outcome.analysis_list.analysis.method,
      #   FUN = function(x) normalise_string(stringi::stri_split_fixed(x, " / ")[[1]])),
      # #
      # firstPmethodCtgov = mapply(
      #   function(x, y) if_else(length(x) < y, x[y], NA_character_),
      #   x = .data$firstPmethodCtgov,
      #   y = .data$isPrimEpsCtgov,
      #   SIMPLIFY = TRUE, USE.NAMES = FALSE),
      #
      #
      # firstPsizeCtgov = mapply(
      #   function(x, y) {
      #     if (!is.data.frame(x)) return(NA_integer_)
      #     x <- x[y, ]
      #     # "measure.analyzed_list.analyzed.count_list.count.@attributes.value"
      #     if (any(grepl("measure", names(x)))) {
      #       x <- unlist(x)
      #       y <- grepl("measure.analyzed_list.analyzed.count_list.count.@attributes.value[0-9]*", names(x))
      #       if (!length(y) || !any(y)) return(NA_integer_)
      #       return(sum(as.numeric(gsub(",", "", x[y]))))
      #     } else return(NA_integer_)
      #   },
      #   x = .data$clinical_results.outcome_list.outcome,
      #   y = .data$isPrimEpsCtgov,
      #   SIMPLIFY = TRUE, USE.NAMES = FALSE
      # )
      #
    ) %>%
    select(!c("clinical_results.outcome_list.outcome.type",
              "clinical_results.outcome_list.outcome",
              "isPrimEpsCtgov", "primStatsCtgov")) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      # only use information from the first primary endpoint
      isPrimEpsCtgov2 = sapply(
        .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.type,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "PRIMARY")[1],
        USE.NAMES = FALSE),

      primStatsCtgov2 = mapply(
        function(x, y) if (is.na(y)) NA else x[y, ],
        x = .data$resultsSection.outcomeMeasuresModule.outcomeMeasures,
        y = isPrimEpsCtgov2,
        SIMPLIFY = TRUE, USE.NAMES = FALSE),
      #
      firstPvalueCtgov2 = sapply(
        primStatsCtgov2,
        function(x) {
          if (is.null(x)) return(NA_real_)
          o <- unlist(x)
          o <- unname(o["analyses.pValue"])
          as.numeric(trimws(gsub("[^0-9.,]", "", o)))
        }),
      #
      firstPmethodCtgov2 = sapply(
        primStatsCtgov2,
        function(x) {
          if (is.null(x)) return(NA_character_)
          o <- unlist(x)
          o <- unname(o["analyses.statisticalMethod"])
          normalise_string(o)
        }),
      #
      firstPsizeCtgov2 = sapply(
        primStatsCtgov2,
        function(x, y) {
          if (is.atomic(x)) return(NA_integer_)
          if (!any(grepl("denoms", names(x)))) return(NA_integer_)
          x <- unlist(x)
          y <- grepl("denoms.counts.value[0-9]*", names(x))
          if (!length(y) || !any(y)) return(NA_integer_)
          return(sum(as.integer(gsub(",", "", x[y]))))
        }
      )
      #
      #
      # firstPvalueCtgov2 = lapply(
      #   .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValue,
      #   FUN = function(x) as.numeric(trimws(gsub("[^0-9.,]", "", stringi::stri_split_fixed(x, " / ")[[1]])))),
      # #
      # firstPvalueCtgov2 = mapply(
      #   function(x, y) if_else(length(x) < y, x[y], NA_real_),
      #   x = .data$firstPvalueCtgov2,
      #   y = .data$isPrimEpsCtgov2,
      #   SIMPLIFY = TRUE, USE.NAMES = FALSE),
      # #
      # #
      # firstPmethodCtgov2 = lapply(
      #   .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.statisticalMethod,
      #   FUN = function(x) normalise_string(stringi::stri_split_fixed(x, " / ")[[1]])),
      # #
      # firstPmethodCtgov = mapply(
      #   function(x, y) if_else(length(x) < y, x[y], NA_character_),
      #   x = .data$firstPmethodCtgov2,
      #   y = .data$isPrimEpsCtgov2,
      #   SIMPLIFY = TRUE, USE.NAMES = FALSE),
      # #
      # #
      # firstPsizeCtgov2 = mapply(
      #   function(x, y) {
      #     if (!is.data.frame(x)) return(NA_integer_)
      #     x <- x[y, ]
      #     # "resultsSection.outcomeMeasuresModule.outcomeMeasures.denoms.counts.value"
      #     if (any(grepl("denoms", names(x)))) {
      #       x <- unlist(x)
      #       y <- grepl("denoms.counts.value[0-9]*", names(x))
      #       if (!length(y) || !any(y)) return(NA_integer_)
      #       return(sum(as.numeric(gsub(",", "", x[y]))))
      #     } else return(NA_integer_)
      #   },
      #   x = .data$resultsSection.outcomeMeasuresModule.outcomeMeasures,
      #   y = .data$isPrimEpsCtgov2,
      #   SIMPLIFY = TRUE, USE.NAMES = FALSE
      # )
      #
    ) %>%
    select(!c("resultsSection.outcomeMeasuresModule.outcomeMeasures.type",
              "resultsSection.outcomeMeasuresModule.outcomeMeasures",
              "isPrimEpsCtgov2", "primStatsCtgov2")) -> df


  #### . ISRCTN ####

  # no data fields, see above


  #### . CTIS ####

  # no data fields, see above


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
