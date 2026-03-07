#### history ####
# 2025-01-26 first version
# 2026-02-28 ensure primary EP even if not first in json, add euctr other method

#' Calculate details of a study's primary endpoint statistical testing
#'
#' Trial concept calculated: Calculates several results-related elements of
#' the primary statistical analysis of the primary endpoint. Requires loading
#' results-related information.
#' For CTIS and ISRCTN, such information is not available in structured format.
#' Recommended to be combined with \link{f.controlType}, \link{f.sampleSize},
#' \link{f.assignmentType} and other \link{ctrdata-trial-concepts} for analyses.
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
      # only use information from the first statistical analysis
      primStatsEuctr = mapply(
        function(o, y) if (is.na(y)) NA else {
          # cater for different data structures, "o"
          # can correspond to array, list, data frame
          while ((is.list(o) && length(o) == 1L) ||
                 (is.data.frame(o) && (ncol(o) == 1L))) o <- o[[1]]
          #
          if (is.data.frame(o)) {
            o <- o[y, ]
            o <- as.list(o)
          } else {
            o <- o[[1]]
            if (length(o) == 1L) o <- o[[1]]
          }
          #
          o <- o$statisticalAnalyses
          while ((is.list(o) && length(o) == 1L) ||
                 (is.data.frame(o) && (ncol(o) == 1L))) o <- o[[1]]
          #
          if (is.data.frame(o)) {
            o <- o[1, ]
            o <- as.list(o)
          }
          #
          if (is.atomic(o) && o == "") return(NA)
          o
        },
        o = .data$endPoints.endPoint,
        y = .data$isPrimEpsEuctr,
        SIMPLIFY = TRUE, USE.NAMES = FALSE),
      #
      #
      firstPvalueEuctr = sapply(
        .data$primStatsEuctr,
        FUN = function(x) if (is.atomic(x)) NA_real_ else {
          o <- x$statisticalHypothesisTest$value[1]
          if (is.null(o) || is.na(o)) return(NA_real_)
          as.numeric(trimws(gsub("[^0-9.,]", "", o)))
        }, simplify = TRUE, USE.NAMES = FALSE),
      #
      firstPmethodEuctr = sapply(
        .data$primStatsEuctr,
        FUN = function(x) if (is.atomic(x)) NA_character_ else {
          o <- x$statisticalHypothesisTest$method$value[1]
          if (is.null(o) || is.na(o)) return(NA_character_)
          if (o != "HYPOTHESIS_METHOD.other") o else {
            o <- x$statisticalHypothesisTest$otherMethod[1]
          }
          normalise_string(o)
        }, simplify = TRUE, USE.NAMES = FALSE),
      #
      firstPvalueGroups = lapply(
        .data$primStatsEuctr,
        FUN = function(x) if (is.atomic(x)) NA_character_ else {
          o1 <- x$subjectAnalysisSetComparisonGroupId
          o2 <- x$armComparisonGroupId
          unique(c(unlist(o1), unlist(o2)))
        }),
      #
      firstPsizeEuctr = mapply(
        function(x, y, z) {
          #
          if (all(is.na(z))) return(NA_integer_)
          if (!is.data.frame(x)) return(NA_integer_) else x <- x[y, ]
          if (!any(grepl("ReportingGroups", names(x)))) return(NA_integer_)
          #
          # after review of results for various trials,
          # the subject numbers should be added up across
          # relevant rows from both o1 and o2 for the
          # statistical analysis
          o1 <- x$subjectAnalysisSetReportingGroups
          o2 <- x$armReportingGroups
          #
          while ((is.list(o1) && length(o1) == 1L) ||
                 (is.data.frame(o1) && (ncol(o1) == 1L))) o1 <- o1[[1]]
          while ((is.list(o2) && length(o2) == 1L) ||
                 (is.data.frame(o2) && (ncol(o2) == 1L))) o2 <- o2[[1]]
          #
          if (is.data.frame(o1)) o1 <- o1[o1$id %in% z, ]
          if (!any("subjects" == names(o1))) o1 <-
            NA_integer_ else o1 <- as.numeric(o1$subjects)
          if (is.data.frame(o1)) o1 <- o1[o1$id %in% z, ]
          #
          if (is.data.frame(o2)) o2 <- o2[o2$id %in% z, ]
          if (!any("subjects" == names(o2))) o2 <-
            NA_integer_ else o2 <- as.numeric(o2$subjects)
          #
          return(sum(o1, o2, na.rm = TRUE))
        },
        x = .data$endPoints.endPoint,
        y = .data$isPrimEpsEuctr,
        z = .data$firstPvalueGroups,
        SIMPLIFY = TRUE, USE.NAMES = FALSE)
      #
    ) %>%
    select(
      !c("endPoints.endPoint", "endPoints.endPoint.type.value",
         "isPrimEpsEuctr", "primStatsEuctr",
         "firstPvalueGroups")) -> df


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      # only use information from first primary endpoint
      isPrimEpsCtgov = sapply(
        .data$clinical_results.outcome_list.outcome.type,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "Primary")[1],
        simplify = TRUE, USE.NAMES = FALSE),
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
          o <- o[grepl("analysis_list.analysis.p_value", names(o))]
          # first p value
          o <- unname(o[1])
          as.numeric(trimws(gsub("[^0-9.,]", "", o)))
        }, simplify = TRUE, USE.NAMES = FALSE),
      #
      firstPmethodCtgov = sapply(
        .data$primStatsCtgov,
        function(x) {
          if (is.null(x)) return(NA_character_)
          o <- unlist(x)
          o <- o[grepl("analysis_list.analysis.method", names(o))]
          o <- unname(o[1])
          normalise_string(o)
        }, simplify = TRUE, USE.NAMES = FALSE),
      #
      firstPsizeCtgov = sapply(
        .data$primStatsCtgov,
        function(x, y) {
          if (is.atomic(x)) return(NA_integer_)
          if (!any(grepl("measure", names(x)))) return(NA_integer_)
          x <- unlist(x)
          y <- grepl("measure.analyzed_list.analyzed.count_list.count.(@attributes.|)value[0-9]*", names(x))
          if (!length(y) || !any(y)) return(NA_integer_)
          return(sum(as.integer(gsub("[,.]", "", x[y]))))
        }, simplify = TRUE, USE.NAMES = FALSE)
    ) %>%
    select(
      !c("clinical_results.outcome_list.outcome.type",
         "clinical_results.outcome_list.outcome",
         "isPrimEpsCtgov", "primStatsCtgov")) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      # only use information from first primary endpoint
      isPrimEpsCtgov2 = sapply(
        .data$resultsSection.outcomeMeasuresModule.outcomeMeasures.type,
        function(x) which(stringi::stri_split_fixed(x, " / ")[[1]] == "PRIMARY")[1],
        simplify = TRUE, USE.NAMES = FALSE),

      primStatsCtgov2 = mapply(
        function(x, y) if (is.na(y)) NA else x[y, ],
        x = .data$resultsSection.outcomeMeasuresModule.outcomeMeasures,
        y = .data$isPrimEpsCtgov2,
        SIMPLIFY = TRUE, USE.NAMES = FALSE),
      #
      firstPvalueCtgov2 = sapply(
        .data$primStatsCtgov2,
        function(x) {
          if (is.null(x)) return(NA_real_)
          o <- unlist(x)
          o <- unname(o["analyses.pValue"])
          as.numeric(trimws(gsub("[^0-9.,]", "", o)))
        }, simplify = TRUE, USE.NAMES = FALSE),
      #
      firstPmethodCtgov2 = sapply(
        .data$primStatsCtgov2,
        function(x) {
          if (is.null(x)) return(NA_character_)
          o <- unlist(x)
          o <- unname(o["analyses.statisticalMethod"])
          normalise_string(o)
        }, simplify = TRUE, USE.NAMES = FALSE),
      #
      firstPsizeCtgov2 = sapply(
        .data$primStatsCtgov2,
        function(x, y) {
          if (is.atomic(x)) return(NA_integer_)
          if (!any(grepl("denoms", names(x)))) return(NA_integer_)
          x <- unlist(x)
          y <- grepl("denoms.counts.value[0-9]*", names(x))
          if (!length(y) || !any(y)) return(NA_integer_)
          return(sum(as.integer(gsub("[,.]", "", x[y]))))
        }, simplify = TRUE, USE.NAMES = FALSE)
    ) %>%
    select(
      !c("resultsSection.outcomeMeasuresModule.outcomeMeasures.type",
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
        .data$firstPsizeEuctr, .data$firstPsizeCtgov2, .data$firstPsizeCtgov, .ptype = numeric()),
      .primaryEndpointFirstPsize = if_else(
        is.na(.data$.primaryEndpointFirstPvalue), NA_integer_, .data$.primaryEndpointFirstPsize)
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
