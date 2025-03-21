#### history ####
# 2025-02-26 first version

#' Calculate details of a primary endpoint of a study
#'
#' Trial concept calculated: full description of the primary endpoint,
#' concatenating with " == " its title, description, time frame of assessment.
#' The details vary by register. The text description can be used for
#' identifying trials of interest or for analysing trends in primary
#' endpoints, which among the set of all endpoints are most often used
#' for determining the number of participants sought for the study.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.primaryEndpointDescription`,
#' which is a list (that is, one or more items in one vector per row; the
#' background is that some trials have several endpoints as primary).
#'
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom stringi stri_split_regex
#'
#' @examples
#' # fields needed
#' f.primaryEndpointDescription()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO
#' )
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.primaryEndpointDescription",
#'   con = dbc
#' )
#' trialsDf
#'
f.primaryEndpointDescription <- function(df = NULL) {
  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  # four estimand components
  #
  # - V *variable* (or outcome) to be obtained or measured for each individual
  #   participant that is required to address the scientific question,
  #   e.g. visual analogue score (VAS) at pre-specified visit times
  #
  # - P *population*, referring to participants targeted with the scientific
  #   question, e.g. adults suffering from acute pain
  #
  # - S *population-level summary* for the variable which provides a basis for a
  #   comparison between treatment conditions, e.g. the difference in VAS
  #   means between experimental and control arm at week 12
  #
  # - I specification to account for *intercurrent events* to reflect the
  #   scientific question of interest; with strategies exemplified in E9(R1)
  #   (1) treatment policy, (2) hypothetical, (3) composite,
  #   (4) while on treatment and (5) principal stratum


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      #
      # V, S
      # protocol-related information
      "e51_primary_end_points", # V, often V + S
      "e511_timepoints_of_evaluation_of_this_end_point" # part of S
      #
      # results-related information
      # "endPoints.endPoint.timeFrame",  # S part
      # "endPoints.endPoint.title",      # V, often V + S, " / "
      # "endPoints.endPoint.unit",       # S, e.g. Number of patients / Score
      # "endPoints.endPoint.centralTendencyType.value", # S, e.g. MEASURE_TYPE.number, MEASURE_TYPE.arithmetic
      # "endPoints.endPoint.countable",  # TRUE / FALSE
      # "endPoints.endPoint.description", # V + S, long text
      # "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.value", # S PARAMETER_TYPE.meanDiffFinal
      # P
      # "e3_principal_inclusion_criteria", # P
      # "subjectAnalysisSets.subjectAnalysisSet.description", # P / V / S
      # I
      # "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.description" # S perhaps I,
      # "Clinical Dementia Rating - Sum of Boxes (CDR-SB) Score Mixed Model Repeated Measures (MMRM)
      # Analysis of Change from Baseline after two years of treatment / Alzheimer&apos;s
      # Disease Co-operative Study — Activities of Daily Living (Mild Cognitive Impairment Version)
      # (ADCS-MCI-ADL) Mixed Model Repeated Measures (MMRM) Analysis of Change from Baseline at Visit 16 (Week 104) ..."
      #
      # "baselineCharacteristics.baselineReportingGroups.baselineReportingGroup.description", # only groups
      # "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.analysisSpecification.value" # ANALYSIS_SPEC.preSpecified
      # "endPoints.endPoint.type.value", # ENDPOINT_TYPE.primary
      # "endPoints.endPoint.categories",
    ),
    "ctgov" = c(
      #
      # V, S
      "primary_outcome.measure",
      "primary_outcome.description",
      "primary_outcome.time_frame"
      #
      # P
      # "eligibility.criteria.textblock",
      #
      # I
      # "clinical_results.outcome_list.outcome.analysis_list.analysis.method_desc",
      # "clinical_results.outcome_list.outcome.measure.population",
      # "clinical_results.outcome_list.outcome.time_frame",
      # "clinical_results.outcome_list.outcome.title",
      # "clinical_results.outcome_list.outcome.type"
    ),
    "ctgov2" = c(
      "protocolSection.outcomesModule.primaryOutcomes.measure",
      "protocolSection.outcomesModule.primaryOutcomes.description",
      "protocolSection.outcomesModule.primaryOutcomes.timeFrame"
    ),
    "isrctn" = c(
      "trialDescription.primaryOutcome"
      # "trialDescription.studyHypothesis"
    ),
    "ctis" = c(
      #
      # S, V
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.endPoint", # V, S " / "
      # "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary"
      #
      "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.endPoint" # V, S " / "
      # "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary"
      #
      # P
      # "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria"
      #
      # I
      #
    )
  )


  #### describe ####
  if (is.null(df)) {
    # generic, do not edit
    return(fldsNeeded)
  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper function similar to unite and also splitting
  # by contained primary endpoints and unite corresponding
  # parts of endpoints from across columns
  pasteCols <- function(...) {
    apply(..., 1, function(i) {
      # using a positive lookahead, which is a non-consuming pattern
      # that only checks for an uppercase letter to the right of the
      # separator without adding it to the match, preserving it in the output
      s <- stringi::stri_split_regex(na.omit(i), " / (?=[0-9A-Z])")
      # in different number of splits per column, append NA
      # to splits of less than max length, to fill data frame
      l <- sapply(s, length)
      if (length(unique(l)) != 1L) {
        s <- lapply(
          s, function(i) c(i, NA[seq_len(max(l) - length(i))])
        )
      }
      o <- unlist(apply(data.frame(s), 1, paste, collapse = " == "))
      if (!length(o)) NA else o
    })
  }


  #### . all ####
  dplyr::mutate(
    df,
    .primaryEndpointDescription = pasteCols(dplyr::select(
      df, unlist(fldsNeeded, use.names = FALSE)
    ))
  ) -> df

  # keep only outcome columns
  df <- df[, c("_id", ".primaryEndpointDescription"), drop = FALSE]


  #### checks ####
  stopifnot(ncol(df) == 2L)
  stopifnot(
    inherits(df[[".primaryEndpointDescription"]], "list") ||
    inherits(df[[".primaryEndpointDescription"]], "logical") # if NA
  )

  # return
  return(df)
} # end .primaryEndpointDescription
