# function definition for dfCalculate

#### history ####
# 2025-02-26 first version

# @export

#' @noRd
#' @importFrom dplyr if_else mutate case_when `%>%`
#' @importFrom stringi stri_split_fixed
.primaryEndpointDescription <- function(df = NULL) {

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
      "e51_primary_end_points",        # V, often V + S
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

      # not useful:
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

      # S, V
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.endPoint", # V, S " / "
      # "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary"# ,
      #
      "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.endPoint"#, # V, S " / "
      # "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary"
      #
      # P
      # "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria"

      # I

    ))

  # not relevant after inspection:
  #


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the full description of the primary endpoint, including e.g. its
title, description, timeframe of assessment. The details vary by register.
The text description can be used for identifying trials of interest or for
analysing trends in primary endpoints, which among the set of all endpoints
are most often used for determining the number of participants sought for the
study.

Returns a list (that is, one or more items in one vector per row; the reason
is a proportion of records lists several endpoints as primary).
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

  # helper function similar to unite and also splitting
  # by contained primary endpoints and unite across columns
  pasteCols <- function(...) apply(..., 1, function(i) {
    s <- stringi::stri_split_fixed(na.omit(i), " / ")
    list(apply(data.frame(s), 1, paste, collapse = " == "))[[1]]
  })


  #### . all ####
  dplyr::mutate(
    df, .primaryEndpointDescription = dplyr::select(
      df, unlist(fldsNeeded, use.names = FALSE)
    ) %>% pasteCols(.)
  ) -> df


  # keep only outcome columns
  df <- df[, c("_id", ".primaryEndpointDescription"), drop = FALSE]

  #### checks ####
  stopifnot(inherits(df[[".primaryEndpointDescription"]], "list"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .primaryEndpointDescription
