# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when rowwise ungroup `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_detect_fixed
.isPlatformTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "a3_full_title_of_the_trial",
      "trialInformation.fullTitle",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title"
    ),
    "ctgov" = c(
      "official_title",
      "detailed_description.textblock",
      "condition",
      "clinical_results.participant_flow.period_list.period.title"
    ),
    "ctgov2" = c(
      "protocolSection.identificationModule.officialTitle",
      "protocolSection.descriptionModule.detailedDescription",
      "resultsSection.participantFlowModule.periods.title"
    ),
    "isrctn" = c(
      "trialDescription.title",
      "trialDescription.scientificTitle",
      "interventions.intervention.interventionType"
    ),
    "ctis" = c(
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle",
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title"
    ))

  # merge with fields needed for nested function
  fldsAdded <- suppressMessages(.numTestArmsSubstances())
  fldsNeeded <- sapply(names(fldsNeeded), function(i) na.omit(c(
    fldsNeeded[[i]], fldsAdded[[i]])), simplify = FALSE)


  # not relevant after inspection:
  #
  # CTGOV2
  # "protocolSection.armsInterventionsModule.interventions.name"
  #
  # ISRCTN
  # "interventions.intervention.description"


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates if the trial is likely a platform trial or not.
As operational definition, at least one of these criteria is true:

- trial has "platform", "basket" or "umbrella" in its title or description (for
ISRCTN, this is the only criterion; some trials in EUCTR lack data in English)
- trial has more than 2 active arms with different investigational medicines,
after excluding comparator, auxiliary and placebo medicines (calculated with
function .numTestArmsSubstances())
- trial more than 2 periods, after excluding screening, extension and
follow-up periods (for CTGOV and CTGOV2, this criterion requires results-related
data)

Requires packages dplyr and stringdist to be installed; stringdist is used for
evaluating names of active substances, which are considered similar when the
similarity is 0.8 or higher.

Returns a logical.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper definitions
  thresholdSimilar <- 0.8
  titleDefPlatform <- "basket|platform|umbrella"
  minNumArmsDefPlatform <- 3L
  minNumPeriodsDefPlatform <- 3L

  # helper function
  `%>%` <- dplyr::`%>%`

  # apply nested function
  df$analysis_numberDifferentTestArms <- .numTestArmsSubstances(df = df)


  #### ..EUCTR ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_titleRelevant =
        stringi::stri_detect_fixed(
          a3_full_title_of_the_trial,
          titleDefPlatform, case_insensitive=TRUE) |
        stringi::stri_detect_fixed(
          trialInformation.fullTitle,
          titleDefPlatform, case_insensitive=TRUE),
      helper_periodTitle =
        list(strsplit(
          subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title,
          " / ", fixed = TRUE)[[1]]
        ),
      analysis_numberTestPeriods = sum(
        sapply(
          helper_periodTitle, function(i) !grepl("screen|follow|exten", unique(i), ignore.case = TRUE),
          USE.NAMES = FALSE)
      ),
      out = analysis_titleRelevant |
        analysis_numberDifferentTestArms >= minNumArmsDefPlatform |
        analysis_numberTestPeriods >= minNumPeriodsDefPlatform
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$euctr


  #### ..CTGOV ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_titleRelevant =
        stringi::stri_detect_fixed(
          official_title,
          titleDefPlatform, case_insensitive=TRUE) |
        stringi::stri_detect_fixed(
          detailed_description.textblock,
          titleDefPlatform, case_insensitive=TRUE),
      helper_periodTitle =
        list(strsplit(
          subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title,
          " / ", fixed = TRUE)[[1]]
        ),
      analysis_numberTestPeriods = sum(
        sapply(
          helper_periodTitle, function(i) !grepl("screen|follow|exten", unique(i), ignore.case = TRUE),
          USE.NAMES = FALSE)
      ),
      out = analysis_titleRelevant |
        analysis_numberDifferentTestArms >= minNumArmsDefPlatform |
        analysis_numberTestPeriods >= minNumPeriodsDefPlatform
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctgov


  #### ..CTGOV2 ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_titleRelevant =
        stringi::stri_detect_fixed(
          protocolSection.identificationModule.officialTitle,
          titleDefPlatform, case_insensitive=TRUE) |
        stringi::stri_detect_fixed(
          protocolSection.descriptionModule.detailedDescription,
          titleDefPlatform, case_insensitive=TRUE),
      helper_periodTitle =
        list(strsplit(
          resultsSection.participantFlowModule.periods.title,
          " / ", fixed = TRUE)[[1]]
        ),
      analysis_numberTestPeriods = sum(
        sapply(
          helper_periodTitle, function(i) !grepl("screen|follow|exten", unique(i), ignore.case = TRUE),
          USE.NAMES = FALSE)
      ),
      out = analysis_titleRelevant |
        analysis_numberDifferentTestArms >= minNumArmsDefPlatform |
        analysis_numberTestPeriods >= minNumPeriodsDefPlatform
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctgov2


  #### ..ISRCTN ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_titleRelevant =
        stringi::stri_detect_fixed(
          trialDescription.scientificTitle,
          titleDefPlatform, case_insensitive=TRUE) |
        stringi::stri_detect_fixed(
          trialDescription.title,
          titleDefPlatform, case_insensitive=TRUE),
      analysis_isDrugTrial =
        stringi::stri_detect_fixed(
          interventions.intervention.interventionType,
          "drug", case_insensitive = TRUE),
      out = analysis_titleRelevant & analysis_isDrugTrial,
      out = dplyr::if_else(is.na(trialDescription.title), NA, out)
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$isrctn


  #### ..CTIS ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_titleRelevant =
        stringi::stri_detect_fixed(
          authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle,
          titleDefPlatform, case_insensitive=TRUE) |
        stringi::stri_detect_fixed(
          authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle,
          titleDefPlatform, case_insensitive=TRUE),
      helper_periodTitle =
        list(strsplit(
          authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title,
          " / ", fixed = TRUE)[[1]]
        ),
      analysis_numberTestPeriods = sum(
        sapply(
          helper_periodTitle, function(i) !grepl("screen|follow|exten", unique(i), ignore.case = TRUE),
          USE.NAMES = FALSE)
      ),
      out = analysis_titleRelevant |
        analysis_numberDifferentTestArms >= minNumArmsDefPlatform |
        analysis_numberTestPeriods >= minNumPeriodsDefPlatform
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  )


  #### checks ####
  stopifnot(is.logical(vct))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .isPlatformTrial
