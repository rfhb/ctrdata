# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when rowwise ungroup `%>%`
#' @importFrom stringdist stringsimmatrix
.isPlatformTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "a3_full_title_of_the_trial",
      "trialInformation.fullTitle",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.title",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title"
    ),
    "ctgov" = c(
      "official_title",
      "detailed_description.textblock",
      "condition",
      "number_of_arms",
      "arm_group.arm_group_label",
      "arm_group.arm_group_type",
      "clinical_results.participant_flow.period_list.period.title"
    ),
    "ctgov2" = c(
      "protocolSection.identificationModule.officialTitle",
      "protocolSection.descriptionModule.detailedDescription",
      "protocolSection.armsInterventionsModule.armGroups.label",
      "protocolSection.armsInterventionsModule.armGroups.type",
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
      "authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName",
      "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.productDictionaryInfo.activeSubstanceName",
      "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title"
    ))

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
after excluding comparator, auxiliary and placebo medicines
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
    return(invisible(unlist(fldsNeeded)))

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

  # helper function
  asTestSimilarity <- function(x) {

    x <- tolower(unlist(x))
    t <- stringdist::stringsimmatrix(x, x)

    if (ncol(t) < 1L) return(0L)
    diag(t) <- NA
    if (all(is.na(t))) return(0L)
    apply(t, 2, max, na.rm = TRUE)

  }


  #### ..EUCTR ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_titleRelevant =
        grepl(
          titleDefPlatform,
          a3_full_title_of_the_trial,
          ignore.case = TRUE
        ) | grepl(
          titleDefPlatform,
          trialInformation.fullTitle,
          ignore.case = TRUE
        ),
      helper_multipleActiveArms =
        stringi::stri_count_fixed(
          subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value,
          "ARM_TYPE.experimental"),
      helper_asName =
        list(strsplit(
          subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.title,
          " / ", fixed = TRUE)[[1]]),
      helper_prodRole =
        list(strsplit(
          subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value,
          " / ", fixed = TRUE)[[1]]),
      helper_isTest =
        list(
          sapply(helper_prodRole, function(i) i == "ARM_TYPE.experimental", USE.NAMES = FALSE)
        ),
      helper_asTest =
        list(
          sapply(seq_along(helper_asName), function(i) {
            t <- helper_asName[i][helper_isTest[i]]
            c <- nchar(t)
            na.omit(t[c > 0L])
          }, USE.NAMES = FALSE)
        ),
      helper_simTest = list(
        asTestSimilarity(helper_asTest)
      ),
      analysis_numberDifferentTestArms = list(
        helper_multipleActiveArms -
          max(c(0L, sum(sapply(helper_simTest, function(i) i >= thresholdSimilar)) - 1L), na.rm = TRUE)
      ),
      helper_periodTitle =
        list(strsplit(
          clinical_results.participant_flow.period_list.period.title,
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
        grepl(
          titleDefPlatform,
          official_title,
          ignore.case = TRUE
        ) | grepl(
          titleDefPlatform,
          detailed_description.textblock,
          ignore.case = TRUE
        ),
      helper_multipleActiveArms =
        stringi::stri_count_fixed(
          arm_group.arm_group_type,
          "Experimental"),
      helper_asName =
        list(strsplit(
          arm_group.arm_group_label,
          " / ", fixed = TRUE)[[1]]),
      helper_prodRole =
        list(strsplit(
          arm_group.arm_group_type,
          " / ", fixed = TRUE)[[1]]),
      helper_isTest =
        list(
          sapply(helper_prodRole, function(i) i == "Experimental", USE.NAMES = FALSE)
        ),
      helper_asTest =
        list(
          sapply(seq_along(helper_asName), function(i) {
            t <- helper_asName[i][helper_isTest[i]]
            c <- nchar(t)
            na.omit(t[c > 0L])
          }, USE.NAMES = FALSE)
        ),
      helper_simTest = list(
        asTestSimilarity(helper_asTest)
      ),
      analysis_numberDifferentTestArms = list(
        helper_multipleActiveArms -
          max(c(0L, sum(sapply(helper_simTest, function(i) i >= thresholdSimilar)) - 1L), na.rm = TRUE)
      ),
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
        grepl(
          titleDefPlatform,
          protocolSection.identificationModule.officialTitle,
          ignore.case = TRUE
        ) | grepl(
          titleDefPlatform,
          protocolSection.descriptionModule.detailedDescription,
          ignore.case = TRUE
        ),
      helper_multipleActiveArms =
        stringi::stri_count_fixed(
          protocolSection.armsInterventionsModule.armGroups.type,
          "EXPERIMENTAL"),
      helper_asName =
        list(strsplit(
          protocolSection.armsInterventionsModule.armGroups.label,
          " / ", fixed = TRUE)[[1]]),
      helper_prodRole =
        list(strsplit(
          protocolSection.armsInterventionsModule.armGroups.type,
          " / ", fixed = TRUE)[[1]]),
      helper_isTest =
        list(
          sapply(helper_prodRole, function(i) i == "EXPERIMENTAL", USE.NAMES = FALSE)
        ),
      helper_asTest =
        list(
          sapply(seq_along(helper_asName), function(i) {
            t <- helper_asName[i][helper_isTest[i]]
            c <- nchar(t)
            na.omit(t[c > 0L])
          }, USE.NAMES = FALSE)
        ),
      helper_simTest = list(
        asTestSimilarity(helper_asTest)
      ),
      analysis_numberDifferentTestArms = list(
        helper_multipleActiveArms -
          max(c(0L, sum(sapply(helper_simTest, function(i) i >= thresholdSimilar)) - 1L), na.rm = TRUE)
      ),
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
        grepl(
          titleDefPlatform,
          trialDescription.scientificTitle,
          ignore.case = TRUE
        ) | grepl(
          titleDefPlatform,
          trialDescription.title,
          ignore.case = TRUE
        ),
      analysis_isDrugTrial = grepl(
        "drug",
        interventions.intervention.interventionType,
        ignore.case = TRUE
      ),
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
        grepl(
          titleDefPlatform,
          authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle,
          ignore.case = TRUE
        ) | grepl(
          titleDefPlatform,
          authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle,
          ignore.case = TRUE
        ),
      helper_multipleActiveArms =
        stringi::stri_count_fixed(
          authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName,
          "Test"),
      helper_asName =
        list(strsplit(
          authorizedApplication.authorizedPartI.productRoleGroupInfos.products.productDictionaryInfo.activeSubstanceName,
          " / ", fixed = TRUE)[[1]]),
      helper_prodRole =
        list(strsplit(
          authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName,
          " / ", fixed = TRUE)[[1]]),
      helper_isTest =
        list(
          sapply(helper_prodRole, function(i) i == "Test", USE.NAMES = FALSE)
        ),
      helper_asTest =
        list(
          sapply(seq_along(helper_asName), function(i) {
            t <- helper_asName[i][helper_isTest[i]]
            c <- nchar(t)
            na.omit(t[c > 0L])
          }, USE.NAMES = FALSE)
        ),
      helper_simTest = list(
        asTestSimilarity(helper_asTest)
      ),
      analysis_numberDifferentTestArms = list(
        helper_multipleActiveArms -
          max(c(0L, sum(sapply(helper_simTest, function(i) i >= thresholdSimilar)) - 1L), na.rm = TRUE)
      ),
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
