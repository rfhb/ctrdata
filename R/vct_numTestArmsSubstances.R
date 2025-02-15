# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version
# 2025-01-31 working
# 2025-02-08 adding euctr protocol, isrctn


#' @noRd
#' @export
#' @importFrom dplyr mutate case_when if_else pull `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_split_fixed
.numTestArmsSubstances <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e824_number_of_treatment_arms_in_the_trial",
      "e81_controlled",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm"
    ),
    "ctgov" = c(
      "arm_group"
    ),
    "ctgov2" = c(
      "protocolSection.armsInterventionsModule.armGroups"
    ),
    "isrctn" = c(
      "trialDesign.secondaryStudyDesign",
      "interventions.intervention.interventionType",
      "interventions.intervention.drugNames"
    ),
    "ctis" = c(
      # CTIS1
      "authorizedPartI.productRoleGroupInfos",
      # CTIS2
      "authorizedApplication.authorizedPartI.productRoleGroupInfos"
    ))


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the number of active arms with different investigational medicines,
after excluding comparator, auxiliary and placebo arms / medicines.
For ISRCTN, this is imprecise because arms are not identified in a field.
Most registers provide no or only limited information on phase 1 trials,
so that this number typically cannot be calculated for these trials.

Requires packages stringdist to be installed; stringdist is used for
evaluating names of active substances, which are considered similar when the
similarity is 0.8 or higher.

Returns an integer.
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

  # helper function
  `%>%` <- dplyr::`%>%`

  # helper function, to be call per group
  asTestSimilarityArms <- function(x) {

    # early exit
    if (is.null(x) || !length(x) || all(is.na(x))) return(NULL)

    # normalise
    x <- tolower(x) # TODO removed unlist(x)

    # early exit if single arm
    if (length(x) == 1L) return(0L)

    # calculate similarities
    t <- stringdist::stringsimmatrix(x, x)

    # early exit
    if (ncol(t) < 1L) return(NA)

    # extract max similarity of arm n with other arm(s)
    diag(t) <- NA
    if (all(is.na(t))) return(0L)
    apply(t, 2, max, na.rm = TRUE)

  } # asTestSimilarityArms

  # helper function, to be called per trial
  asTestSimilarityTrial <- function(x) {

    # collapse active substances into one string per treatment arm / group
    t <- sapply(x, function(i) paste0(unique(i), collapse = " / "))

    # return vector of similarities of treatment arms
    asTestSimilarityArms(t)

  } # end asTestSimilarityTrial


  #### . EUCTR ####
  df %>% dplyr::mutate(
    #
    # protocol data
    helper_protocol = dplyr::if_else(
      e81_controlled,
      e824_number_of_treatment_arms_in_the_trial - 1,
      e824_number_of_treatment_arms_in_the_trial,
    ),
    # results data
    helper_asNamesPerTestGroup = lapply(
      subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm,
      function(i) {if (any(names(i) == "title"))
        as.list(i)[["title"]][
          as.list(i)[["type"]][["value"]] == "ARM_TYPE.experimental"] else NA}),
    #
    helper_simTestGroupsInTrial = lapply(
      helper_asNamesPerTestGroup, function(i) asTestSimilarityTrial(i)),
    #
    helper_numTestGroupsInTrial = sapply(
      helper_simTestGroupsInTrial, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentTestGroupsInTrial = dplyr::case_when(
      is.na(helper_asNamesPerTestGroup) ~ NA,
      helper_numTestGroupsInTrial == 1L ~ 1L,
      .default = sapply(helper_simTestGroupsInTrial, function(i)
        max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    out = if_else(
      !is.na(analysis_numDifferentTestGroupsInTrial),
      analysis_numDifferentTestGroupsInTrial,
      helper_protocol
    )
  ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerExpArm = lapply(
      arm_group,
      function(i) as.list(i)[["arm_group_label"]][
        as.list(i)[["arm_group_type"]] == "Experimental"]),
    #
    helper_simExpArmsInTrial = lapply(
      helper_asNamesPerExpArm, function(i) asTestSimilarityTrial(i)),
    #
    helper_numExpArmsInTrial = sapply(
      helper_simExpArmsInTrial, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentExpArmsInTrial = dplyr::case_when(
      is.na(helper_asNamesPerExpArm) ~ NA,
      helper_numExpArmsInTrial == 1L ~ 1L,
      helper_numExpArmsInTrial > 1L ~
        sapply(helper_simExpArmsInTrial, function(i)
          max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE
        )
    ),
    # if at least one test arm im trial
    out = analysis_numDifferentExpArmsInTrial
  ) %>%
    dplyr::pull(out) -> df$ctgov


  #### . CTGOV2 ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerExpArm = lapply(
      protocolSection.armsInterventionsModule.armGroups,
      function(i) i["label"][i["type"] == "EXPERIMENTAL"]),
    #
    helper_simExpArmsInTrial = lapply(
      helper_asNamesPerExpArm, function(i) asTestSimilarityTrial(i)),
    #
    helper_numExpArmsInTrial = sapply(
      helper_simExpArmsInTrial, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentExpArmsInTrial = dplyr::case_when(
      is.na(helper_asNamesPerExpArm) ~ NA,
      helper_numExpArmsInTrial == 1L ~ 1L,
      helper_numExpArmsInTrial > 1L ~ sapply(
        helper_simExpArmsInTrial, function(i)
          max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    #
    out = analysis_numDifferentExpArmsInTrial
  ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      #
      helper_drugNames = stringi::stri_split_fixed(
        interventions.intervention.drugNames, ", "),
      #
      helper_numberDrugs = sapply(helper_drugNames, function(i)
        length(i), USE.NAMES = FALSE, simplify = TRUE),
      #
      helper_simDrugNames = lapply(
        helper_drugNames, function(i) asTestSimilarityTrial(i)),
      #
      analysis_numDifferentExpArmsInTrial = dplyr::case_when(
        is.na(helper_numberDrugs) ~ NA,
        helper_numberDrugs == 1L ~ 1L,
        helper_numberDrugs > 1L ~ sapply(
          helper_simDrugNames, function(i)
            sum(i < thresholdSimilar),
          simplify = TRUE
        )
      ),
      #
      helper_numberarms = if_else(
        grepl("controlled|parallel.?group", trialDesign.secondaryStudyDesign, ignore.case = TRUE),
        analysis_numDifferentExpArmsInTrial, 1L),
      #
      out = if_else(
        grepl("drug|biological|vaccine", interventions.intervention.interventionType, ignore.case = TRUE),
        helper_numberarms, NA
      )
    ) %>%
    dplyr::pull(out)-> df$isrctn


  #### . CTIS ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerTestGroupCtis1 = lapply(
      authorizedPartI.productRoleGroupInfos,
      function(i) {if (any(names(i) == "products"))
        lapply(i[["products"]], "[[", "productName")[
          i[["productRoleName"]] == "Test"] else NA}),
    #
    helper_simTestGroupsInTrialCtis1 = lapply(
      helper_asNamesPerTestGroupCtis1, function(i) asTestSimilarityTrial(i)),
    #
    helper_numTestGroupsInTrialCtis1 = sapply(
      helper_simTestGroupsInTrialCtis1, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentTestGroupsInTrialCtis1 = dplyr::case_when(
      is.na(helper_asNamesPerTestGroupCtis1) ~ NA,
      helper_numTestGroupsInTrialCtis1 == 1L ~ 1L,
      .default = sapply(helper_simTestGroupsInTrialCtis1, function(i)
        max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    #
    #
    helper_asNamesPerTestGroupCtis2 = lapply(
      authorizedApplication.authorizedPartI.productRoleGroupInfos,
      function(i) {if (any(names(i) == "products"))
        lapply(i[["products"]], "[[", "productName")[
          i[["productRoleName"]] == "Test"] else NA}),
    #
    helper_simTestGroupsInTrialCtis2 = lapply(
      helper_asNamesPerTestGroupCtis2, function(i) asTestSimilarityTrial(i)),
    #
    helper_numTestGroupsInTrialCtis2 = sapply(
      helper_simTestGroupsInTrialCtis2, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentTestGroupsInTrialCtis2 = dplyr::case_when(
      is.na(helper_asNamesPerTestGroupCtis2) ~ NA,
      helper_numTestGroupsInTrialCtis2 == 1L ~ 1L,
      .default = sapply(helper_simTestGroupsInTrialCtis2, function(i)
        max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    #
    #
    out = dplyr::case_when(
      !is.na(analysis_numDifferentTestGroupsInTrialCtis1) ~
        analysis_numDifferentTestGroupsInTrialCtis1,
      !is.na(analysis_numDifferentTestGroupsInTrialCtis2) ~
        analysis_numDifferentTestGroupsInTrialCtis2,
      .default = NA
    )
  ) %>%
  dplyr::pull(out) -> df$ctis


  # merge into vector (ordered factor)
  vct <- as.integer(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)
    )
  )


  #### checks ####
  stopifnot(is.integer(vct) || all(is.na(vct)))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .numTestArmsSubstances
