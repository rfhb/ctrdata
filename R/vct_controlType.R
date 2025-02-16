# function definition for dfCalculate

#### history ####
# 2025-01-26 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when `%>%`
.controlType <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e81_controlled",
      "e816_cross_over",
      "e822_placebo",
      "e823_other", # other comparator
      "e824_number_of_treatment_arms_in_the_trial"
    ),
    "ctgov" = c(
      "arm_group.arm_group_type"
    ),
    "ctgov2" = c(
      "protocolSection.designModule.designInfo.interventionModel",
      "protocolSection.armsInterventionsModule.armGroups.type"
    ),
    "isrctn" = c(
      "trialDesign.studyDesign",
      "trialDesign.primaryStudyDesign"
    ),
    "ctis" = c(
      # CTIS1
      "authorizedPartI.productRoleGroupInfos.productRoleName",
      # CTIS2
      "authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName"
    ))

  # not relevant after inspection:
  #


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the type of control(s) data collected within a clinical trial.
ICH E10 lists as types of control: placebo concurrent control, no-treatment
concurrent control, dose-response concurrent control, active (positive)
concurrent control, external (including historical) control, multiple control
groups.

Dose-controlled trials are currently not identified.
External (including historical) controls are so far not identified in specific
register fields. Cross-over designs, where identifiable, have active controls.

Returns a factor with levels "none", "no-treatment", "placebo", "active",
"placebo+active" and "other".
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


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        !e81_controlled ~ "none",
        e822_placebo & (e823_other != "No") ~ "placebo+active",
        e822_placebo ~ "placebo",
        e816_cross_over ~ "crossover",
        e823_other != "No" ~ "active",
        e823_other == "No" ~ "no-treatment",
        e824_number_of_treatment_arms_in_the_trial > 1L ~ "other",
        .default = NA
      )
    ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("Placebo Comparator", arm_group.arm_group_type) &
          grepl("Active Comparator", arm_group.arm_group_type) ~ "placebo+active",
        grepl("Placebo Comparator", arm_group.arm_group_type) ~ "placebo",
        grepl("Active Comparator", arm_group.arm_group_type) ~ "active",
        grepl("No Intervention", arm_group.arm_group_type) ~ "no-treatment",
        !is.na(arm_group.arm_group_type) ~ "none",
        .default = NA
      )
    ) %>%
    dplyr::pull(out) -> df$ctgov


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("PLACEBO_COMPARATOR", protocolSection.armsInterventionsModule.armGroups.type) &
          grepl("ACTIVE_COMPARATOR", protocolSection.armsInterventionsModule.armGroups.type) ~ "placebo+active",
        grepl("PLACEBO_COMPARATOR", protocolSection.armsInterventionsModule.armGroups.type) ~ "placebo",
        grepl("ACTIVE_COMPARATOR", protocolSection.armsInterventionsModule.armGroups.type) ~ "active",
        grepl("NO_INTERVENTION", protocolSection.armsInterventionsModule.armGroups.type) ~ "no-treatment",
        !is.na(protocolSection.armsInterventionsModule.armGroups.type) ~ "none",
        .default = NA
      )
    ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("placebo.?control", trialDesign.studyDesign, ignore.case = TRUE) &
          grepl("active.?control", trialDesign.studyDesign, ignore.case = TRUE) ~ "placebo+active",
        grepl("placebo.?control", trialDesign.studyDesign, ignore.case = TRUE) ~ "placebo",
        grepl("active.?control", trialDesign.studyDesign, ignore.case = TRUE) ~ "active",
        grepl("[^no].?controlled", trialDesign.studyDesign, ignore.case = TRUE)  ~ "other",
        trialDesign.primaryStudyDesign == "Interventional" ~ "none",
        .default = NA
      )
    ) %>%
    dplyr::pull(out) -> df$isrctn


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("placebo", authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) &
          grepl("comparator", authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo+active",
        grepl("placebo", authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) &
          grepl("comparator", authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo+active",
        grepl("placebo", authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo",
        grepl("placebo", authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo",
        grepl("comparator", authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "active",
        grepl("comparator", authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "active",
        !is.na(authorizedPartI.productRoleGroupInfos.productRoleName) ~ "none",
        !is.na(authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName) ~ "none",
        .default = NA
      )
    ) %>%
    dplyr::pull(out) -> df$ctis


  # merge into vector (ordered factor)
  df[[".controlType"]] <- factor(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)
    ), levels = c(
      "none", "no-treatment", "placebo",
      "active", "placebo+active", "other")
  )

  # keep only outcome columns
  df <- df[, c("_id", ".controlType"), drop = FALSE]


  #### checks ####
  # stopifnot(inherits(vct, "factor"))
  # stopifnot(length(vct) == nrow(df))
  # TODO
  stopifnot(inherits(df[[".controlType"]], "factor"))

  # return
  return(df)

} # end .controlType
