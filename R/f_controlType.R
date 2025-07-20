#### history ####
# 2025-01-26 first version
# 2025-07-20 include "other"

#' Calculate type of control data collected in a study
#'
#' Trial concept calculated: type of internal control.
#' ICH E10 lists as types of control: placebo concurrent control, no-treatment
#' concurrent control, dose-response concurrent control, active (positive)
#' concurrent control, external (including historical) control, multiple control
#' groups. Dose-controlled trials are currently not identified.
#' External (including historical) controls are so far not identified in specific
#' register fields. Cross-over designs, where identifiable, have active controls.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.controlType`, which is
#' a factor with levels `none`, `no-treatment`, `placebo`, `active`,
#' `placebo+active` and `other`.
#'
#' @export
#'
#' @importFrom dplyr if_else mutate case_when `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.controlType()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   field = "ctrname",
#'   calculate = "f.controlType",
#'   con = dbc)
#' trialsDf
#'
f.controlType <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e81_controlled",
      "e816_cross_over",
      "e822_placebo",
      "e823_other", # other comparator
      "e8231_comparator_description",
      "e824_number_of_treatment_arms_in_the_trial",
      # if any results were loaded, also look at
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value"
    ),
    "ctgov" = c(
      "arm_group.arm_group_type"
    ),
    "ctgov2" = c(
      # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-ArmGroupType
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
      helper = .data$subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value,
      #
      out = dplyr::case_when(
        # EudraCT result related data Dictionary_V2_for publication (2)
        # Subject disposition, C33, picklist:
        # Experimental
        # active comparator
        # placebo
        # no intervention
        # other
        grepl("ARM_TYPE.placeboComp", .data$helper) &
          grepl("ARM_TYPE.activeComp", .data$helper) ~ "placebo+active",
        grepl("ARM_TYPE.placeboComp", .data$helper) ~ "placebo",
        grepl("ARM_TYPE.activeComp", .data$helper) ~ "active",
        grepl("ARM_TYPE.experimental", .data$helper) ~ "active",
        grepl("ARM_TYPE.noImp", .data$helper) ~ "no-treatment",
        grepl("ARM_TYPE.other", .data$helper) ~ "other",
        #
        # e81: If 'Yes' selected, E.8.1.1-E.8.1.7.1 applying to the design of the
        # trial should be completed. In a controlled trial, the tested product is
        # compared to a reference treatment. The reference treatment can be, for
        # example, a placebo, a product known to be effective, a surgical procedure,
        # or a different dose of the same product.
        !.data$e81_controlled ~ "none",
        .data$e822_placebo & .data$e823_other ~ "placebo+active",
        .data$e822_placebo & grepl("dos[ea]", .data$e8231_comparator_description, TRUE) ~ "placebo+active",
        .data$e822_placebo ~ "placebo",
        .data$e816_cross_over ~ "crossover",
        grepl("dos[ea]", .data$e8231_comparator_description, TRUE) ~ "active",
        grepl("no treat", .data$e8231_comparator_description, TRUE) ~ "no-treatment",
        .data$e823_other ~ "other",
        .data$e81_controlled ~ "other",
        .data$e824_number_of_treatment_arms_in_the_trial > 1L ~ "other",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        # Experimental | Active Comparator |Placebo Comparator | Sham
        # Comparator | No Intervention | Other
        grepl("Placebo Comparator", .data$arm_group.arm_group_type) &
          grepl("Active Comparator", .data$arm_group.arm_group_type) ~ "placebo+active",
        grepl("Placebo Comparator", .data$arm_group.arm_group_type) ~ "placebo",
        grepl("Active Comparator", .data$arm_group.arm_group_type) ~ "active",
        grepl("No Intervention", .data$arm_group.arm_group_type) ~ "no-treatment",
        !is.na(.data$arm_group.arm_group_type) ~ "none",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("PLACEBO_COMPARATOR", .data$protocolSection.armsInterventionsModule.armGroups.type) &
          grepl("ACTIVE_COMPARATOR", .data$protocolSection.armsInterventionsModule.armGroups.type) ~ "placebo+active",
        grepl("PLACEBO_COMPARATOR", .data$protocolSection.armsInterventionsModule.armGroups.type) ~ "placebo",
        grepl("ACTIVE_COMPARATOR", .data$protocolSection.armsInterventionsModule.armGroups.type) ~ "active",
        grepl("NO_INTERVENTION", .data$protocolSection.armsInterventionsModule.armGroups.type) ~ "no-treatment",
        !is.na(.data$protocolSection.armsInterventionsModule.armGroups.type) ~ "none",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("placebo.?control", .data$trialDesign.studyDesign, ignore.case = TRUE) &
          grepl("active.?control", .data$trialDesign.studyDesign, ignore.case = TRUE) ~ "placebo+active",
        grepl("placebo.?control", .data$trialDesign.studyDesign, ignore.case = TRUE) ~ "placebo",
        grepl("active.?control", .data$trialDesign.studyDesign, ignore.case = TRUE) ~ "active",
        grepl("[^no].?controlled", .data$trialDesign.studyDesign, ignore.case = TRUE)  ~ "other",
        .data$trialDesign.primaryStudyDesign == "Interventional" ~ "none",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        grepl("placebo", .data$authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) &
          grepl("comparator", .data$authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo+active",
        grepl("placebo", .data$authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) &
          grepl("comparator", .data$authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo+active",
        grepl("placebo", .data$authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo",
        grepl("placebo", .data$authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "placebo",
        grepl("comparator", .data$authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "active",
        grepl("comparator", .data$authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName, ignore.case = TRUE) ~ "active",
        !is.na(.data$authorizedPartI.productRoleGroupInfos.productRoleName) ~ "none",
        !is.na(.data$authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName) ~ "none",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctis


  # merge into vector (ordered factor)
  df[[".controlType"]] <- factor(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)[names(fldsNeeded) != ""]
    ), levels = c(
      "none", "no-treatment", "placebo",
      "active", "placebo+active", "other")
  )

  # keep only outcome columns
  df <- df[, c("_id", ".controlType"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".controlType"]], "factor"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .controlType
