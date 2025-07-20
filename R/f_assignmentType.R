#### history ####
# 2025-07-12 first version

#' Calculate type of assignment to intervention in a study
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.assignmentType`, which is
#' a factor with levels `R` (randomised assignment) and `NR` (all other types
#' of assignment).
#'
#' @export
#'
#' @importFrom dplyr if_else mutate case_when `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.assignmentType()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   field = "ctrname",
#'   calculate = "f.assignmentType",
#'   con = dbc)
#' trialsDf
#'
f.assignmentType <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e811_randomised",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.allocation.value"
    ),
    "ctgov" = c(
      "study_design_info.allocation"
    ),
    "ctgov2" = c(
      "protocolSection.designModule.designInfo.allocation"
    ),
    "isrctn" = c(
      "trialDesign.studyDesign"
    ),
    "ctis" = c(
      # CTIS1
      "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod",
      # CTIS2
      "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod"

    ),
    "ctrname")


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
      out = dplyr::case_when(
        # EudraCT protocol related data dictionary.xls
        # If each subject in the trial is randomly assigned to
        # receive either the study treatment or a placebo
        .data$e811_randomised ~ "R",
        !.data$e811_randomised ~ "NR",
        # EudraCT Results - XML Schema Guidance.pdf
        .data$subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.allocation.value == "ALLOCATION.randControlled" ~ "R",
        .data$subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.allocation.value != "ALLOCATION.nonRandControlled" ~ "NR",
        #
        .data$ctrname == "EUCTR" ~ "NR",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        .data$study_design_info.allocation == "Randomized" ~ "R",
        .data$study_design_info.allocation != "Randomized" ~ "NR",
        .data$ctrname == "CTGOV" ~ "NR",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        .data$protocolSection.designModule.designInfo.allocation == "RANDOMIZED" ~ "R",
        .data$protocolSection.designModule.designInfo.allocation != "RANDOMIZED" ~ "NR",
        .data$ctrname == "CTGOV2" ~ "NR",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        .data$ctrname != "ISRCTN" ~ NA_character_,
        grepl("(^| )random", .data$trialDesign.studyDesign, ignore.case = TRUE) &
          !grepl("no[nt][ -]*random", .data$trialDesign.studyDesign, ignore.case = TRUE) ~ "R",
        TRUE ~ "NR"
      )
    ) %>%
    dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  # CT03.03.02.02.04	Field: Allocation method
  # Lookup: Clinical trial allocation mehtod
  # Method of assigning subjects to treatment group in this period.
  # Reference list number 57.
  # CODE	NAME
  # 1	Randomised Controlled
  # 2	Non-Randomised Controlled
  # 3	Not Applicable
  df %>%
    dplyr::mutate(
      out = dplyr::case_when(
        .data$authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod == 1 ~ "R",
        .data$authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod == 2 ~ "NR",
        .data$authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod == 1 ~ "R",
        .data$authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.allocationMethod == 2 ~ "NR",
        .data$ctrname == "CTIS" ~ "NR",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctis


  # merge into vector (ordered factor)
  df[[".assignmentType"]] <- factor(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)[names(fldsNeeded) != ""]
    ), levels = c(
      "R", "NR")
  )

  # keep only outcome columns
  df <- df[, c("_id", ".assignmentType"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".assignmentType"]], "factor"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .assignmentType
