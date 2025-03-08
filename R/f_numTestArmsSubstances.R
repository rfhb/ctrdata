#### history ####
# 2025-01-27 first partly working version
# 2025-01-31 working
# 2025-02-08 adding euctr protocol, isrctn

#' Calculate type of control data collected in a study
#'
#' Trial concept calculated: number of active arms with different
#' investigational medicines, after excluding comparator, auxiliary and placebo
#' arms / medicines.
#' For ISRCTN, this is imprecise because arms are not identified in a field.
#' Most registers provide no or only limited information on phase 1 trials,
#' so that this number typically cannot be calculated for these trials.
#' Requires packages stringdist to be installed; stringdist is used for
#' evaluating names of active substances, which are considered similar when
#' the similarity is 0.8 or higher.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.numTestArmsSubstances`, an integer
#'
#' @export
#'
#' @importFrom dplyr mutate case_when if_else pull `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_split_fixed
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.controlType()
#'
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.numTestArmsSubstances",
#'   con = dbc)
#' }
#'
f.numTestArmsSubstances <- function(df = NULL) {

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

    # generic, do not edit
    return(fldsNeeded)

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
    x <- tolower(x)

    # early exit if single arm
    if (length(x) == 1L) return(0L)

    # calculate similarities
    t <- stringdist::stringsimmatrix(x, x)

    # early exit
    if (ncol(t) < 1L) return(NA_integer_)

    # extract max similarity of arm n with other arm(s)
    diag(t) <- NA_integer_
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
      .data$e81_controlled,
      .data$e824_number_of_treatment_arms_in_the_trial - 1,
      .data$e824_number_of_treatment_arms_in_the_trial,
    ),
    # results data
    helper_asNamesPerTestGroup = lapply(
      .data$subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm,
      function(i) {if (any(names(i) == "title"))
        as.list(i)[["title"]][
          as.list(i)[["type"]][["value"]] == "ARM_TYPE.experimental"] else NA}),
    #
    helper_simTestGroupsInTrial = lapply(
      .data$helper_asNamesPerTestGroup, function(i) asTestSimilarityTrial(i)),
    #
    helper_numTestGroupsInTrial = sapply(
      .data$helper_simTestGroupsInTrial, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentTestGroupsInTrial = dplyr::case_when(
      is.na(.data$helper_asNamesPerTestGroup) ~ NA_integer_,
      .data$helper_numTestGroupsInTrial == 1L ~ 1L,
      .default = sapply(.data$helper_simTestGroupsInTrial, function(i)
        max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    out = if_else(
      !is.na(.data$analysis_numDifferentTestGroupsInTrial),
      .data$analysis_numDifferentTestGroupsInTrial,
      .data$helper_protocol
    )
  ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerExpArm = lapply(
      .data$arm_group,
      function(i) as.list(i)[["arm_group_label"]][
        as.list(i)[["arm_group_type"]] == "Experimental"]),
    #
    helper_simExpArmsInTrial = lapply(
      .data$helper_asNamesPerExpArm, function(i) asTestSimilarityTrial(i)),
    #
    helper_numExpArmsInTrial = sapply(
      .data$helper_simExpArmsInTrial, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentExpArmsInTrial = dplyr::case_when(
      is.na(.data$helper_asNamesPerExpArm) ~ NA_integer_,
      .data$helper_numExpArmsInTrial == 1L ~ 1L,
      .data$helper_numExpArmsInTrial > 1L ~
        sapply(.data$helper_simExpArmsInTrial, function(i)
          max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE
        )
    ),
    # if at least one test arm im trial
    out = .data$analysis_numDifferentExpArmsInTrial
  ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerExpArm = lapply(
      .data$protocolSection.armsInterventionsModule.armGroups,
      function(i) i["label"][i["type"] == "EXPERIMENTAL"]),
    #
    helper_simExpArmsInTrial = lapply(
      .data$helper_asNamesPerExpArm, function(i) asTestSimilarityTrial(i)),
    #
    helper_numExpArmsInTrial = sapply(
      .data$helper_simExpArmsInTrial, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentExpArmsInTrial = dplyr::case_when(
      is.na(.data$helper_asNamesPerExpArm) ~ NA_integer_,
      .data$helper_numExpArmsInTrial == 1L ~ 1L,
      .data$helper_numExpArmsInTrial > 1L ~ sapply(
        .data$helper_simExpArmsInTrial, function(i)
          max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    #
    out = .data$analysis_numDifferentExpArmsInTrial
  ) %>%
    dplyr::pull("out") -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      #
      helper_drugNames = stringi::stri_split_fixed(
        .data$interventions.intervention.drugNames, ", "),
      #
      helper_numberDrugs = sapply(.data$helper_drugNames, function(i)
        length(i), USE.NAMES = FALSE, simplify = TRUE),
      #
      helper_simDrugNames = lapply(
        .data$helper_drugNames, function(i) asTestSimilarityTrial(i)),
      #
      analysis_numDifferentExpArmsInTrial = dplyr::case_when(
        is.na(.data$helper_numberDrugs) ~ NA_integer_,
        .data$helper_numberDrugs == 1L ~ 1L,
        .data$helper_numberDrugs > 1L ~ sapply(
          .data$helper_simDrugNames, function(i)
            sum(i < thresholdSimilar),
          simplify = TRUE
        )
      ),
      #
      helper_numberarms = if_else(
        grepl("controlled|parallel.?group", .data$trialDesign.secondaryStudyDesign, ignore.case = TRUE),
        .data$analysis_numDifferentExpArmsInTrial, 1L),
      #
      out = if_else(
        grepl("drug|biological|vaccine", .data$interventions.intervention.interventionType, ignore.case = TRUE),
        .data$helper_numberarms, NA_integer_
      )
    ) %>%
    dplyr::pull("out")-> df$isrctn


  #### . CTIS ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerTestGroupCtis1 = lapply(
      .data$authorizedPartI.productRoleGroupInfos,
      function(i) {if (any(names(i) == "products"))
        lapply(i[["products"]], "[[", "productName")[
          i[["productRoleName"]] == "Test"] else NA}),
    #
    helper_simTestGroupsInTrialCtis1 = lapply(
      .data$helper_asNamesPerTestGroupCtis1, function(i) asTestSimilarityTrial(i)),
    #
    helper_numTestGroupsInTrialCtis1 = sapply(
      .data$helper_simTestGroupsInTrialCtis1, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentTestGroupsInTrialCtis1 = dplyr::case_when(
      is.na(.data$helper_asNamesPerTestGroupCtis1) ~ NA_integer_,
      .data$helper_numTestGroupsInTrialCtis1 == 1L ~ 1L,
      .default = sapply(.data$helper_simTestGroupsInTrialCtis1, function(i)
        max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    #
    #
    helper_asNamesPerTestGroupCtis2 = lapply(
      .data$authorizedApplication.authorizedPartI.productRoleGroupInfos,
      function(i) {if (any(names(i) == "products"))
        lapply(i[["products"]], "[[", "productName")[
          i[["productRoleName"]] == "Test"] else NA_integer_}),
    #
    helper_simTestGroupsInTrialCtis2 = lapply(
      .data$helper_asNamesPerTestGroupCtis2, function(i) asTestSimilarityTrial(i)),
    #
    helper_numTestGroupsInTrialCtis2 = sapply(
      .data$helper_simTestGroupsInTrialCtis2, length, USE.NAMES = FALSE, simplify = TRUE),
    #
    analysis_numDifferentTestGroupsInTrialCtis2 = dplyr::case_when(
      is.na(.data$helper_asNamesPerTestGroupCtis2) ~ NA_integer_,
      .data$helper_numTestGroupsInTrialCtis2 == 1L ~ 1L,
      .default = sapply(.data$helper_simTestGroupsInTrialCtis2, function(i)
        max(sum(i < thresholdSimilar), 1L, na.rm = TRUE), simplify = TRUE)
    ),
    #
    #
    out = dplyr::case_when(
      !is.na(.data$analysis_numDifferentTestGroupsInTrialCtis1) ~
        .data$analysis_numDifferentTestGroupsInTrialCtis1,
      !is.na(.data$analysis_numDifferentTestGroupsInTrialCtis2) ~
        .data$analysis_numDifferentTestGroupsInTrialCtis2,
      .default = NA_integer_
    )
  ) %>%
    dplyr::pull("out") -> df$ctis


  # merge into vector (ordered factor)
  df[[".numTestArmsSubstances"]] <- as.integer(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)
    )
  )

  # keep only outcome columns
  df <- df[, c("_id", ".numTestArmsSubstances"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".numTestArmsSubstances"]], "integer"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.numTestArmsSubstances
