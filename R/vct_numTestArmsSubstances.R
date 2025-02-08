# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version
# 2025-01-31 working


#' @noRd
#' @export
#' @importFrom dplyr mutate case_when pull `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_split_fixed
.numTestArmsSubstances <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm"
    ),
    "ctgov" = c(
      "arm_group"
    ),
    "ctgov2" = c(
      "protocolSection.armsInterventionsModule.armGroups"
    ),
    "isrctn" = c(
      "trialDescription.title"
    ),
    "ctis" = c(
      "authorizedApplication.authorizedPartI.productRoleGroupInfos"
    ))


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the number of active arms with different investigational medicines,
after excluding comparator, auxiliary and placebo medicines. (No data found
for ISRCTN to calculate.)

For EUCTR, requires that results have been included in the collection, using
ctrLoadQueryIntoDb(queryterm = ..., euctrresults = TRUE, con = ...).

Requires packages dplyr and stringdist to be installed; stringdist is used for
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
        sum(i < thresholdSimilar), simplify = TRUE)
    ),
    #
    out = analysis_numDifferentTestGroupsInTrial
  ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    # helper_multipleActiveArms = stringi::stri_count_fixed(
    #   arm_group.arm_group_type, "Experimental"),
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
        max(as.integer(helper_numExpArmsInTrial > 0L),
            sapply(helper_simExpArmsInTrial, function(i)
              sum(i < thresholdSimilar), simplify = TRUE)
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
      helper_numExpArmsInTrial > 1L ~ sapply(helper_simExpArmsInTrial, function(i)
        sum(i < thresholdSimilar), simplify = TRUE)
    ),
    #
    out = analysis_numDifferentExpArmsInTrial
  ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### . ISRCTN ####
  df$isrctn <- NA


  #### . CTIS ####
  df %>% dplyr::mutate(
    #
    helper_asNamesPerTestGroup = lapply(
      authorizedApplication.authorizedPartI.productRoleGroupInfos,
      function(i) {if (any(names(i) == "products"))
        lapply(i[["products"]], "[[", "productName")[
          i[["productRoleName"]] == "Test"] else NA}),
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
        sum(i < thresholdSimilar), simplify = TRUE)
    ),
    #
    out = analysis_numDifferentTestGroupsInTrial
  ) %>%
    dplyr::pull(out) -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  )


  #### checks ####
  stopifnot(is.integer(vct) || all(is.na(vct)))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .numTestArmsSubstances
