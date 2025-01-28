# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when rowwise ungroup `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed
.numTestArmsSubstances <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.title",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.arms.arm.type.value"
    ),
    "ctgov" = c(
      "arm_group.arm_group_label",
      "arm_group.arm_group_type"
    ),
    "ctgov2" = c(
      "protocolSection.armsInterventionsModule.armGroups.label",
      "protocolSection.armsInterventionsModule.armGroups.type"
    ),
    "isrctn" = c(
      "trialDescription.title"
    ),
    "ctis" = c(
      "authorizedApplication.authorizedPartI.productRoleGroupInfos.productRoleName",
      "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.productDictionaryInfo.activeSubstanceName"
    ))


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the number of active arms with different investigational medicines,
after excluding comparator, auxiliary and placebo medicines. (No data found
for ISRCTN to calculate.)

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
      out = analysis_numberDifferentTestArms
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$euctr


  #### ..CTGOV ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
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
      out = analysis_numberDifferentTestArms
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctgov


  #### ..CTGOV2 ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
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
      out = analysis_numberDifferentTestArms
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctgov2


  #### ..ISRCTN ####
  df$isrctn <- NA


  #### ..CTIS ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
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
      out = analysis_numberDifferentTestArms
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
  stopifnot(is.integer(vct))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .numTestArmsSubstances
