# function definition for dfCalculate

#### history ####
# 2025-02-08 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate pull select `%>%`
.sampleSize <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      # results
      "trialInformation.countrySubjectCounts.countrySubjectCount.subjects",
      # protocol
      "f11_number_of_subjects_for_this_age_range",   # under 18 years of age
      "f1111_number_of_subjects_for_this_age_range", # in utero
      "f1121_number_of_subjects_for_this_age_range", # preterm
      "f1131_number_of_subjects_for_this_age_range", # newborns
      "f1141_number_of_subjects_for_this_age_range", # infants and toddlers
      "f1151_number_of_subjects_for_this_age_range", # children
      "f1161_number_of_subjects_for_this_age_range", # adolescents
      "f121_number_of_subjects_for_this_age_range",  # 18 to 64 years of age
      "f131_number_of_subjects_for_this_age_range"   # elderly 65 years of age and older
    ),
    "ctgov" = c(
      "enrollment"
    ),
    "ctgov2" = c(
      "protocolSection.designModule.enrollmentInfo.count"
    ),
    "isrctn" = c(
      "participants.targetEnrolment",
      "participants.totalFinalEnrolment"
    ),
    "ctis" = c(
      # CTIS1
      "authorizedPartsII.recruitmentSubjectCount", # in EEA
      "authorizedPartI.rowSubjectCount", # ROW, outside EEA
      # CTIS2
      "authorizedApplication.authorizedPartsII.recruitmentSubjectCount", # in EEA
      "authorizedApplication.authorizedPartI.rowSubjectCount" # ROW, outside EEA
    )
  )

  # not relevant after inspection:
  #


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the sample size of the trial, preferring results-related over
protocol-related information.

Returns an integer.
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
  fldsEuctrProtocol <- fldsNeeded$euctr[-1]
  df %>%
    dplyr::select(dplyr::all_of(fldsEuctrProtocol)) %>%
    dplyr::mutate(out = rowSums(., na.rm = TRUE)) %>%
    dplyr::pull(out) -> df$helper_euctr_protocol
  df %>%
    mutate(
      helper_euctr_results = sapply(
        trialInformation.countrySubjectCounts.countrySubjectCount.subjects,
        function(i) sum(i, na.rm = TRUE),
        USE.NAMES = FALSE, simplify = TRUE),
      out = dplyr::if_else(
        helper_euctr_results > 0L,
        helper_euctr_results,
        helper_euctr_protocol
      )
    ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####

  # directly using field, see above


  #### . CTGOV2 ####

  # directly using field, see above

  # TODO for results data
  # num_participants = sum(as.integer(
  #   resultsSection.baselineCharacteristicsModule.denoms.counts.value[which_not_total]))
  # num_arms_or_groups = max(number_of_arms, length(which_not_total))
  # number_of_arms = stringi::stri_count_fixed(
  # resultsSection.baselineCharacteristicsModule.groups.title, " / "),
  # num_participants = sum(as.integer(
  # resultsSection.baselineCharacteristicsModule.denoms.counts.value[which_not_total])),
  #   number_of_arms = stringi::stri_count_fixed(
  #     resultsSection.baselineCharacteristicsModule.groups.title, " / "),
  #   which_not_total = list(which(strsplit(
  #     resultsSection.baselineCharacteristicsModule.groups.title, " / ")[[1]] != "Total")),
  #   num_sites = length(strsplit(protocolSection.contactsLocationsModule.locations.city, " / ")[[1]]),
  #   num_participants = sum(as.integer(
  #     resultsSection.baselineCharacteristicsModule.denoms.counts.value[which_not_total])),
  #   num_arms_or_groups = max(number_of_arms, length(which_not_total))
  # )
  # which_not_total = list(which(strsplit(
  #   resultsSection.baselineCharacteristicsModule.groups.title, " / ")[[1]] != "Total")),


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      out = if_else(
        !is.na(participants.totalFinalEnrolment),
        participants.totalFinalEnrolment,
        participants.targetEnrolment
      )
    ) %>%
    dplyr::pull(out) -> df$isrctn


  #### . CTIS ####
  df %>%
    mutate(
      helper_ctis1 = sapply(
        authorizedPartsII.recruitmentSubjectCount,
        function(i) sum(i, na.rm = TRUE),
        USE.NAMES = FALSE, simplify = TRUE),
      helper_ctis2 = sapply(
        authorizedApplication.authorizedPartsII.recruitmentSubjectCount,
        function(i) sum(i, na.rm = TRUE),
        USE.NAMES = FALSE, simplify = TRUE)
    ) %>%
    dplyr::select(c(
      helper_ctis1, authorizedPartI.rowSubjectCount,
      helper_ctis2, authorizedApplication.authorizedPartI.rowSubjectCount
    )) %>%
    dplyr::mutate(out = rowSums(., na.rm = TRUE)) %>%
    dplyr::pull(out) -> df$ctis


  # merge all
  df %>%
    dplyr::select(
      euctr,
      enrollment,
      protocolSection.designModule.enrollmentInfo.count,
      isrctn,
      ctis
    ) %>%
    dplyr::mutate(
      out = rowSums(., na.rm = TRUE),
      out = if_else(out > 0L, out, NA_integer_),
      out = as.integer(out)
    ) %>%
    dplyr::pull(out) -> df[[".sampleSize"]]

  # keep only outcome columns
  df <- df[, c("_id", ".sampleSize"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".sampleSize"]], "integer"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .sampleSize
