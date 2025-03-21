#### history ####
# 2025-02-08 first version

#' Calculate sample size of a study
#'
#' Trial concept calculated: sample size of the trial, preferring
#' results-related over protocol-related information.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.sampleSize`, an integer.
#'
#' @export
#'
#' @importFrom dplyr if_else mutate pull select `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.sampleSize()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.sampleSize",
#'   con = dbc)
#' trialsDf
#'
f.sampleSize <- function(df = NULL) {

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

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### . EUCTR ####
  fldsEuctrProtocol <- fldsNeeded$euctr[-1]
  dplyr::mutate(
    df, out = rowSums(
      dplyr::select(
        df, fldsEuctrProtocol), na.rm = TRUE)) %>%
    dplyr::pull("out") -> df$helper_euctr_protocol
  df %>%
    mutate(
      helper_euctr_results = sapply(
        .data$trialInformation.countrySubjectCounts.countrySubjectCount.subjects,
        function(i) sum(i, na.rm = TRUE),
        USE.NAMES = FALSE, simplify = TRUE),
      out = dplyr::if_else(
        .data$helper_euctr_results > 0L,
        .data$helper_euctr_results,
        .data$helper_euctr_protocol
      )
    ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####

  # directly using field, see above


  #### . CTGOV2 ####

  # directly using field, see above

  # TODO alternative, using results data
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
        !is.na(.data$participants.totalFinalEnrolment),
        .data$participants.totalFinalEnrolment,
        .data$participants.targetEnrolment
      )
    ) %>%
    dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  df %>%
    mutate(
      helper_ctis1 = sapply(
        .data$authorizedPartsII.recruitmentSubjectCount,
        function(i) sum(i, na.rm = TRUE),
        USE.NAMES = FALSE, simplify = TRUE),
      helper_ctis2 = sapply(
        .data$authorizedApplication.authorizedPartsII.recruitmentSubjectCount,
        function(i) sum(i, na.rm = TRUE),
        USE.NAMES = FALSE, simplify = TRUE)
    ) -> df
  dplyr::mutate(
    df, out = rowSums(
      dplyr::select(
        df, c(
          "helper_ctis1", "authorizedPartI.rowSubjectCount",
          "helper_ctis2", "authorizedApplication.authorizedPartI.rowSubjectCount"
        )), na.rm = TRUE)) %>%
    dplyr::pull("out") -> df$ctis

  # merge all
  dplyr::mutate(
    df,
    out = rowSums(
      dplyr::select(
        df,c(
          "euctr",
          "enrollment",
          "protocolSection.designModule.enrollmentInfo.count",
          "isrctn",
          "ctis"
        )), na.rm = TRUE),
    out = if_else(.data$out > 0L, .data$out, NA_integer_),
    out = as.integer(.data$out)
  ) %>%
    dplyr::pull("out") -> df[[".sampleSize"]]

  # keep only outcome columns
  df <- df[, c("_id", ".sampleSize"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".sampleSize"]], "integer"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.sampleSize
