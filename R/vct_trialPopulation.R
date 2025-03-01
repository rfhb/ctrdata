# function definition for dfCalculate

#### history ####
# 2025-01-26 first version

# @export

#' @noRd
#' @importFrom dplyr mutate select case_when if_else `%>%`
#' @importFrom stringi stri_replace_all_regex stri_split_fixed
#' @importFrom lubridate as.period
.trialPopulation <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e3_principal_inclusion_criteria",
      "e4_principal_exclusion_criteria",
      "f111_in_utero",
      "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks",
      "f113_newborns_027_days",
      "f114_infants_and_toddlers_28_days23_months",
      "f115_children_211years",
      "f116_adolescents_1217_years",
      "f11_trial_has_subjects_under_18",
      "f12_adults_1864_years",
      "f13_elderly_65_years"
    ),
    "ctgov" = c(
      "eligibility.criteria.textblock",
      "eligibility.maximum_age",
      "eligibility.minimum_age"
    ),
    "ctgov2" = c(
      "protocolSection.eligibilityModule.maximumAge",
      "protocolSection.eligibilityModule.minimumAge",
      "protocolSection.eligibilityModule.eligibilityCriteria"
    ),
    "isrctn" = c(
      "participants.ageRange",
      "participants.inclusion",
      "participants.exclusion"
    ),
    "ctis" = c(
      "ageGroup",
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria",
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria"
    ))


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates from protocol-related information the age groups that can
participate in a trial.

Returns new columns:
.trialPopulationAgeGroup (factor, "P", "A", "P+A", "E", "A+E", "P+A+E")
.trialPopulationInclusion (string)
.trialPopulationExclusion (string)
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

  # helper function similar to unite
  anyCols <- function(...) apply(
    ..., 1, function(i) ifelse(
      any(i), TRUE, ifelse(all(is.na(i)), NA, FALSE)))


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      #
      euctrAnyPaediatric = dplyr::select(
        df, c(
          "f111_in_utero",
          "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks",
          "f113_newborns_027_days",
          "f114_infants_and_toddlers_28_days23_months",
          "f115_children_211years",
          "f116_adolescents_1217_years",
          "f11_trial_has_subjects_under_18"
        )) %>% anyCols(.),
      #
      # initialise
      .trialPopulationInclusion = NA_character_,
      .trialPopulationExclusion = NA_character_,
      .trialPopulationAgeGroup = NA_character_,
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        euctrAnyPaediatric & f12_adults_1864_years ~ "P+A",
        f12_adults_1864_years & f13_elderly_65_years ~ "A+E",
        euctrAnyPaediatric & f12_adults_1864_years &
          f13_elderly_65_years ~ "P+A+E",
        euctrAnyPaediatric ~ "P",
        f12_adults_1864_years ~ "A",
        f13_elderly_65_years ~ "E",
        .default = .trialPopulationAgeGroup
      ),
      #
      .trialPopulationInclusion = dplyr::if_else(
        !is.na(e3_principal_inclusion_criteria),
        e3_principal_inclusion_criteria,
        .trialPopulationInclusion
      ),
      #
      .trialPopulationExclusion = dplyr::if_else(
        !is.na(e4_principal_exclusion_criteria),
        e4_principal_exclusion_criteria,
        .trialPopulationExclusion
      )
      #
    ) -> df


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        eligibility.maximum_age >= lubridate::as.period("65 years") &
          eligibility.minimum_age < lubridate::as.period("18 years") ~ "P+A+E",
        eligibility.maximum_age > lubridate::as.period("18 years") &
          eligibility.minimum_age < lubridate::as.period("18 years") ~ "P+A",
        eligibility.maximum_age >= lubridate::as.period("65 years") &
          eligibility.minimum_age > lubridate::as.period("18 years") ~ "A+E",
        eligibility.maximum_age < lubridate::as.period("18 years") ~ "P",
        eligibility.minimum_age > lubridate::as.period("18 years") ~ "A",
        .default = .trialPopulationAgeGroup
      ),
      #
      eligibility.criteria.textblock = gsub(
        "[\n\r]+", "", eligibility.criteria.textblock),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(eligibility.criteria.textblock), .trialPopulationInclusion,
        trimws(stringi::stri_replace_all_regex(
          eligibility.criteria.textblock,
          "^Inclusion Criteria:(.+)Exclusion Criteria:(.+)$", "$1"
        ))),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(eligibility.criteria.textblock), .trialPopulationExclusion,
        trimws(stringi::stri_replace_all_regex(
          eligibility.criteria.textblock,
          "^Inclusion Criteria:(.+)Exclusion Criteria:(.+)$", "$2"
        )))
      #
    ) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        protocolSection.eligibilityModule.maximumAge >= lubridate::as.period("65 years") &
          protocolSection.eligibilityModule.minimumAge < lubridate::as.period("18 years") ~ "P+A+E",
        protocolSection.eligibilityModule.maximumAge > lubridate::as.period("18 years") &
          protocolSection.eligibilityModule.minimumAge < lubridate::as.period("18 years") ~ "P+A",
        protocolSection.eligibilityModule.maximumAge >= lubridate::as.period("65 years") &
          protocolSection.eligibilityModule.minimumAge > lubridate::as.period("18 years") ~ "A+E",
        protocolSection.eligibilityModule.maximumAge < lubridate::as.period("18 years") ~ "P",
        protocolSection.eligibilityModule.minimumAge > lubridate::as.period("18 years") ~ "A",
        .default = .trialPopulationAgeGroup
      ),
      #
      protocolSection.eligibilityModule.eligibilityCriteria = gsub(
        "[\n\r]+", "", protocolSection.eligibilityModule.eligibilityCriteria),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(protocolSection.eligibilityModule.eligibilityCriteria), .trialPopulationInclusion,
        trimws(stringi::stri_replace_all_regex(
          protocolSection.eligibilityModule.eligibilityCriteria,
          "^Inclusion Criteria:(.+)Exclusion Criteria:(.+)$", "$1"
        ))),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(protocolSection.eligibilityModule.eligibilityCriteria), .trialPopulationExclusion,
        trimws(stringi::stri_replace_all_regex(
          protocolSection.eligibilityModule.eligibilityCriteria,
          "^Inclusion Criteria:(.+)Exclusion Criteria:(.+)$", "$2"
        )))
      #
    ) -> df


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      #
      .trialPopulationAgeGroup = dplyr::case_match(
        participants.ageRange,
        "Adult" ~ "A",
        "All" ~ "P+A+E",
        "Child" ~ "P",
        "Neonate" ~ "P",
        "Senior" ~ "E",
        # "Mixed" ~ "P",
        # "Not Specified" ~ "P",
        # "Other" ~ "P",
        .default = .trialPopulationAgeGroup
      ),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(participants.inclusion),
        .trialPopulationInclusion,
        participants.inclusion
      ),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(participants.exclusion),
        .trialPopulationExclusion,
        participants.exclusion
      )
      #
    ) -> df


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      # ageGroups are variably concatenated, normalise
      ageGroup = sapply(df$ageGroup, function(i)
        paste0(sort(stringi::stri_split_fixed(i, ", ")[[1]]), collapse = ", "),
        USE.NAMES = FALSE),
      #
      .trialPopulationAgeGroup = dplyr::case_match(
        ageGroup,
        "0-17 years, 18-64 years, 65+ years" ~ "P+A+E",
        "0-17 years, 18-64 years" ~ "P+A",
        "18-64 years, 65+ years" ~ "A+E",
        "0-17 years" ~ "P",
        "18-64 years" ~ "A",
        "65+ years" ~ "E",
        .default = .trialPopulationAgeGroup
      ),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria),
        .trialPopulationInclusion,
        authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria
      ),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria),
        .trialPopulationExclusion,
        authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria
      )
      #
    ) -> df


  #### merge ####

  # keep only outcome columns
  df %>%
    dplyr::select(
      "_id",
      .trialPopulationInclusion,
      .trialPopulationExclusion,
      .trialPopulationAgeGroup,
    ) -> df

  # factorise
  df$.trialPopulationAgeGroup <- factor(df$.trialPopulationAgeGroup)


  #### checks ####
  stopifnot(ncol(df) == 4L)
  stopifnot(inherits(df[[".trialPopulationInclusion"]], "character"))
  stopifnot(inherits(df[[".trialPopulationExclusion"]], "character"))
  stopifnot(inherits(df[[".trialPopulationAgeGroup"]], "factor"))

  # return
  return(df)

} # end .trialPopulation
