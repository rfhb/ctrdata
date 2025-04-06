#### history ####
# 2025-01-26 first version

#' Calculate in- and exclusion criteria and age groups
#'
#' Trial concept calculated: inclusion and exclusion criteria as well as
#' age groups that can participate in a trial, based on protocol-related
#' information. Since CTGOV uses single text field for eligibility criteria,
#' text extraction is used to separate in- and exclusion criteria.
#' (See \link{dfMergeVariablesRelevel} with an example for healthy volunteers.)
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and new columns:
#' `.trialPopulationAgeGroup` (factor, "P", "A", "P+A", "E", "A+E", "P+A+E"),
#' `.trialPopulationInclusion` (string),
#' `.trialPopulationExclusion` (string).
#'
#' @export
#'
#' @importFrom dplyr mutate select case_when if_else `%>%`
#' @importFrom stringi stri_replace_all_regex stri_split_fixed
#' @importFrom lubridate as.period
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.trialPopulation()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.trialPopulation",
#'   con = dbc)
#' trialsDf
#'
f.trialPopulation <- function(df = NULL) {

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
      # "f31_healthy_volunteers"
    ),
    "ctgov" = c(
      "eligibility.criteria.textblock",
      "eligibility.maximum_age",
      "eligibility.minimum_age"
      # "eligibility.healthy_volunteers"
    ),
    "ctgov2" = c(
      "protocolSection.eligibilityModule.maximumAge",
      "protocolSection.eligibilityModule.minimumAge",
      "protocolSection.eligibilityModule.eligibilityCriteria",
      "protocolSection.eligibilityModule.stdAges"
      # "protocolSection.eligibilityModule.healthyVolunteers"
    ),
    "isrctn" = c(
      "participants.ageRange",
      "participants.inclusion",
      "participants.exclusion"
    ),
    "ctis" = c(
      "ageGroup",
      # ctis1
      "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria",
      "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria",
      # ctis2
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria",
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria"
    ))


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

  # helper function similar to unite
  anyCols <- function(...) apply(
    ..., 1, function(i) ifelse(
      any(i), TRUE, ifelse(all(is.na(i)), NA, FALSE)))


  #### . EUCTR ####
  dplyr::mutate(
    df, euctrAnyPaediatric = anyCols(
      dplyr::select(
        df, c(
          "f111_in_utero",
          "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks",
          "f113_newborns_027_days",
          "f114_infants_and_toddlers_28_days23_months",
          "f115_children_211years",
          "f116_adolescents_1217_years",
          "f11_trial_has_subjects_under_18"
        ))
    )) %>%
    dplyr::pull("euctrAnyPaediatric") -> df$euctrAnyPaediatric

  #
  df %>%
    dplyr::mutate(
      # initialise
      .trialPopulationInclusion = NA_character_,
      .trialPopulationExclusion = NA_character_,
      .trialPopulationAgeGroup = NA_character_,
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        .data$euctrAnyPaediatric & .data$f12_adults_1864_years ~ "P+A",
        .data$f12_adults_1864_years & .data$f13_elderly_65_years ~ "A+E",
        .data$euctrAnyPaediatric & .data$f12_adults_1864_years &
          .data$f13_elderly_65_years ~ "P+A+E",
        .data$euctrAnyPaediatric ~ "P",
        .data$f12_adults_1864_years ~ "A",
        .data$f13_elderly_65_years ~ "E",
        .default = .data$.trialPopulationAgeGroup
      ),
      #
      .trialPopulationInclusion = dplyr::if_else(
        !is.na(.data$e3_principal_inclusion_criteria),
        .data$e3_principal_inclusion_criteria,
        .data$.trialPopulationInclusion
      ),
      #
      .trialPopulationExclusion = dplyr::if_else(
        !is.na(.data$e4_principal_exclusion_criteria),
        .data$e4_principal_exclusion_criteria,
        .data$.trialPopulationExclusion
      )
      #
    ) -> df

  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        .data$eligibility.maximum_age >= lubridate::as.period("65 years") &
          .data$eligibility.minimum_age < lubridate::as.period("18 years") ~ "P+A+E",
        .data$eligibility.maximum_age >= lubridate::as.period("18 years") &
          .data$eligibility.minimum_age < lubridate::as.period("18 years") ~ "P+A",
        .data$eligibility.maximum_age >= lubridate::as.period("65 years") &
          .data$eligibility.minimum_age >= lubridate::as.period("18 years") ~ "A+E",
        .data$eligibility.maximum_age < lubridate::as.period("18 years") ~ "P",
        .data$eligibility.minimum_age >= lubridate::as.period("18 years") ~ "A",
        .default = .data$.trialPopulationAgeGroup
      ),
      #
      eligibility.criteria.textblock = gsub(
        "[\n\r]+", "", .data$eligibility.criteria.textblock),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(.data$eligibility.criteria.textblock), .data$.trialPopulationInclusion,
        trimws(stringi::stri_replace_all_regex(
          .data$eligibility.criteria.textblock,
          "^(|Patient )Inclusion Criteria:(.+)(|Patient )Exclusion Criteria:(.+)$", "$2"
        ))),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(.data$eligibility.criteria.textblock), .data$.trialPopulationExclusion,
        trimws(stringi::stri_replace_all_regex(
          .data$eligibility.criteria.textblock,
          "^(|Patient )Inclusion Criteria:(.+)(|Patient )Exclusion Criteria:(.+)$", "$4"
        )))
      #
    ) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        .data$protocolSection.eligibilityModule.maximumAge >= lubridate::as.period("65 years") &
          .data$protocolSection.eligibilityModule.minimumAge < lubridate::as.period("18 years") ~ "P+A+E",
        .data$protocolSection.eligibilityModule.maximumAge >= lubridate::as.period("18 years") &
          .data$protocolSection.eligibilityModule.minimumAge < lubridate::as.period("18 years") ~ "P+A",
        .data$protocolSection.eligibilityModule.maximumAge >= lubridate::as.period("65 years") &
          .data$protocolSection.eligibilityModule.minimumAge >= lubridate::as.period("18 years") ~ "A+E",
        .data$protocolSection.eligibilityModule.maximumAge < lubridate::as.period("18 years") ~ "P",
        .data$protocolSection.eligibilityModule.minimumAge >= lubridate::as.period("18 years") ~ "A",
        .default = .data$.trialPopulationAgeGroup
      ),
      #
      #
      .trialPopulationAgeGroup = dplyr::case_when(
        !is.na(.data$.trialPopulationAgeGroup) ~ .data$.trialPopulationAgeGroup,
        # CHILD / ADULT / OLDER_ADULT
        grepl("CHILD", .data$protocolSection.eligibilityModule.stdAges) &
          grepl("ADULT", .data$protocolSection.eligibilityModule.stdAges) &
          grepl("OLDER_ADULT", .data$protocolSection.eligibilityModule.stdAges) ~ "P+A+E",
        grepl("CHILD", .data$protocolSection.eligibilityModule.stdAges) &
          grepl("ADULT", .data$protocolSection.eligibilityModule.stdAges) ~ "P+A",
        grepl("ADULT", .data$protocolSection.eligibilityModule.stdAges) &
          grepl("OLDER_ADULT", .data$protocolSection.eligibilityModule.stdAges) ~ "A+E",
        grepl("CHILD", .data$protocolSection.eligibilityModule.stdAges) ~ "P",
        grepl("ADULT", .data$protocolSection.eligibilityModule.stdAges) ~ "A",
        grepl("OLDER_ADULT", .data$protocolSection.eligibilityModule.stdAges) ~ "E"
      ),
      #
      protocolSection.eligibilityModule.eligibilityCriteria = gsub(
        "[\n\r]+", "", .data$protocolSection.eligibilityModule.eligibilityCriteria),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(.data$protocolSection.eligibilityModule.eligibilityCriteria), .data$.trialPopulationInclusion,
        trimws(stringi::stri_replace_all_regex(
          .data$protocolSection.eligibilityModule.eligibilityCriteria,
          "^(|Patient )Inclusion Criteria:(.+)(|Patient )Exclusion Criteria:(.+)$", "$2"
        ))),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(.data$protocolSection.eligibilityModule.eligibilityCriteria), .data$.trialPopulationExclusion,
        trimws(stringi::stri_replace_all_regex(
          .data$protocolSection.eligibilityModule.eligibilityCriteria,
          "^(|Patient )Inclusion Criteria:(.+)(|Patient )Exclusion Criteria:(.+)$", "$4"
        )))
      #
    ) -> df


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      #
      .trialPopulationAgeGroup = dplyr::case_match(
        .data$participants.ageRange,
        "Adult" ~ "A",
        "All" ~ "P+A+E",
        "Child" ~ "P",
        "Neonate" ~ "P",
        "Senior" ~ "E",
        # "Mixed" ~ "P",
        # "Not Specified" ~ "P",
        # "Other" ~ "P",
        .default = .data$.trialPopulationAgeGroup
      ),
      #
      .trialPopulationInclusion = dplyr::if_else(
        is.na(.data$participants.inclusion),
        .data$.trialPopulationInclusion,
        .data$participants.inclusion
      ),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(.data$participants.exclusion),
        .data$.trialPopulationExclusion,
        .data$participants.exclusion
      )
      #
    ) -> df


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      # ageGroups are variably concatenated, normalise
      ageGroup = sapply(.data$ageGroup, function(i)
        paste0(sort(stringi::stri_split_fixed(i, ", ")[[1]]), collapse = ", "),
        USE.NAMES = FALSE),
      #
      .trialPopulationAgeGroup = dplyr::case_match(
        .data$ageGroup,
        "0-17 years, 18-64 years, 65+ years" ~ "P+A+E",
        "0-17 years, 18-64 years" ~ "P+A",
        "18-64 years, 65+ years" ~ "A+E",
        "0-17 years" ~ "P",
        "18-64 years" ~ "A",
        "65+ years" ~ "E",
        .default = .data$.trialPopulationAgeGroup
      ),
      #
      # ctis1
      .trialPopulationInclusion = dplyr::if_else(
        is.na(.data$authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria),
        .data$.trialPopulationInclusion,
        .data$authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria
      ),
      #
      .trialPopulationExclusion = dplyr::if_else(
        is.na(.data$authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria),
        .data$.trialPopulationExclusion,
        .data$authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria
      ),
      # ctis2
      .trialPopulationInclusion = dplyr::if_else(
        is.na(.data$authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria),
        .data$.trialPopulationInclusion,
        .data$authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.principalInclusionCriteria
      ),
      .trialPopulationExclusion = dplyr::if_else(
        is.na(.data$authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria),
        .data$.trialPopulationExclusion,
        .data$authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.principalExclusionCriteria
      )
      #
    ) -> df


  #### merge ####

  # keep only outcome columns
  df %>%
    dplyr::select(c(
      "_id",
      ".trialPopulationAgeGroup",
      ".trialPopulationInclusion",
      ".trialPopulationExclusion"
    )) -> df

  # factorise
  df$.trialPopulationAgeGroup <- factor(df$.trialPopulationAgeGroup)


  #### checks ####
  stopifnot(ncol(df) == 4L)
  stopifnot(inherits(df[[".trialPopulationInclusion"]], "character"))
  stopifnot(inherits(df[[".trialPopulationExclusion"]], "character"))
  stopifnot(inherits(df[[".trialPopulationAgeGroup"]], "factor"))

  # return
  return(df)

} # end f.trialPopulation
