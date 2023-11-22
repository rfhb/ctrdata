### ctrdata package
### utility functions

#### variable definitions ####

# prototype return structure
emptyReturn <- list(n = 0L, success = NULL, failed = NULL)
#
# EUCTR definitions
countriesEUCTR <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB", "IS", "LI",
  "NO", "3RD")
#
# regexpr
# - queryterm and urls
regQueryterm <- "[^-.a-zA-Z0-9=?+&#%_:\"/, ]"
# - EudraCT e.g. 2010-022945-52
regEuctr <- "[0-9]{4}-[0-9]{6}-[0-9]{2}"
# - CTGOV
regCtgov <- "NCT[0-9]{8}"
# - CTGOV2
regCtgov2 <- regCtgov
# - regIsrctn
# FIXME check if first digit is always non zero
regIsrctn <- "[1-9][0-9]{7}"
# - CTIS e.g. 2022-501549-57-00
regCtis <- "[0-9]{4}-[0-9]{6}-[0-9]{2}-[0-9]{2}"
#
# register list
registerList <- c("EUCTR", "CTGOV", "ISRCTN", "CTIS", "CTGOV2")
#
# mapping field names to typing function for typeField()
typeVars <- list(
  #
  # dates
  #
  # - ctrdata intern
  "record_last_import" = "ctrDateCtr",
  #
  # - EUCTR
  "n_date_of_competent_authority_decision" = "ctrDate",
  "n_date_of_ethics_committee_opinion"     = "ctrDate",
  "p_date_of_the_global_end_of_the_trial"  = "ctrDate",
  "firstreceived_results_date"             = "ctrDate",
  "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database" = "ctrDate",
  "trialInformation.primaryCompletionDate" = "ctrDate",
  "trialInformation.analysisStageDate"     = "ctrDateTime",
  "trialInformation.globalEndOfTrialDate"  = "ctrDateTime",
  "trialInformation.recruitmentStartDate"  = "ctrDateTime",
  #
  "e891_in_the_member_state_concerned_days"   = "ctrDifftimeDays",
  "e891_in_the_member_state_concerned_months" = "ctrDifftimeMonths",
  "e891_in_the_member_state_concerned_years"  = "ctrDifftimeYears",
  "e892_in_all_countries_concerned_by_the_trial_days"   = "ctrDifftimeDays",
  "e892_in_all_countries_concerned_by_the_trial_months" = "ctrDifftimeMonths",
  "e892_in_all_countries_concerned_by_the_trial_years"  = "ctrDifftimeYears",
  #
  # - CTGOV
  "completion_date"          = "ctrDateUs",
  "last_update_posted"       = "ctrDateUs",
  "last_update_submitted_qc" = "ctrDateUs",
  "last_update_submitted"    = "ctrDateUs",
  "primary_completion_date"  = "ctrDateUs",
  "results_first_posted"     = "ctrDateUs",
  "start_date"               = "ctrDateUs",
  "study_first_posted"       = "ctrDateUs",
  "verification_date"        = "ctrDateUs",
  "required_header.download_date" = "ctrDateUs",
  "eligibility.minimum_age" = "ctrDifftime",
  "eligibility.maximum_age" = "ctrDifftime",
  #
  # - CTGOV2
  "protocolSection.statusModule.completionDateStruct.date"        = "ctrDate",
  "protocolSection.statusModule.lastUpdatePostDateStruct.date"    = "ctrDate",
  "protocolSection.statusModule.lastUpdateSubmitDate"             = "ctrDate",
  "protocolSection.statusModule.primaryCompletionDateStruct.date" = "ctrDate",
  "protocolSection.statusModule.startDateStruct.date"             = "ctrDate",
  "protocolSection.statusModule.studyFirstPostDateStruct.date"    = "ctrDate",
  #
  # - ISRCTN
  "participants.recruitmentStart" = "ctrDateTime",
  "participants.recruitmentEnd"   = "ctrDateTime",
  "trialDesign.overallStartDate"  = "ctrDateTime",
  "trialDesign.overallEndDate"    = "ctrDateTime",
  #
  # - CTIS
  "applications.ctMSCs.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.ctMSCs.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.ctMSCs.firstDecisionDate" = "ctrDate",
  "applications.ctMSCs.fromDate" = "ctrDate",
  "applications.ctMSCs.toDate" = "ctrDate",
  "applications.ctMSCs.trialPeriod.fromDate" = "ctrDate",
  "applications.ctMSCs.trialPeriod.trialStartDate" = "ctrDate",
  "applications.ctMSCsByApplication.fromDate" = "ctrDate",
  "applications.ctMSCsByApplication.toDate" = "ctrDate",
  "applications.decisionDate" = "ctrDate",
  "applications.isDossierUpdate" = "ctrDate",
  "applications.partI.assessmentOutcomeDate" = "ctrDate",
  "applications.partI.conclusionDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.startDate" = "ctrDate",
  "applications.partI.products.startDate" = "ctrDate",
  "applications.partI.sponsors.fromDate" = "ctrDate",
  "applications.partI.submissionDate" = "ctrDate",
  "applications.partI.trialDetails.trialInformation.trialDuration.estimatedEndDate" = "ctrDate",
  "applications.partI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate" = "ctrDate",
  "applications.partIIInfo.applicationStatusDate" = "ctrDate",
  "applications.partIIInfo.conclusionDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.firstDecisionDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.toDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialPeriod.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialPeriod.trialStartDate" = "ctrDate",
  "applications.partIIInfo.submissionDate" = "ctrDate",
  "applications.primarySponsor.fromDate" = "ctrDate",
  "applications.reportingDate" = "ctrDate",
  "applications.submissionDate" = "ctrDate",
  "applications.validationConclusionDate" = "ctrDate",
  "authorizationDate" = "ctrDate",
  "authorizedPartI.assessmentOutcomeDate" = "ctrDate",
  "authorizedPartI.conclusionDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.startDate" = "ctrDate",
  "authorizedPartI.products.startDate" = "ctrDate",
  "authorizedPartI.sponsors.fromDate" = "ctrDate",
  "authorizedPartI.submissionDate" = "ctrDate",
  "authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedEndDate" = "ctrDate",
  "authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate" = "ctrDate",
  "authorizedPartsII.applicationStatusDate" = "ctrDate",
  "authorizedPartsII.conclusionDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartsII.mscInfo.firstDecisionDate" = "ctrDate",
  "authorizedPartsII.mscInfo.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.toDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartsII.submissionDate" = "ctrDate",
  "coSponsors.fromDate" = "ctrDate",
  "decisionDate" = "ctrDate",
  "eeaStartDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.fromDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.trialStartDate" = "ctrDate",
  "memberStatesConcerned.firstDecisionDate" = "ctrDate",
  "memberStatesConcerned.fromDate" = "ctrDate",
  "memberStatesConcerned.toDate" = "ctrDate",
  "memberStatesConcerned.trialPeriod.fromDate" = "ctrDate",
  "memberStatesConcerned.trialPeriod.trialStartDate" = "ctrDate",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.date" = "ctrDate",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.submitDate" = "ctrDate",
  "primarySponsor.fromDate" = "ctrDate",
  "publicEvaluation.decisions.decisionDate" = "ctrDate",
  "publicEvaluation.partIAssessmentOutcomeDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIConclusion.decisionInfoList.decisionDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIConclusion.eventDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfiConsiderations.rfiConsiderations.fromDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfiConsiderations.rfiConsiderations.rfiSubmissionDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfiConsiderations.rfiConsiderations.sponsorSubmitDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfis.createdDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfis.dueDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfis.responseDate" = "ctrDate",
  "publicEvaluation.partIIEvaluationList.partIIRfis.submissionDate" = "ctrDate",
  "publicEvaluation.partIRfiConsiderations.rfiConsiderations.fromDate" = "ctrDate",
  "publicEvaluation.partIRfiConsiderations.rfiConsiderations.rfiSubmissionDate" = "ctrDate",
  "publicEvaluation.partIRfiConsiderations.rfiConsiderations.sponsorSubmitDate" = "ctrDate",
  "publicEvaluation.partIRfis.createdDate" = "ctrDate",
  "publicEvaluation.partIRfis.dueDate" = "ctrDate",
  "publicEvaluation.partIRfis.responseDate" = "ctrDate",
  "publicEvaluation.partIRfis.submissionDate" = "ctrDate",
  "publicEvaluation.validationConclusion.conclusionDate" = "ctrDate",
  "publicEvaluation.validationRfiConsiderations.rfiConsiderations.fromDate" = "ctrDate",
  "publicEvaluation.validationRfiConsiderations.rfiConsiderations.rfiSubmissionDate" = "ctrDate",
  "publicEvaluation.validationRfiConsiderations.rfiConsiderations.sponsorSubmitDate" = "ctrDate",
  "publicEvaluation.validationRfis.createdDate" = "ctrDate",
  "publicEvaluation.validationRfis.dueDate" = "ctrDate",
  "publicEvaluation.validationRfis.responseDate" = "ctrDate",
  "publicEvaluation.validationRfis.submissionDate" = "ctrDate",
  "startDateEU" = "ctrDate",
  "submissionDate" = "ctrDate",
  "trialStartDate" = "ctrDate",
  #
  #
  # factors / logical
  #
  # - EUCTR Yes / No / Information not present in EudraCT
  "a7_trial_is_part_of_a_paediatric_investigation_plan" = "ctrYesNo",
  "dimp.d21_imp_to_be_used_in_the_trial_has_a_marketing_authorisation" = "ctrYesNo",
  "e13_condition_being_studied_is_a_rare_disease" = "ctrYesNo",
  "e23_trial_contains_a_substudy" = "ctrYesNo",
  #
  "e61_diagnosis"         = "ctrYesNo",
  "e62_prophylaxis"       = "ctrYesNo",
  "e63_therapy"           = "ctrYesNo",
  "e64_safety"            = "ctrYesNo",
  "e65_efficacy"          = "ctrYesNo",
  "e66_pharmacokinetic"   = "ctrYesNo",
  "e67_pharmacodynamic"   = "ctrYesNo",
  "e68_bioequivalence"    = "ctrYesNo",
  "e69_dose_response"     = "ctrYesNo",
  "e610_pharmacogenetic"  = "ctrYesNo",
  "e611_pharmacogenomic"  = "ctrYesNo",
  "e612_pharmacoeconomic" = "ctrYesNo",
  "e613_others"           = "ctrYesNo",
  #
  "e71_human_pharmacology_phase_i"         = "ctrYesNo",
  "e711_first_administration_to_humans"    = "ctrYesNo",
  "e712_bioequivalence_study"              = "ctrYesNo",
  "e713_other"                             = "ctrYesNo",
  "e72_therapeutic_exploratory_phase_ii"   = "ctrYesNo",
  "e73_therapeutic_confirmatory_phase_iii" = "ctrYesNo",
  "e74_therapeutic_use_phase_iv"           = "ctrYesNo",
  #
  "e81_controlled"      = "ctrYesNo",
  "e811_randomised"     = "ctrYesNo",
  "e812_open"           = "ctrYesNo",
  "e813_single_blind"   = "ctrYesNo",
  "e814_double_blind"   = "ctrYesNo",
  "e815_parallel_group" = "ctrYesNo",
  "e816_cross_over"     = "ctrYesNo",
  "e817_other"          = "ctrYesNo",
  "e822_placebo"        = "ctrYesNo",
  #
  "e83_the_trial_involves_single_site_in_the_member_state_concerned"    = "ctrYesNo",
  "e83_will_this_trial_be_conducted_at_a_single_site_globally"          = "ctrYesNo",
  "e83_single_site_trial"                                               = "ctrYesNo", # 2023-10-05
  "e84_multiple_sites_in_member_state"                                  = "ctrYesNo", # 2023-10-05
  "e84_will_this_trial_be_conducted_at_multiple_sites_globally"         = "ctrYesNo",
  "e84_the_trial_involves_multiple_sites_in_the_member_state_concerned" = "ctrYesNo",
  "e840_multiple_sites_globally"                                        = "ctrYesNo", # 2023-10-05
  "e84_multiple_sites_in_member_state"                                  = "ctrYesNo", # 2023-10-05
  "e85_the_trial_involves_multiple_member_states"                       = "ctrYesNo",
  "e861_trial_being_conducted_both_within_and_outside_the_eea"          = "ctrYesNo",
  "e862_trial_being_conducted_completely_outside_of_the_eea"            = "ctrYesNo",
  "e87_trial_has_a_data_monitoring_committee"                           = "ctrYesNo",
  #
  "f11_trial_has_subjects_under_18"            = "ctrYesNo",
  "f111_in_utero"                              = "ctrYesNo",
  "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks" = "ctrYesNo",
  "f113_newborns_027_days"                     = "ctrYesNo",
  "f114_infants_and_toddlers_28_days23_months" = "ctrYesNo",
  "f115_children_211years"                     = "ctrYesNo",
  "f116_adolescents_1217_years"                = "ctrYesNo",
  "f12_adults_1864_years"                      = "ctrYesNo",
  "f13_elderly_65_years"                       = "ctrYesNo",
  "f21_female"                                 = "ctrYesNo",
  "f22_male"                                   = "ctrYesNo",
  "f31_healthy_volunteers"                     = "ctrYesNo",
  "f32_patients"                               = "ctrYesNo",
  "f33_specific_vulnerable_populations"        = "ctrYesNo",
  "f331_women_of_childbearing_potential_not_using_contraception_" = "ctrYesNo",
  "f332_women_of_childbearing_potential_using_contraception"      = "ctrYesNo",
  "f333_pregnant_women"      = "ctrYesNo",
  "f334_nursing_women"       = "ctrYesNo",
  "f335_emergency_situation" = "ctrYesNo",
  "f336_subjects_incapable_of_giving_consent_personally" = "ctrYesNo",
  #
  "trialInformation.analysisForPrimaryCompletion" = "ctrFalseTrue",
  "trialInformation.partOfPIP"                 = "ctrFalseTrue",
  "trialInformation.art45Related"              = "ctrFalseTrue",
  "trialInformation.art46Related"              = "ctrFalseTrue",
  "trialInformation.longTermFollowUpPlanned"   = "ctrFalseTrue",
  "trialInformation.idmcInvolvement"           = "ctrFalseTrue",
  "trialInformation.isGlobalEndOfTrialReached" = "ctrFalseTrue",
  "trialInformation.globalEndOfTrialPremature" = "ctrFalseTrue",
  #
  # - CTGOV
  "has_expanded_access"            = "ctrYesNo",
  "oversight_info.has_dmc"         = "ctrYesNo",
  "eligibility.healthy_volunteers" = "ctrYesNo",
  #
  # - ISRCTN
  "trialDescription.acknowledgment" = "ctrFalseTrue",
  "results.biomedRelated"           = "ctrFalseTrue",
  #
  # - CTIS
  "hasDeferrallApplied" = "ctrFalseTrue",
  "hasAmendmentApplied" = "ctrFalseTrue",
  "eudraCtInfo.hasVhp" = "ctrFalseTrue",
  #
  # numbers
  #
  # - EUCTR
  "e824_number_of_treatment_arms_in_the_trial"  = "ctrInt",
  "e841_number_of_sites_anticipated_in_member_state_concerned" = "ctrInt",
  "e851_number_of_sites_anticipated_in_the_eea" = "ctrInt",
  "e891_in_the_member_state_concerned_years"    = "ctrInt",
  "e891_in_the_member_state_concerned_months"   = "ctrInt",
  "e891_in_the_member_state_concerned_days"     = "ctrInt",
  "e892_in_all_countries_concerned_by_the_trial_years"  = "ctrInt",
  "e892_in_all_countries_concerned_by_the_trial_months" = "ctrInt",
  "e892_in_all_countries_concerned_by_the_trial_days"   = "ctrInt",
  "f11_number_of_subjects_for_this_age_range"   = "ctrInt",
  "f1111_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1121_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1131_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1141_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1151_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1161_number_of_subjects_for_this_age_range" = "ctrInt",
  "f121_number_of_subjects_for_this_age_range"  = "ctrInt",
  "f131_number_of_subjects_for_this_age_range"  = "ctrInt",
  "f41_in_the_member_state"          = "ctrInt",
  "f421_in_the_eea"                  = "ctrInt",
  "f422_in_the_whole_clinical_trial" = "ctrInt",
  #
  "trialInformation.populationAgeGroup.inUtero"               = "ctrInt",
  "trialInformation.populationAgeGroup.pretermNewbornInfants" = "ctrInt",
  "trialInformation.populationAgeGroup.newborns"              = "ctrInt",
  "trialInformation.populationAgeGroup.infantsAndToddlers"    = "ctrInt",
  "trialInformation.populationAgeGroup.children"              = "ctrInt",
  "trialInformation.populationAgeGroup.adolescents"           = "ctrInt",
  "trialInformation.populationAgeGroup.adults"                = "ctrInt",
  "trialInformation.populationAgeGroup.elderly65To84"         = "ctrInt",
  "trialInformation.populationAgeGroup.elderlyOver85"         = "ctrInt",
  #
  # - CTGOV
  "number_of_arms" = "ctrInt",
  "enrollment"     = "ctrInt",
  #
  # - ISRCTN
  "participants.targetEnrolment"     = "ctrInt",
  "participants.totalFinalEnrolment" = "ctrInt",
  #
  # - CTIS
  "totalNumberEnrolled" = "ctrInt"
  #
)

#### functions ####

#' ctgovVersion
#'
#' Checks for mismatch between label CTGOV and CTGOV2
#' and tries to guess the correct label
#'
#' @param url url or data frame with query term
#' @param register any of the register names
#'
#' @keywords internal
#' @noRd
#'
#' @returns string
#'
#' @examples
#'
#' ctgovVersion("https://www.clinicaltrials.gov/ct2/show/NCT02703272", "")
#' ctgovVersion("https://classic.clinicaltrials.gov/ct2/results?cond=&term=NCT02703272&cntry=", "")
#' ctgovVersion("https://clinicaltrials.gov/ct2/results?cond=&term=NCT02703272&cntry=", "")
#' ctgovVersion("https://classic.clinicaltrials.gov/ct2/show/NCT02703272?term=NCT02703272&draw=2&rank=1")
#' ctgovVersion("https://clinicaltrials.gov/ct2/results?cond=", "")
#'
#' ctgovVersion("https://www.clinicaltrials.gov/search?term=NCT04412252,%20NCT04368728", "")
#' ctgovVersion("term=NCT04412252,%20NCT04368728", "CTGOV2")
#' ctgovVersion("https://www.clinicaltrials.gov/search?distance=50&cond=Cancer", "")
#'
ctgovVersion <- function(url, register) {

  # in case the input is from dbQueryHistory
  if (!is.atomic(url)) try({url <- url[["query-term"]]}, silent = TRUE)
  if (inherits(url, "try-error") || is.null(url)) return(register)

  # logic 1
  if (grepl(paste0(
    "clinicaltrials[.]gov/ct2/|",
    # vvv These capture classic-specific parameters
    "[?&]state=|[?&]city=|[?&]dist=|[?&]rsub=|",
    "[?&]type=|[?&]rslt=|[?&]gndr=|[?&]cntry=|",
    "[?&][a-z]+_[a-z]+="), url)) {
    message("* Appears specific for CTGOV CLASSIC")
    return("CTGOV")
  }

  # logic 2
  if (grepl(paste0(
    # clear identifiers of CTGOV2
    "aggFilters|clinicaltrials[.]gov/(search|study)[/?]|",
    "[:][^/]|%3[aA]"), url)) {
    message("* Appears specific for CTGOV REST API 2.0.0")
    return("CTGOV2")
  }

  # default return
  message("Not overruling register label ", register)
  return(register)

}


#' Check, write, read cache object for ctrdata
#'
#' @param xname name of variable to read or write
#'
#' @param xvalue value of variable to write
#'
#' @param verbose set to `TRUE` to print debug info
#'
#' @keywords internal
#' @noRd
#'
#' @return value of variable or `NULL` if variable does not exist
#'
ctrCache <- function(xname, xvalue = NULL, verbose = FALSE) {

  # hidden environment .ctrdataenv created in zzz.R

  # write or overwrite and exit early
  if (!is.null(xvalue)) {
    assign(x = xname, value = xvalue, envir = .ctrdataenv)
    if (verbose) message(" wrote ", xname, " to cache ")
    return(xvalue)
  }

  # check and read any value for xname variable
  if (verbose) message(" accessing cache...", appendLF = FALSE)
  if (exists(x = xname, envir = .ctrdataenv)) {
    tmp <- try(get(x = xname, envir = .ctrdataenv), silent = TRUE)
    if (inherits(tmp, "try-error")) return(NULL)
    if (verbose) message("\b\b\b, returning ", xname, " ", appendLF = FALSE)
    return(tmp)
  }

  # default
  return(NULL)
}


#' Check and prepare nodbi connection object for ctrdata
#'
#' @param con A connection object, see section
#' `Databases` in \link{ctrdata}.
#'
#' @keywords internal
#'
#' @importFrom nodbi src_sqlite src_duckdb docdb_list
#' @importFrom utils capture.output
#'
#' @return Connection object as list, with collection
#'  element under root
#'
ctrDb <- function(con) {

  ## postgres
  if (inherits(con, "src_postgres")) {

    if (is.null(con$collection)) {
      stop("Specify 'collection' with a table name, using ",
           "<nodbi src_postgres object>[[\"collection\"]] <- \"test\"), ",
           "for package ctrdata to work.",
           call. = FALSE)
    }

    # add database as element under root
    con <- c(con,
             "db" = con$dbname,
             "ctrDb" = TRUE)

    ## return
    return(structure(con,
                     class = c("src_postgres", "docdb_src")))
  }

  ## sqlite
  if (inherits(con, "src_sqlite")) {

    if (is.null(con$collection)) {
      stop("Specify parameter 'collection' with a table name, ",
           "such as nodbi::src_sqlite(collection = 'test'), ",
           "for package ctrdata to work.",
           call. = FALSE)
    }

    # check
    if (inherits(try(nodbi::docdb_list(con), silent = TRUE), "try-error")) {
      con <- nodbi::src_sqlite(dbname = con$dbname,
                               collection = con$collection)
    }

    # add database as element under root
    con <- c(con,
             "db" = con$dbname,
             "ctrDb" = TRUE)

    # print warning
    if (grepl(":memory:", con$dbname)) {
      warning("Database not persisting",
              call. = FALSE, noBreaks. = FALSE)
    }

    ## return
    return(structure(con,
                     class = c("src_sqlite", "docdb_src")))
  }

  ## mongo
  if (inherits(con, "src_mongo")) {

    # rights may be insufficient to call info(),
    # hence this workaround that should always
    # work and be stable to retrieve name of
    # collection in the mongo connection
    # suppress... for reconnect info from mongolite
    coll <- suppressMessages(utils::capture.output(con$con)[1])
    coll <- sub("^.*'(.*)'.*$", "\\1", coll)

    # add collection as element under root
    con <- c(con,
             "collection" = coll,
             "ctrDb" = TRUE)

    ## return
    return(structure(con,
                     class = c("src_mongo", "docdb_src")))
  }

  ## duckdb
  if (inherits(con, "src_duckdb")) {

    if (is.null(con$collection)) {
      stop("Specify parameter 'collection' with a table name, ",
           "such as nodbi::src_duckdb(collection = 'test'), ",
           "for package ctrdata to work.",
           call. = FALSE)
    }

    # check
    if (inherits(try(nodbi::docdb_list(con), silent = TRUE), "try-error")) {
      con <- nodbi::src_duckdb(
        dbdir = attr(attr(con$con, "driver"), "dbdir"),
        collection = con$collection)
    }

    # add database as element under root
    con <- c(con,
             "db" = attr(attr(con$con, "driver"), "dbdir"),
             "ctrDb" = TRUE)

    # print warning about nodbi::src_duckdb()
    if (grepl(":memory:", attr(attr(con$con, "driver"), "dbdir"))) {
      warning("Database not persisting\n",
              call. = FALSE, noBreaks. = FALSE)

    }

    ## return
    return(structure(con,
                     class = c("src_duckdb", "docdb_src")))

  }

  ## unprepared for other nodbi adapters so far
  stop("Please specify in parameter 'con' a database connection. ",
       "crdata supports src_mongo(), src_sqlite(), src_postgres() and src_duckdb().",
       call. = FALSE)

} # end ctrDb



#' Change type of field based on name of field
#'
#' @param dv a vector of character strings
#'
#' @param fn a field name
#'
#' @return a typed vector, same length as dv
#'
#' @importFrom xml2 xml_text read_html
#' @importFrom lubridate duration
#'
#' @keywords internal
#' @noRd
#'
typeField <- function(dv, fn) {

  # early exit if dv is not character
  if (!is.atomic(dv)) return(dv)

  # early exit if dv is not character
  if (!all(class(dv) %in% "character")) return(dv)

  # clean up for all character vectors
  # - if NA as string, change to NA
  dv[grepl("^N/?A$|^ND$", dv)] <- NA
  # - remove explanatory text before date
  dv <- sub("^ClinicalTrials.gov processed this data on ", "", dv)
  # - give Month Year also a Day to work with as.Date
  dv <- sub("^([a-zA-Z]+) ([0-9]{4})$", "\\1 15, \\2", dv)
  # - convert html entities because these had to
  #   be left intact when converting to ndjson
  htmlEnt <- grepl("&[#a-zA-Z]+;", dv)
  if (any(htmlEnt)) dv[htmlEnt] <-
    sapply(dv[htmlEnt], function(i)
      xml2::xml_text(xml2::read_html(charToRaw(i))), USE.NAMES = FALSE)
  # - convert newline
  dv <- gsub("\r", "\n", dv)

  # early exit if fn is not in typeVars
  if (is.null(typeVars[[fn]])) return(dv)

  # for date time conversion
  lct <- Sys.getlocale("LC_TIME")

  # main typing functions
  ctrDate <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%Y-%m-%d")
  }
  #
  ctrDateUs <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%b %e, %Y")
  }
  #
  ctrDateCtr <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%Y-%m-%d %H:%M:%S")
  }
  #
  ctrDateTime <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%Y-%m-%dT%H:%M:%S")
  }
  #
  ctrYesNo <- function() {
    vapply(dv, FUN = function(x)
      switch(x, "Yes" = TRUE, "No" = FALSE, NA),
      logical(1L), USE.NAMES = FALSE)
  }
  #
  ctrFalseTrue <- function() {
    vapply(dv, FUN = function(x)
      switch(x, "true" = TRUE, "false" = FALSE, NA),
      logical(1L), USE.NAMES = FALSE)
  }
  #
  ctrInt       <- function() {
    vapply(dv, FUN = function(x)
      as.integer(x = x), integer(1L),
      USE.NAMES = FALSE)
  }
  #
  ctrDifftime   <- function() {
    out <- sapply(dv, FUN = function(x) {
      if (is.na(x)) {NA} else {
        as.numeric(
          lubridate::duration(
            tolower(x)
          ), units = "days")
      }
    }, USE.NAMES = FALSE)
    as.difftime(out, units = "days")
  }
  #
  ctrDifftimeDays   <- function() {
    lubridate::ddays(x = as.numeric(dv))
  }
  #
  ctrDifftimeMonths   <- function() {
    lubridate::dmonths(x = as.numeric(dv))
  }
  #
  ctrDifftimeYears   <- function() {
    lubridate::dyears(x = as.numeric(dv))
  }

  ## apply typing
  ldv <- length(dv)
  if (any(grepl(" / ", dv))) {

    # if any concatenations, apply typing per concatenated
    # item and return list per item. note that dv has to be
    # overwritten in outer environment for typeVars to work
    out <- lapply(dv, function(r)  {
      dv <<- strsplit(r, " / ", fixed = TRUE)[[1]]
      try(do.call(typeVars[[fn]], list()), silent = TRUE)
    })

  } else {

    # apply typing function with its specified type
    out <- try(do.call(typeVars[[fn]], list()), silent = TRUE)

  }

  # error output
  if (any(sapply(out, function(r) inherits(r, "try-error"))) ||
      length(out) != ldv) {
    out <- rep.int(x = NA, times = ldv)
  }

  # return
  return(out)

} # end typeField



#' Annotate ctrdata function return values
#'
#' @param x object to be annotated
#'
#' @inheritParams ctrDb
#'
#' @keywords internal
#' @noRd
#'
addMetaData <- function(x, con) {

  # add metadata
  attr(x, "ctrdata-dbname")         <- con$db
  attr(x, "ctrdata-table")          <- con$collection
  attr(x, "ctrdata-dbqueryhistory") <- dbQueryHistory(
    con = con,
    verbose = FALSE)

  # return annotated object
  return(x)

} # end addMetaData



#' Install necessary helper apps (Windows only)
#'
#' Convenience function to install a minimal Cygwin environment under MS
#' Windows, including perl, cat and sed.
#' Alternatively and in case of difficulties, download and run the cygwin
#' setup yourself as follows: \code{cygwinsetup.exe --no-admin --quiet-mode
#' --verbose --upgrade-also --root c:/cygwin --site
#' https://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages
#' perl}.
#' These binaries are required only for function \link{ctrLoadQueryIntoDb}
#' when used for register "EUCTR",
#' but not for any other register or any other function in this package.
#'
#' @export
#'
#' @param force Set to \code{TRUE} to update a Cygwin environment that
#'   was previously installed with the function, or to overwrite any existing
#'   installation in \code{c:\\cygwin}
#'
#' @param proxy Specify any proxy to be used for downloading via http, e.g.
#'   `host_or_ip:port`; defaults to the environment variable `https_proxy`.
#'   Set to `""` to not specify or to unset a proxy.
#'
#' @examples
#' \dontrun{
#'
#'
#' try(installCygwinWindowsDoInstall(), silent = TRUE)
#'
#' }
installCygwinWindowsDoInstall <- function(
    force = FALSE, proxy = Sys.getenv("https_proxy")) {

  # checks
  if (.Platform$OS.type != "windows") {
    stop("This function is only for MS Windows operating systems.",
         call. = FALSE)
  }
  #
  if (!force && dir.exists("c:\\cygwin")) {
    message("cygwin is already installed in c:\\cygwin. ",
            "To update or re-install, use force = TRUE.")
    # exit function after testing
    return(installCygwinWindowsTest(verbose = TRUE))
  }

  # create R session temporary directory
  tmpfile <- paste0(tempdir(), "/cygwin_inst")
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")

  # generate download url
  tmpurl <- ifelse(
    grepl("x64", utils::win.version()),
    "setup-x86_64.exe",
    "setup-x86.exe")
  tmpurl <- paste0("https://cygwin.org/", tmpurl)

  # inform user
  message("Attempting download of ", tmpurl, " ...")

  # download.file uses the proxy configured in the system
  tmpdl <- try({
    utils::download.file(
      url = tmpurl,
      destfile = dstfile,
      quiet = FALSE,
      mode = "wb")
  }, silent = TRUE)

  # compose setup command
  setupcmd <- paste0(
    dstfile,
    " --no-admin --quiet-mode --upgrade-also --no-shortcuts --root c:/cygwin",
    " --site https://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/",
    " --packages perl ",
    " --local-package-dir ", tmpfile,
    ifelse(nchar(proxy), " --proxy ", ""), proxy
  )

  # check
  if (!file.exists(dstfile) ||
      file.size(dstfile) < (5 * 10 ^ 5) ||
      (inherits(tmpdl, "try-error"))) {
    stop("Failed, please download manually and install with:\n",
         tmpurl, " ", setupcmd, call. = FALSE)
  }

  # execute cygwin setup command
  message("Executing: ", setupcmd)
  system(setupcmd)

  # return cygwin installation test
  return(installCygwinWindowsTest(verbose = TRUE))

}
# end installCygwinWindowsDoInstall



#' Convenience function to test for working cygwin installation
#'
#' @param verbose If \code{TRUE}, prints confirmatory
#'  message (default \code{FALSE})
#'
#' @return Information if Cygwin can be used, \code{TRUE}
#'  or \code{FALSE}, or NULL if not under MS Windows
#'
#' @keywords internal
#' @noRd
#
installCygwinWindowsTest <- function(verbose = FALSE) {
  #
  if (.Platform$OS.type != "windows") {
    message("Function installCygwinWindowsTest() is ",
            "only for MS Windows operating systems.")
    return(invisible(NULL))
  }
  #
  if (checkBinary()) {
    if (verbose) message("cygwin seems to work correctly")
    return(invisible(TRUE))
  } else {
    stop(
      "cygwin is not available, ctrLoadQueryIntoDb() will not work. ",
      "Consider calling ctrdata::installCygwinWindowsDoInstall()",
      call. = FALSE)
  }
}
# end installCygwinWindowsTest



#' Check availability of binaries installed locally
#'
#' @param commandtest Command to be used for testing
#' the availability of a binary, e.g. perl.
#'
#' @param verbose Set to \code{TRUE} to see printed
#' return value of \code{commandtest}
#'
#' @return Logical indicating if executing `commandtest`
#' returned an error or not
#'
#' @keywords internal
#' @noRd
#
checkCommand <- function(commandtest = NULL, verbose = FALSE) {

  # check
  if (is.null(commandtest)) {
    stop("Empty argument: commandtest",
         call. = FALSE)
  }

  # only for windows, add cygwin shell
  if (.Platform$OS.type == "windows") {
    commandtest <- paste0(
      rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
      " --noprofile --norc --noediting -c ",
      shQuote(paste0(
        "PATH=/usr/local/bin:/usr/bin; ",
        commandtest)))
  }
  if (verbose) message(commandtest)

  # test command
  commandresult <- try(
    suppressWarnings(
      system(commandtest,
             intern = TRUE,
             ignore.stderr =
               ifelse(.Platform$OS.type == "windows",
                      FALSE, TRUE))),
    silent = TRUE
  )

  # evaluate command
  commandreturn <- ifelse(
    inherits(commandresult, "try-error") ||
      grepl("error|not found", tolower(paste(commandresult, collapse = " "))) ||
      (!is.null(attr(commandresult, "status")) &&
         (attr(commandresult, "status") != 0)),
    FALSE, TRUE)

  # user info
  if (commandreturn && interactive()) message(". ", appendLF = FALSE)
  if (verbose) print(commandresult)

  # return
  return(commandreturn)
}
# end checkCommand



#' checkBinary
#'
#' @param b Vector of pre-defined binaries to be tested
#'
#' @param verbose Set to \code{TRUE} for more information
#'
#' @keywords internal
#' @noRd
#'
#' @return Logical, \code{TRUE} if all binaries ok
#'
checkBinary <- function(b = NULL, verbose = FALSE) {

  # check actions and user infos
  actionsInfos <- list(
    "notworking" = c("nonexistingbinarytested",
                     "nonexistingbinarytested not found"),
    "sed" = c("echo x | sed s/x/y/",
              "sed not found, ctrLoadQueryIntoDb() will not work "),
    "perl" = c("perl -V:osname",
               "perl not found, ctrLoadQueryIntoDb() will not work ")
  )

  # if input empty, just check all except test
  if (is.null(b)) b <- names(actionsInfos)[-1]

  # do check
  out <- sapply(X = b, function(bi) {

    # check input
    actionsInfo <- actionsInfos[[bi]]
    if (is.null(actionsInfo)) stop("Unknown binary to check: ", bi, call. = FALSE)

    # previously checked and successful?
    checked <- ctrCache(xname = paste0("bin_check_", bi))
    if (!is.null(checked) && checked) return(TRUE)

    # continue to check binary
    ok <- checkCommand(commandtest = actionsInfo[1], verbose = verbose)
    if (!ok) message("\n", actionsInfo[2], appendLF = FALSE)

    # store check to private environment only if successful
    if (ok) ctrCache(xname = paste0("bin_check_", bi), xvalue = ok)

    # return
    return(ok)

  })

  # inform user
  if (!all(out)) message(
    "\nTo install command line binaries needed for the function ",
    "ctrLoadQueryIntoDb() of package ctrdata, see recommendations at ",
    "https://github.com/rfhb/ctrdata#",
    "id_2-external-tools-only-needed-for-euctr",
    "\nAfter installation, detach and load package ctrdata again, ",
    "or restart the R session.\n")

  # return single value since
  # all tests need to be ok
  invisible(all(out))

} # end checkBinary



#' ctrMultiDownload
#'
#' @param urls Vector of urls to be downloaded
#'
#' @param progress Set to \code{FALSE} to not print progress bar
#'
#' @keywords internal
#' @noRd
#'
#' @return Data frame with columns such as status_code etc
#'
#' @importFrom curl multi_download
#' @importFrom utils URLencode
#'
ctrMultiDownload <- function(
    urls,
    destfiles,
    progress = TRUE,
    resume = FALSE,
    verbose = TRUE) {

  stopifnot(length(urls) == length(destfiles))
  if (!length(urls)) return(data.frame())

  # does not error in case any of the individual requests fail
  # inspect the return value to find out which were successful

  toDo <- rep.int(TRUE, times = length(urls))
  numI <- 1L
  canR <- resume

  while (any(toDo) && numI < 5L) {

    args <- c(
      urls = list(utils::URLencode(urls[toDo])),
      destfiles = list(destfiles[toDo]),
      resume = canR,
      progress = progress,
      timeout = Inf,
      multiplex = TRUE,
      c(getOption("httr_config")[["options"]],
        accept_encoding = "gzip,deflate,zstd,br")
    )

    res <- do.call(curl::multi_download, args)

    if (numI == 1L) {
      downloadValue <- res
    } else {
      downloadValue[toDo, , drop = FALSE] <- res
    }

    if (any(grepl(
      "annot resume", downloadValue[toDo, "error", drop = TRUE]))) canR <- FALSE

    if (inherits(downloadValue, "try-error")) {
      stop("Download failed; last error: ", class(downloadValue), call. = FALSE)
    }

    numI <- numI + 1L
    toDo <- is.na(downloadValue[["success"]]) |
      !downloadValue[["success"]] |
      !(downloadValue[["status_code"]] %in% c(200L, 206L, 416L))

  }

  if (any(toDo)) {

    # remove any files from failed downloads
    unlink(downloadValue[toDo, c("destfile"), drop = TRUE])

    if (verbose) {
      message(
        "Download failed for: status code / url(s):"
      )
      apply(
        downloadValue[toDo, c("status_code", "url"), drop = FALSE],
        1, function(r) message(r[1], " / ", r[2], "\n", appendLF = FALSE)
      )
    }

  }

  return(downloadValue[!toDo, , drop = FALSE])

} # end ctrMultiDownload



#' ctrConvertToJSON
#'
#' @param tempDir Name of temporary directory with downloaded
#'  trial information
#' @param scriptName Name of script to run
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return System messages from converting
#'
#' @keywords internal
#' @noRd
#'
ctrConvertToJSON <- function(tempDir, scriptName, verbose) {

  ## compose commands to transform into json
  scriptFile <- system.file(paste0("exec/", scriptName),
                            package = "ctrdata",
                            mustWork = TRUE)

  # special command handling on windows
  if (.Platform$OS.type == "windows") {
    #
    script2Json <- utils::shortPathName(path = scriptFile)
    #
    script2Json <- paste0(
      shQuote(script2Json), " ",
      utils::shortPathName(path = tempDir))
    #
    # transform paths for cygwin use
    script2Json <- gsub("\\\\", "/", script2Json)
    script2Json <- gsub("([A-Z]):/", "/cygdrive/\\1/", script2Json)
    #
    script2Json <- paste0(
      rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
      ' --noprofile --norc --noediting -c ',
      shQuote(paste0(
        "PATH=/usr/local/bin:/usr/bin; ",
        script2Json)))
    #
  } else {
    #
    # platforms other than windows
    #
    script2Json <- system.file(paste0("exec/", scriptName),
                               package = "ctrdata",
                               mustWork = TRUE)
    #
    script2Json <- paste0(
      shQuote(script2Json), " ",
      tempDir)
    #
  } # if windows

  # run conversion of download to json
  imported <- system(script2Json, intern = TRUE)
  message("\b\b\b, ", imported, " records converted")
  if (verbose) message("DEBUG: ", script2Json)

  # return
  return(imported)

} # end ctrConvertToJSON



#' ctrTempDir
#'
#' create empty temporary directory on localhost for
#' downloading from register into temporary directory
#'
#' @return path to existing directory
#'
#' @keywords internal
#' @noRd
#'
ctrTempDir <- function(verbose = FALSE) {

  # get temporary space
  tempDir <- getOption(
    "ctrdata.tempdir",
    default = tempfile(pattern = "ctrDATA"))

  # create and normalise for OS
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)

  # insert on.exit() call into the parent function
  if (!verbose) {
    do.call(
      on.exit,
      list(
        substitute(fun(), list(
          fun = function() unlink(tempDir, recursive = TRUE))),
        add = TRUE),
      envir = parent.frame(2L)
    )
  }

  # inform user
  if (verbose) message("DEBUG: ", tempDir)

  # return
  return(tempDir)

}



#' ctrDocsDownload
#'
#' download documents
#'
#' @param dlFiles data frame with columns _id, filename, url
#' @param documents.path parameter from parent call
#' @param documents.regexp parameter from parent call
#' @param verbose parameter from parent call
#'
#' @return number of documents
#'
#' @keywords internal
#' @noRd
#'
ctrDocsDownload <- function(
    dlFiles,
    documents.path,
    documents.regexp,
    verbose) {

  # check and create directory
  createdDir <- try(
    dir.create(documents.path, recursive = TRUE, showWarnings = FALSE),
    silent = TRUE)

  # early return
  if (inherits(createdDir, "try-errror")) {

    warning("Directory could not be created for 'documents.path' ",
            documents.path, ", cannot download files", call. = FALSE)

    return(0L)
  }

  # continue after if
  message("* Downloading documents into 'documents.path' = ", documents.path)

  # canonical directory path
  documents.path <- normalizePath(documents.path, mustWork = TRUE)
  if (createdDir) message("- Created directory ", documents.path)

  # documents download
  message("- Creating subfolder for each trial")

  # add destination file directory path
  dlFiles$filepath <- file.path(documents.path, dlFiles$`_id`)

  # create subdirectories by trial
  invisible(sapply(
    unique(dlFiles$filepath), function(i) if (!dir.exists(i))
      dir.create(i, showWarnings = FALSE, recursive = TRUE)
  ))

  # check if destination document exists
  dlFiles$filepathname <- file.path(dlFiles$filepath, dlFiles$filename)
  dlFiles$fileexists <- file.exists(dlFiles$filepathname) &
    file.size(dlFiles$filepathname) > 10L

  # placeholder or files
  if (is.null(documents.regexp)) {

    message("- Creating empty document placeholders (max. ", nrow(dlFiles), ")")

    # create empty files
    tmp <-
      sapply(
        dlFiles$filepathname,
        function(i) if (!file.exists(i))
          file.create(i, showWarnings = TRUE),
        USE.NAMES = FALSE)

    tmp <- sum(unlist(tmp), na.rm = TRUE)

  } else {

    # inform
    message("- Applying 'documents.regexp' to ", nrow(dlFiles), " documents")

    # apply regexp
    dlFiles <- dlFiles[
      grepl(documents.regexp, dlFiles$filename, ignore.case = TRUE), ,
      drop = FALSE]

    # inform
    message("- Downloading ",
            nrow(dlFiles[!dlFiles$fileexists, , drop = FALSE]),
            " missing documents")

    # do download
    tmp <- ctrMultiDownload(
      urls = dlFiles$url[!dlFiles$fileexists],
      destfiles = dlFiles$filepathname[!dlFiles$fileexists],
      verbose = verbose)

    # check results
    if (!nrow(tmp)) tmp <- 0L else {

      # handle failures despite success is true
      suppressMessages(invisible(sapply(
        tmp[tmp$status_code != 200L, "destfile", drop = TRUE], unlink
      )))
      tmp <- nrow(tmp[tmp$status_code == 200L, , drop = FALSE])

    }

  } # is.null(documents.regexp)

  # inform user
  message(sprintf(paste0(
    "= Newly saved %i ",
    ifelse(is.null(documents.regexp), "placeholder ", ""),
    "document(s) for %i trial(s); ",
    "%i document(s) for %i trial(s) already existed in %s"),
    tmp,
    length(unique(dlFiles$`_id`)),
    sum(dlFiles$fileexists),
    length(unique(dlFiles$`_id`[dlFiles$fileexists])),
    documents.path
  ))

  # return
  return(tmp)

} # end ctrDocsDownload



#' initTranformers
#'
#' https://cran.r-project.org/web/packages/V8/vignettes/npm.html
#'
#' @importFrom V8 v8 JS
#'
#' @keywords internal
#' @noRd
#'
initTranformers <- function() {

  # prepare V8, see ./inst/js/
  ct <- V8::v8()

  # get javascript
  ct$source(system.file("js/bundle.js", package = "ctrdata"))
  assign("ct", ct, envir = .ctrdataenv)

  # functions for conversions
  ct$assign(
    "parsexml",
    # https://www.npmjs.com/package/xml2js#options
    V8::JS("function(x, y) {injs.parseString(x, y, function (err, result)
           { out = result; }); return JSON.stringify(out); }"))

}



#' dbCTRLoadJSONFiles
#'
#' @param dir Path to local directory with JSON files
#' from downloading and converting
#'
#' @importFrom jsonlite validate
#' @importFrom nodbi docdb_create
#' @importFrom stats na.omit
#' @importFrom jqr jq
#'
#' @inheritParams ctrDb
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return List with elements n (number of imported trials),
#' _id's of successfully imported trials and
#' _id's of trials that failed to import
#'
#' @keywords internal
#' @noRd
#'
dbCTRLoadJSONFiles <- function(dir, con, verbose) {

  # find files
  tempFiles <- dir(path = dir,
                   pattern = "^.+_trials_.*.ndjson$",
                   full.names = TRUE)

  # check
  if (!length(tempFiles)) stop("no .+_trials_.*.ndjson files found in ", dir)

  # initialise counters
  fc <- length(tempFiles)

  ## iterate ndjson files -----------------------------------------------------------------

  retimp <- lapply(
    X = seq_along(tempFiles),
    function(tempFile) {

      ## initialise output
      idSuccess <- NULL
      idFailed <- NULL
      idAnnotation <- NULL
      nImported <- 0
      ids <- NULL

      ## get _id's

      # main function for fast reading,
      # switching off warning about final EOL missing
      fd <- file(description = tempFiles[tempFile],
                 open = "rt", blocking = TRUE)
      on.exit(try(close(fd), silent = TRUE), add = TRUE)

      # inform user
      message(
        "JSON file #: ", tempFile, " / ", fc,
        "                               \r",
        appendLF = FALSE)

      # get all ids using jq, safet than regex
      ids <- gsub("\"", "", as.vector(jqr::jq(file(tempFiles[tempFile]), " ._id ")))

      ## existing annotations -------------------------------------------------

      # get annotations
      annoDf <- try({
        nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), "]}}"),
          fields = '{"_id": 1, "annotation": 1}')
      }, silent = TRUE)
      if (!inherits(annoDf, "try-error") && length(annoDf[["_id"]])) {
        annoDf <- merge(
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE),
          annoDf, all.x = TRUE) # only need input ids, do not need all.y
      } else {
        annoDf <-
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE)
      }
      if (is.null(annoDf[["annotation"]]))
        annoDf[["annotation"]] <- rep(NA, length(ids))

      ## delete and import ----------------------------------------------------

      # delete any existing records
      try({
        nodbi::docdb_delete(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), ']}}'),
          fields = '{"_id": 1}')
      }, silent = TRUE)

      ## import
      tmp <- try({
        suppressWarnings(
          suppressMessages(
            nodbi::docdb_create(
              src = con,
              key = con$collection,
              value = tempFiles[tempFile]
            )))}, silent = TRUE)

      ## return values for lapply
      if (inherits(tmp, "try-error") || tmp == 0L || tmp != nrow(annoDf)) {

        # step into line by line mode
        fdLines <- file(tempFiles[tempFile], open = "rt", blocking = TRUE)
        fLineOut <- tempfile(pattern = "tmpOneLine", tmpdir = dir, fileext = ".ndjson")
        fTmp <- NULL
        while (TRUE) {
          tmpOneLine <- readLines(con = fdLines, n = 1L, warn = FALSE)
          if (length(tmpOneLine) == 0L || !nchar(tmpOneLine)) break
          id <- sub(".*\"_id\":[ ]*\"(.*?)\".*", "\\1", tmpOneLine)
          cat(tmpOneLine, file = fLineOut)
          tmp <- suppressWarnings(suppressMessages(nodbi::docdb_create(
            src = con, key = con$collection, value = fLineOut)))
          nImported <- nImported + tmp
          if (tmp) idSuccess <- c(idSuccess, id)
          if (!tmp) idFailed <- c(idFailed, id)
          if (!tmp) warning("Failed to load: ", id, call. = FALSE)
          if (tmp) idAnnotation <- c(idAnnotation, annoDf[
            annoDf[["_id"]] == id, "annotation", drop = TRUE][1])
        }
        close(fdLines)

      } else {
        nImported <- nImported + tmp
        idSuccess <- c(idSuccess, annoDf[ , "_id", drop = TRUE])
        idAnnotation <- c(idAnnotation, annoDf[ , "annotation", drop = TRUE])
      }

      # close this file
      close(fd)

      # return values
      list(success = idSuccess,
           failed = idFailed,
           n = nImported,
           annotations = idAnnotation)

    }) # sapply tempFiles

  # prepare return values, n is successful only
  n <- sum(sapply(retimp, "[[", "n"), na.rm = TRUE)
  success <- as.vector(unlist(sapply(retimp, "[[", "success")))
  failed <- as.vector(unlist(sapply(retimp, "[[", "failed")))
  annotations <- as.vector(unlist(sapply(retimp, "[[", "annotations")))

  # return
  return(list(n = n,
              success = success,
              failed = failed,
              annotations = annotations))

} # end dbCTRLoadJSONFiles


#' dbQueryAnnotateRecords
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_update
#'
dbCTRAnnotateQueryRecords <- function(
    recordnumbers,
    recordannotations,
    annotation.text,
    annotation.mode,
    con,
    verbose) {

  # debug
  if (verbose) message("Annotating records...")
  if (verbose) message(recordnumbers)
  if (verbose) message(annotation.mode)

  # df from existing annotations
  if (is.null(recordannotations)) recordannotations <- ""
  annotations <- data.frame(
    "_id" = recordnumbers,
    "annotation" = recordannotations,
    stringsAsFactors = FALSE,
    check.names = FALSE)

  # check if dataframe is as expected: columns _id and annotation
  # dataframe could be empty if _ids not yet imported
  if (nrow(annotations) == 0) {
    annotations <- data.frame("_id" = recordnumbers,
                              "annotation" = "",
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  }

  # modify the annotations
  annotations[["annotation"]] <- trimws(
    switch(
      annotation.mode,
      "replace" = paste0(annotation.text),
      "prepend" = paste0(annotation.text, " ", ifelse(
        is.na(annotations[["annotation"]]), "", annotations[["annotation"]])),
      paste0(ifelse(is.na(annotations[["annotation"]]), "", annotations[["annotation"]]),
             " ", annotation.text)
    ))

  # ensure columns including order
  annotations <- annotations[, c("_id", "annotation"), drop = FALSE]

  # debug
  if (verbose) message(annotations)

  # update the database
  result <- nodbi::docdb_update(
    src = con,
    key = con$collection,
    value = annotations,
    query = "")

  # inform user
  message("= Annotated retrieved records (", result, " records)")

} # end dbCTRAnnotateQueryRecords


#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_delete docdb_create docdb_update
#'
dbCTRUpdateQueryHistory <- function(
    register,
    queryterm,
    recordnumber,
    con,
    verbose) {

  ## check database connection
  con <- ctrDb(con)

  # debug
  if (verbose) message("Running dbCTRUpdateQueryHistory...")

  # compose history entry from current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  newHist <- data.frame(
    "query-timestamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "query-register"  = register,
    "query-records"   = recordnumber,
    "query-term"      = queryterm,
    check.names = FALSE,
    stringsAsFactors = FALSE)

  # retrieve existing history data
  hist <- dbQueryHistory(con, verbose)

  # append current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  if (nrow(hist)) {

    newHist <- rbind(hist, newHist)
    newHist <- list("queries" = newHist)

    tmp <- suppressMessages(
      nodbi::docdb_update(
        src = con,
        key = con$collection,
        value = newHist,
        query = '{"_id": "meta-info"}'
      ))

  } else {

    # to list
    newHist <- list(list(
      "_id" = "meta-info",
      "queries" = newHist))

    # write new document
    tmp <- suppressMessages(
      nodbi::docdb_create(
        src = con,
        key = con$collection,
        value = newHist
      ))
  }

  # inform user
  if (tmp == 1L) {
    message('Updated history ("meta-info" in "', con$collection, '")')
  } else {
    warning('Could not update history ("meta-info" in "', con$collection,
            '")', call. = FALSE, immediate. = FALSE)
  }
} # end dbCTRUpdateQueryHistory
