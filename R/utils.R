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
regIsrctn <- "[0-9][0-9]{7}"
# - CTIS e.g. 2022-501549-57-00
regCtis <- "[0-9]{4}-[0-9]{6}-[0-9]{2}-[0-9]{2}"
#
# register list
registerList <- c("EUCTR", "CTGOV", "ISRCTN", "CTIS", "CTGOV2")
#
# mapping field names to typing function for typeField()
typeVars <- list(
  #
  #### . dates ####
  #
  # - ctrdata intern
  "record_last_import" = "ctrDate",
  #
  # - EUCTR
  "n_date_of_competent_authority_decision" = "ctrDate",
  "n_date_of_ethics_committee_opinion"     = "ctrDate",
  "p_date_of_the_global_end_of_the_trial"  = "ctrDate",
  "firstreceived_results_date"             = "ctrDate",
  "trialInformation.primaryCompletionDate" = "ctrDate",
  "trialInformation.analysisStageDate"     = "ctrDate",
  "trialInformation.globalEndOfTrialDate"  = "ctrDate",
  "trialInformation.recruitmentStartDate"  = "ctrDate",
  "trialChanges.globalAmendments.globalAmendment.date" = "ctrDate",
  "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database" = "ctrDate",
  #
  "e891_in_the_member_state_concerned_days"   = "ctrDifftimeDays",
  "e891_in_the_member_state_concerned_months" = "ctrDifftimeMonths",
  "e891_in_the_member_state_concerned_years"  = "ctrDifftimeYears",
  "e892_in_all_countries_concerned_by_the_trial_days"   = "ctrDifftimeDays",
  "e892_in_all_countries_concerned_by_the_trial_months" = "ctrDifftimeMonths",
  "e892_in_all_countries_concerned_by_the_trial_years"  = "ctrDifftimeYears",
  #
  # - CTGOV
  "completion_date"            = "ctrDateUs",
  "eligibility.minimum_age"    = "ctrDifftime",
  "eligibility.maximum_age"    = "ctrDifftime",
  "last_update_posted"         = "ctrDateUs",
  "last_update_submitted_qc"   = "ctrDateUs",
  "last_update_submitted"      = "ctrDateUs",
  "primary_completion_date"    = "ctrDateUs",
  "provided_document_section.provided_document.document_date" = "ctrDateUs",
  "required_header.download_date" = "ctrDateUs",
  "results_first_posted"       = "ctrDateUs",
  "results_first_submitted_qc" = "ctrDateUs",
  "start_date"                 = "ctrDateUs",
  "this_results_date"          = "ctrDateUs",
  "study_first_posted"         = "ctrDateUs",
  "verification_date"          = "ctrDateUs",
  #
  # - CTGOV2
  "annotationSection.annotationModule.unpostedAnnotation.unpostedEvents.date"         = "ctrDate",
  "derivedSection.miscInfoModule.submissionTracking.estimatedResultsFirstSubmitDate"  = "ctrDate",
  "derivedSection.miscInfoModule.submissionTracking.firstMcpInfo.postDateStruct.date" = "ctrDate",
  "derivedSection.miscInfoModule.submissionTracking.submissionInfos.releaseDate"      = "ctrDate",
  "derivedSection.miscInfoModule.submissionTracking.submissionInfos.resetDate"        = "ctrDate",
  "derivedSection.miscInfoModule.submissionTracking.submissionInfos.unreleaseDate"    = "ctrDate",
  "documentSection.largeDocumentModule.largeDocs.date"       = "ctrDate",
  "documentSection.largeDocumentModule.largeDocs.uploadDate" = "ctrDate",
  "history.documentSection.largeDocumentModule.largeDocs.date"            = "ctrDate",
  "history.documentSection.largeDocumentModule.largeDocs.uploadDate"      = "ctrDate",
  "history.history_version.version_date"                                  = "ctrDate",
  "history.protocolSection.statusModule.completionDateStruct.date"        = "ctrDate",
  "history.protocolSection.statusModule.dispFirstPostDateStruct.date"     = "ctrDate",
  "history.protocolSection.statusModule.dispFirstSubmitDate"              = "ctrDate",
  "history.protocolSection.statusModule.dispFirstSubmitQcDate"            = "ctrDate",
  "history.protocolSection.statusModule.lastUpdatePostDateStruct.date"    = "ctrDate",
  "history.protocolSection.statusModule.lastUpdateSubmitDate"             = "ctrDate",
  "history.protocolSection.statusModule.primaryCompletionDateStruct.date" = "ctrDate",
  "history.protocolSection.statusModule.resultsFirstPostDateStruct.date"  = "ctrDate",
  "history.protocolSection.statusModule.resultsFirstSubmitDate"           = "ctrDate",
  "history.protocolSection.statusModule.resultsFirstSubmitQcDate"         = "ctrDate",
  "history.protocolSection.statusModule.startDateStruct.date"             = "ctrDate",
  "history.protocolSection.statusModule.statusVerifiedDate"               = "ctrDate",
  "history.protocolSection.statusModule.studyFirstPostDateStruct.date"    = "ctrDate",
  "history.protocolSection.statusModule.studyFirstSubmitDate"             = "ctrDate",
  "history.protocolSection.statusModule.studyFirstSubmitQcDate"           = "ctrDate",
  "protocolSection.statusModule.completionDateStruct.date"        = "ctrDate",
  "protocolSection.statusModule.dispFirstPostDateStruct.date"     = "ctrDate",
  "protocolSection.statusModule.dispFirstSubmitDate"              = "ctrDate",
  "protocolSection.statusModule.dispFirstSubmitQcDate"            = "ctrDate",
  "protocolSection.statusModule.lastUpdatePostDateStruct.date"    = "ctrDate",
  "protocolSection.statusModule.lastUpdateSubmitDate"             = "ctrDate",
  "protocolSection.statusModule.primaryCompletionDateStruct.date" = "ctrDate",
  "protocolSection.statusModule.resultsFirstPostDateStruct.date"  = "ctrDate",
  "protocolSection.statusModule.resultsFirstSubmitQcDate"         = "ctrDate",
  "protocolSection.statusModule.resultsFirstSubmitDate"           = "ctrDate",
  "protocolSection.statusModule.startDateStruct.date"             = "ctrDate",
  "protocolSection.statusModule.statusVerifiedDate"               = "ctrDate",
  "protocolSection.statusModule.studyFirstPostDateStruct.date"    = "ctrDate",
  "protocolSection.statusModule.studyFirstSubmitDate"             = "ctrDate",
  "protocolSection.statusModule.studyFirstSubmitQcDate"           = "ctrDate",
  #
  # - ISRCTN
  "participants.recruitmentStart" = "ctrDateTime",
  "participants.recruitmentEnd"   = "ctrDateTime",
  "trialDesign.overallStartDate"  = "ctrDateTime",
  "trialDesign.overallEndDate"    = "ctrDateTime",
  #
  # - CTIS
  #
  "applications.ctMSCs.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.ctMSCs.activeTrialPeriod.trialEndDate" = "ctrDate",
  "applications.ctMSCs.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.ctMSCs.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.ctMSCs.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.ctMSCs.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "applications.ctMSCs.firstDecisionDate" = "ctrDate",
  "applications.ctMSCs.fromDate" = "ctrDate",
  "applications.ctMSCs.temporaryHaltDate" = "ctrDate",
  "applications.ctMSCs.toDate" = "ctrDate",
  "applications.ctMSCs.trialPeriod.fromDate" = "ctrDate",
  "applications.ctMSCs.trialPeriod.trialEndDate" = "ctrDate",
  "applications.ctMSCs.trialPeriod.trialStartDate" = "ctrDate",
  "applications.ctMSCs.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "applications.ctMSCs.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.ctMSCs.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.ctMSCsByApplication.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.ctMSCsByApplication.activeTrialPeriod.trialEndDate" = "ctrDate",
  "applications.ctMSCsByApplication.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.ctMSCsByApplication.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.ctMSCsByApplication.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.ctMSCsByApplication.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "applications.ctMSCsByApplication.firstDecisionDate" = "ctrDate",
  "applications.ctMSCsByApplication.fromDate" = "ctrDate",
  "applications.ctMSCsByApplication.rmsNotWillingDate" = "ctrDate",
  "applications.ctMSCsByApplication.rmsWillingDate" = "ctrDate",
  "applications.ctMSCsByApplication.temporaryHaltDate" = "ctrDate",
  "applications.ctMSCsByApplication.toDate" = "ctrDate",
  "applications.ctMSCsByApplication.trialPeriod.fromDate" = "ctrDate",
  "applications.ctMSCsByApplication.trialPeriod.trialEndDate" = "ctrDate",
  "applications.ctMSCsByApplication.trialPeriod.trialStartDate" = "ctrDate",
  "applications.ctMSCsByApplication.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "applications.ctMSCsByApplication.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.ctMSCsByApplication.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.decisionDate" = "ctrDate",
  "applications.partI.assessmentOutcomeDate" = "ctrDate",
  "applications.partI.conclusionDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.linkedProducts.startDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.firstDecisionDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.fromDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialStartDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.products.startDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.firstDecisionDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.fromDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialStartDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.partI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.partI.products.startDate" = "ctrDate",
  "applications.partI.sponsors.fromDate" = "ctrDate",
  "applications.partI.submissionDate" = "ctrDate",
  "applications.partI.trialDetails.trialInformation.trialDuration.estimatedEndDate" = "ctrDate",
  "applications.partI.trialDetails.trialInformation.trialDuration.estimatedGlobalEndDate" = "ctrDate",
  "applications.partI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate" = "ctrDate",
  "applications.partIIInfo.applicationStatusDate" = "ctrDate",
  "applications.partIIInfo.conclusionDate" = "ctrDate",
  "applications.partIIInfo.lapsedDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialPeriod.trialEndDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.firstDecisionDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.temporaryHaltDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.toDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialPeriod.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialPeriod.trialEndDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialPeriod.trialStartDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "applications.partIIInfo.mscInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "applications.partIIInfo.submissionDate" = "ctrDate",
  "applications.primarySponsor.fromDate" = "ctrDate",
  "applications.reportingDate" = "ctrDate",
  "applications.submissionDate" = "ctrDate",
  "applications.validationConclusionDate" = "ctrDate",
  "authorizationDate" = "ctrDate",
  "authorizedPartI.assessmentOutcomeDate" = "ctrDate",
  "authorizedPartI.conclusionDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.linkedProducts.startDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.firstDecisionDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.products.startDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.firstDecisionDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedPartI.products.startDate" = "ctrDate",
  "authorizedPartI.sponsors.fromDate" = "ctrDate",
  "authorizedPartI.submissionDate" = "ctrDate",
  "authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedEndDate" = "ctrDate",
  "authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedGlobalEndDate" = "ctrDate",
  "authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate" = "ctrDate",
  "authorizedPartsII.applicationStatusDate" = "ctrDate",
  "authorizedPartsII.conclusionDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialPeriod.trialEndDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedPartsII.mscInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedPartsII.mscInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "authorizedPartsII.mscInfo.firstDecisionDate" = "ctrDate",
  "authorizedPartsII.mscInfo.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.temporaryHaltDate" = "ctrDate",
  "authorizedPartsII.mscInfo.toDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialPeriod.trialEndDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedPartsII.mscInfo.trialRestartDate" = "ctrDate",
  "authorizedPartsII.submissionDate" = "ctrDate",
  "coSponsors.fromDate" = "ctrDate",
  "eeaEndDate" = "ctrDate",
  "eeaStartDate" = "ctrDate",
  "layperson.submissionDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.fromDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.trialEndDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.trialStartDate" = "ctrDate",
  "memberStatesConcerned.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "memberStatesConcerned.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "memberStatesConcerned.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "memberStatesConcerned.firstDecisionDate" = "ctrDate",
  "memberStatesConcerned.fromDate" = "ctrDate",
  "memberStatesConcerned.temporaryHaltDate" = "ctrDate",
  "memberStatesConcerned.toDate" = "ctrDate",
  "memberStatesConcerned.trialPeriod.fromDate" = "ctrDate",
  "memberStatesConcerned.trialPeriod.trialEndDate" = "ctrDate",
  "memberStatesConcerned.trialPeriod.trialStartDate" = "ctrDate",
  "memberStatesConcerned.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "memberStatesConcerned.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "memberStatesConcerned.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "memberStatesConcerned.trialRestartDate" = "ctrDate",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.date" = "ctrDate",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.submitDate" = "ctrDate",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.versions.versionDate" = "ctrDate",
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
  "publicevents.temporaryHaltList.details.eventDate" = "ctrDate",
  "publicevents.temporaryHaltList.details.plannedRestartDate" = "ctrDate",
  "publicevents.temporaryHaltList.details.submissionDate" = "ctrDate",
  "publicevents.temporaryHaltList.submissionDate" = "ctrDate",
  "recruitmentStartDate" = "ctrDate",
  "submissionDate" = "ctrDate",
  "summary.submissionDate" = "ctrDate",
  "trialEndDate" = "ctrDate",
  "trialStartDate" = "ctrDate",
  #
  #
  #### . factors / logical ####
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
  "includesPaediatricSubjects" = "ctrFalseTrue",
  "hasDeferrallApplied" = "ctrFalseTrue",
  "hasAmendmentApplied" = "ctrFalseTrue",
  #
  "baselineCharacteristics.ageCategoricalCharacteristic.readyForValues" = "ctrFalseTrue",
  "baselineCharacteristics.ageContinuousCharacteristic.readyForValues" = "ctrFalseTrue",
  "baselineCharacteristics.genderCategoricalCharacteristic.readyForValues" = "ctrFalseTrue",
  "baselineCharacteristics.studyCategoricalCharacteristics.studyCategoricalCharacteristic.readyForValues" = "ctrFalseTrue",
  "endPoints.endPoint.readyForValues" = "ctrFalseTrue",
  #
  "trialChanges.hasGlobalAmendments" = "ctrFalseTrue",
  "trialChanges.hasGlobalInterruptions" = "ctrFalseTrue",
  "oversight_info.has_dmc" = "ctrFalseTrue",
  "applications.ctMSCsByApplication.hasRecruitmentStarted" = "ctrFalseTrue",
  "applications.ctMSCs.hasRecruitmentStarted" = "ctrFalseTrue",
  "applications.eudraCtInfo.hasVhp" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.hasDevice" = "ctrFalseTrue",
  "applications.partI.products.hasDevice" = "ctrFalseTrue",
  "applications.partI.trialDetails.associatedClinicalTrials.hasDocument" = "ctrFalseTrue",
  "applications.partIIInfo.mscInfo.hasRecruitmentStarted" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.hasDevice" = "ctrFalseTrue",
  "authorizedPartI.products.hasDevice" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.associatedClinicalTrials.hasDocument" = "ctrFalseTrue",
  "authorizedPartsII.mscInfo.hasRecruitmentStarted" = "ctrFalseTrue",
  "eudraCtInfo.hasVhp" = "ctrFalseTrue",
  "memberStatesConcerned.hasRecruitmentStarted" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfis.hasApplicationChanges" = "ctrFalseTrue",
  "publicEvaluation.validationRfis.hasApplicationChanges" = "ctrFalseTrue",
  "publicEvaluation.partIRfis.hasApplicationChanges" = "ctrFalseTrue",
  "protocolSection.statusModule.expandedAccessInfo.hasExpandedAccess" = "ctrFalseTrue",
  #
  "trialInformation.isGlobalEndOfTrialReached" = "ctrFalseTrue",
  "applications.ctMSCsByApplication.isProposedRms" = "ctrFalseTrue",
  "applications.ctMSCsByApplication.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.ctMSCs.isProposedRms" = "ctrFalseTrue",
  "applications.ctMSCs.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.eudraCtInfo.isTransitioned" = "ctrFalseTrue",
  "applications.isDossierUpdate" = "ctrFalseTrue",
  "applications.isMultiTrialSM" = "ctrFalseTrue",
  "applications.partI.isEditable" = "ctrFalseTrue",
  "applications.partI.isLowIntervention" = "ctrFalseTrue",
  "applications.partI.medicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.isDraftUnauthProduct" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.isPaediatricFormulation" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.productDictionaryInfo.isSelectedInMultiTrialSM" = "ctrFalseTrue",
  "applications.partI.products.isDraftUnauthProduct" = "ctrFalseTrue",
  "applications.partI.products.isPaediatricFormulation" = "ctrFalseTrue",
  "applications.partI.products.productDictionaryInfo.isSelectedInMultiTrialSM" = "ctrFalseTrue",
  "applications.partI.sponsors.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.addresses.isCreate" = "ctrFalseTrue",
  "applications.partI.sponsors.addresses.isSave" = "ctrFalseTrue",
  "applications.partI.sponsors.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.contactPointForUnion.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.isCommercial" = "ctrFalseTrue",
  "applications.partI.sponsors.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.thirdParties.organisationAddress.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.sponsors.thirdParties.organisationAddress.isCreate" = "ctrFalseTrue",
  "applications.partI.sponsors.thirdParties.organisationAddress.isSave" = "ctrFalseTrue",
  "applications.partI.sponsors.thirdParties.organisationAddress.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partI.trialDetails.isClockStopRemovalRequested" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.endPoint.secondaryEndPoints.isPrimary" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.medicalCondition.meddraConditionTerms.isActive" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.medicalCondition.partIMedicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.populationOfTrialSubjects.isFemaleSubjects" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.populationOfTrialSubjects.isMaleSubjects" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.populationOfTrialSubjects.isVulnerablePopulationSelected" = "ctrFalseTrue",
  "applications.partI.trialDetails.trialInformation.trialCategory.isLowIntervention" = "ctrFalseTrue",
  "applications.partIIInfo.isEditable" = "ctrFalseTrue",
  "applications.partIIInfo.mscInfo.isProposedRms" = "ctrFalseTrue",
  "applications.partIIInfo.mscInfo.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partIIInfo.trialSites.organisationAddressInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.partIIInfo.trialSites.organisationAddressInfo.isCreate" = "ctrFalseTrue",
  "applications.partIIInfo.trialSites.organisationAddressInfo.isSave" = "ctrFalseTrue",
  "applications.partIIInfo.trialSites.organisationAddressInfo.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.addresses.isCreate" = "ctrFalseTrue",
  "applications.primarySponsor.addresses.isSave" = "ctrFalseTrue",
  "applications.primarySponsor.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.contactPointForUnion.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.isCommercial" = "ctrFalseTrue",
  "applications.primarySponsor.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.thirdParties.organisationAddress.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.primarySponsor.thirdParties.organisationAddress.isCreate" = "ctrFalseTrue",
  "applications.primarySponsor.thirdParties.organisationAddress.isSave" = "ctrFalseTrue",
  "applications.primarySponsor.thirdParties.organisationAddress.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.isEditable" = "ctrFalseTrue",
  "authorizedPartI.isLowIntervention" = "ctrFalseTrue",
  "authorizedPartI.medicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.isDraftUnauthProduct" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.isPaediatricFormulation" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.productDictionaryInfo.isSelectedInMultiTrialSM" = "ctrFalseTrue",
  "authorizedPartI.products.isDraftUnauthProduct" = "ctrFalseTrue",
  "authorizedPartI.products.isPaediatricFormulation" = "ctrFalseTrue",
  "authorizedPartI.products.productDictionaryInfo.isSelectedInMultiTrialSM" = "ctrFalseTrue",
  "authorizedPartI.sponsors.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.addresses.isCreate" = "ctrFalseTrue",
  "authorizedPartI.sponsors.addresses.isSave" = "ctrFalseTrue",
  "authorizedPartI.sponsors.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.contactPointForUnion.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.isCommercial" = "ctrFalseTrue",
  "authorizedPartI.sponsors.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.thirdParties.organisationAddress.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.sponsors.thirdParties.organisationAddress.isCreate" = "ctrFalseTrue",
  "authorizedPartI.sponsors.thirdParties.organisationAddress.isSave" = "ctrFalseTrue",
  "authorizedPartI.sponsors.thirdParties.organisationAddress.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.isClockStopRemovalRequested" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.endPoint.secondaryEndPoints.isPrimary" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.medicalCondition.meddraConditionTerms.isActive" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.medicalCondition.partIMedicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.isFemaleSubjects" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.isMaleSubjects" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.isVulnerablePopulationSelected" = "ctrFalseTrue",
  "authorizedPartI.trialDetails.trialInformation.trialCategory.isLowIntervention" = "ctrFalseTrue",
  "authorizedPartsII.isEditable" = "ctrFalseTrue",
  "authorizedPartsII.mscInfo.isProposedRms" = "ctrFalseTrue",
  "authorizedPartsII.mscInfo.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.isCreate" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.isSave" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.addresses.isCreate" = "ctrFalseTrue",
  "coSponsors.addresses.isSave" = "ctrFalseTrue",
  "coSponsors.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.contactPointForUnion.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.isCommercial" = "ctrFalseTrue",
  "coSponsors.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.thirdParties.organisationAddress.isBusinessKeyValidated" = "ctrFalseTrue",
  "coSponsors.thirdParties.organisationAddress.isCreate" = "ctrFalseTrue",
  "coSponsors.thirdParties.organisationAddress.isSave" = "ctrFalseTrue",
  "coSponsors.thirdParties.organisationAddress.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "memberStatesConcerned.isProposedRms" = "ctrFalseTrue",
  "memberStatesConcerned.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.isBenefitRisckBalanceChange" = "ctrFalseTrue",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.isPublished" = "ctrFalseTrue",
  "primarySponsor.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.addresses.isCreate" = "ctrFalseTrue",
  "primarySponsor.addresses.isSave" = "ctrFalseTrue",
  "primarySponsor.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.contactPointForUnion.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.isCommercial" = "ctrFalseTrue",
  "primarySponsor.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfiConsiderations.rfiConsiderations.isDeConsolidated" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfis.isDueInLessThanTwoDays" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfis.isEditingApplication" = "ctrFalseTrue",
  "publicEvaluation.validationRfiConsiderations.rfiConsiderations.isDeConsolidated" = "ctrFalseTrue",
  "publicEvaluation.validationRfis.isDueInLessThanTwoDays" = "ctrFalseTrue",
  "publicEvaluation.validationRfis.isEditingApplication" = "ctrFalseTrue",
  "publicEvaluation.partIRfiConsiderations.rfiConsiderations.isDeConsolidated" = "ctrFalseTrue",
  "publicEvaluation.partIRfis.isDueInLessThanTwoDays" = "ctrFalseTrue",
  "publicEvaluation.partIRfis.isEditingApplication" = "ctrFalseTrue",
  #
  #### . numbers ####
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
  "trialInformation.countrySubjectCounts.countrySubjectCount.subjects" = "ctrInt",
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
  "rank"           = "ctrInt",
  "clinical_results.baseline.analyzed_list.analyzed.count_list.count.value" = "ctrInt",
  "clinical_results.baseline.measure_list.measure.class_list.class.analyzed_list.analyzed.count_list.count.value" = "ctrInt",
  "clinical_results.outcome_list.outcome.measure.analyzed_list.analyzed.count_list.count.value" = "ctrInt",
  "clinical_results.outcome_list.outcome.measure.class_list.class.analyzed_list.analyzed.count_list.count.value" = "ctrInt",
  #
  # - ISRCTN
  "participants.targetEnrolment"     = "ctrInt",
  "participants.totalFinalEnrolment" = "ctrInt",
  #
  # - CTIS
  "totalNumberEnrolled"     = "ctrInt",
  "totalPartIISubjectCount" = "ctrInt",
  "authorizedPartI.rowSubjectCount" = "ctrInt",
  "applications.mscExtendedInfoList.numberSubjects" = "ctrInt",
  "applications.partIIInfo.recruitmentSubjectCount" = "ctrInt",
  "applications.partI.rowSubjectCount" = "ctrInt",
  "applications.ctMSCs.recruitmentSubjectCount" = "ctrInt",
  "applications.ctMSCsByApplication.recruitmentSubjectCount" = "ctrInt",
  #
  "applications.partI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.number" = "ctrInt",
  "applications.partI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.number" = "ctrInt",
  "applications.partI.trialDetails.trialInformation.endPoint.primaryEndPoints.number"          = "ctrInt",
  "applications.partI.trialDetails.trialInformation.endPoint.secondaryEndPoints.number"        = "ctrInt",
  "applications.partI.trialDetails.trialInformation.trialObjective.secondaryObjectives.number" = "ctrInt",
  "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.number" = "ctrInt",
  "authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.number" = "ctrInt",
  "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.number"          = "ctrInt",
  "authorizedPartI.trialDetails.trialInformation.endPoint.secondaryEndPoints.number"        = "ctrInt",
  "authorizedPartI.trialDetails.trialInformation.trialObjective.secondaryObjectives.number" = "ctrInt",
  #
  # - CTGOV2
  "history.history_version.version_number" = "ctrInt",
  "protocolSection.designModule.enrollmentInfo.count" = "ctrInt",
  "resultsSection.baselineCharacteristicsModule.denoms.counts.value" = "ctrInt",
  "resultsSection.outcomeMeasuresModule.outcomeMeasures.denoms.counts.value" = "ctrInt"
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
    message("* Appears specific for CTGOV Classic website")
    return("CTGOV")
  }

  # logic 2
  if (grepl(paste0(
    # clear identifiers of CTGOV2
    "aggFilters|clinicaltrials[.]gov/(search|study)[/?]|",
    "[:][^/]|%3[aA]"), url)) {
    message("* Appears specific for CTGOV REST API 2.0")
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
    if (verbose) message("- Wrote ", xname, " to cache ")
    return(xvalue)
  }

  # check and read any value for xname variable
  if (verbose) message("- Checking cache...")
  if (exists(x = xname, envir = .ctrdataenv)) {
    tmp <- try(get(x = xname, envir = .ctrdataenv), silent = TRUE)
    if (inherits(tmp, "try-error")) return(NULL)
    if (verbose) message("- Returning ", xname, " ")
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
#' @importFrom lubridate duration ymd_hms dyears dmonths ddays
#'
#' @keywords internal
#' @noRd
#'
typeField <- function(dv, fn) {

  # get function name
  ft <- typeVars[[fn]]

  # expand to function
  if (!is.null(ft)) ft <- switch(
    typeVars[[fn]],
    "ctrInt" = 'as.integer(x = x)',
    "ctrYesNo" = 'sapply(x, function(i) if (is.na(i)) NA else
       switch(i, "Yes" = TRUE, "No" = FALSE, NA), simplify = TRUE)',
    "ctrFalseTrue" = 'if (is.numeric(x)) as.logical(x) else
       sapply(x, function(i) switch(i, "true" = TRUE, "false" = FALSE, NA))',
    "ctrDate" = 'as.Date(x, tryFormats =
       c("%Y-%m-%d", "%Y-%m", "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z"))',
    "ctrDateUs" = 'as.Date(x, tryFormats = c("%b %e, %Y", "%Y-%m-%d", "%Y-%m"))',
    "ctrDateTime" = 'lubridate::ymd_hms(x)',
    "ctrDifftime" = 'as.difftime(as.numeric(lubridate::duration(
       tolower(x)), units = "days"), units = "days")',
    "ctrDifftimeDays" = 'lubridate::ddays(x = as.numeric(x))',
    "ctrDifftimeMonths" = 'lubridate::dmonths(x = as.numeric(x))',
    "ctrDifftimeYears" = 'lubridate::dyears(x = as.numeric(x))',
    NULL
  )

  # clean up text
  if (is.null(ft)) {

    # - if NA as string, change to NA
    dv[grepl("^N/?A$|^ND$", dv)] <- NA

    # - check if any html entities
    htmlEnt <- grepl("&[#a-zA-Z]+;", dv)

    # - convert html entities to text and symbols
    if (any(htmlEnt) && all(sapply(dv, typeof) == "character")) {
      dv[htmlEnt] <-
        lapply(dv[htmlEnt], function(i)
          sapply(i, function(ii)
            xml2::xml_text(xml2::read_html(charToRaw(ii))),
            USE.NAMES = FALSE))
    }

    # - check if possible and convert to numeric
    if (all(is.numeric(dv) | is.na(dv))) dv <- as.numeric(dv)

    # - collapse unless list structure is heterogenous
    rowN1 <- sapply(dv, function(i) is.null(names(i)))
    rowN2 <- sapply(names(rowN1), function(i) is.null(i))
    rowType <- sapply(dv, function(i) typeof(unlist(i, recursive = FALSE)))
    #
    if (all(rowN1) &&
        all(rowN2) &&
        length(unique(rowN1)) <= 1L &&
        any(rowType == "character")) {
      #
      dv <- sapply(dv, function(i) {
        i <- gsub("\r", "\n", i)
        i <- sub("^Information not present in EudraCT", "", i)
        if (length(i) > 1L) {
          rowI <- paste0(i[!is.na(i)], collapse = " / ")
          if (nchar(rowI)) rowI else NA
        } else if (length(i) && !is.na(i)) i else NA
      })
    }

    # early return
    return(dv)

  }

  # early exit if already date or logical
  if (all(sapply(dv, class) %in%
          c("logical", "Date", "POSIXct", "POSIXt"))) return(dv)

  # record length of input dv for NULL handling
  lenDv <- length(dv)

  # apply typing function, returning
  # if possible a vector over list
  tryCatch(
    expr = {
      dv <- lapply(dv, function(x) {
        # - text mangling
        x <- ifelse(grepl("Information not present in EudraCT", x), NA, x)
        # - give Month Year a Day to allow conversion
        if (grepl("date", fn, ignore.case = TRUE)) {
          x <- sub("^ClinicalTrials.gov processed this data on ", "", x)
          x <- sub("^([a-zA-Z]+) ([0-9]{4})$", "\\1 15, \\2", x)
          x <- sub("^([0-9]{4}-[0-9]{2})$", "\\1-15", x)
        }
        # - apply function to x
        eval(parse(text = ft))
      })
    },
    error = function(e) {
      message(fn, ": returning untyped values, as ",
              ft, " raised an error when applied to ",
              paste0(unlist(dv), collapse = " / "))
      return(dv)
    },
    warning = function(w) {
      message(fn, ": returning untyped values, as ",
              ft, " raised a warning when applied to ",
              paste0(unlist(dv), collapse = " / "))
      return(dv)
    }
  )

  # exceptional case inform user
  if (is.null(dv)) {
    warning(paste0(
      fn, " could not be typed, please report here: ",
      "https://github.com/rfhb/ctrdata/issues"))
    dv <- rep_len(NA, lenDv)
  }

  # make original classes (e.g., Date) reappear
  if (!is.list(dv)) dv <- as.list(dv)
  if (all(sapply(dv, length) <= 1L)) {
    return(do.call("c", dv))}

  # return
  return(dv)

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

  # starting values
  numI <- 1L
  canR <- resume

  # do not again download files that already exist
  # or that do not have an (arbitrary) minimal size.
  # nchar("Request failed.") is 15L
  toDo <- rep.int(TRUE, times = length(urls))
  toDo[file.exists(destfiles) &
         (is.na(file.size(destfiles)) |
            file.size(destfiles) > 20L)] <- FALSE

  downloadValue <- data.frame(
    "success" = !toDo,
    "status_code" = rep.int(200L, length(toDo)),
    "resumefrom" = double(length(toDo)),
    "url" = urls,
    "destfile" = destfiles,
    "error" = character(length(toDo)),
    "type" = character(length(toDo)),
    "modified" = double(length(toDo)),
    "time" = double(length(toDo)),
    "headers" = character(length(toDo))
  )

  # does not error in case any of the individual requests fail
  # inspect the return value to find out which were successful
  # make no more than 3 attempts to complete downloading
  while (any(toDo) && numI < 3L) {

    args <- c(
      urls = list(utils::URLencode(downloadValue[toDo, "url", drop = TRUE])),
      destfiles = list(downloadValue[toDo, "destfile", drop = TRUE]),
      resume = canR,
      progress = progress,
      timeout = Inf,
      multiplex = TRUE,
      c(getOption("httr_config")[["options"]],
        accept_encoding = "gzip,deflate,zstd,br")
    )

    res <- do.call(curl::multi_download, args)

    downloadValue[toDo, ] <- res

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

  # retain tempdir for session to accelerate,
  # but only if session is user-interactive.
  # from ctrdata 1.16.0.9000 onwards, all
  # intermediate files are deleted before
  # finalising a ctrLoadQueryIntoDb() call
  # (that is, only downloaded files are kept).
  if (interactive()) options(ctrdata.tempdir = tempDir)

  # register deleting tempDir when exiting session
  assign("keeptempdir", verbose, envir = .ctrdataenv)
  delCtrdataTempDir <- function(x) {
    if (length(.ctrdataenv$keeptempdir) &&
        !is.null(.ctrdataenv$keeptempdir)) {
      if (.ctrdataenv$keeptempdir) {
        message("Since 'verbose = TRUE', not deleting ctrdata.tempdir ", tempDir)
      } else {
        try(unlink(tempDir, recursive = TRUE), silent = TRUE)
        message("...deleted ctrdata.tempdir\r")
      }
    }
    assign("keeptempdir", NULL, envir = .ctrdataenv)
  }
  reg.finalizer(
    e = .ctrdataenv,
    f = delCtrdataTempDir,
    onexit = TRUE
  )

  # inform user
  if (verbose) message(
    "\nDEBUG: ", tempDir,
    "\nUsing any previously downloaded files of the ",
    length(dir(path = tempDir)),
    " files existing in this folder.\n")

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
        tmp[tmp$status_code != 200L, "destfile", drop = TRUE],

        # delete but only micro files, possible remnants
        function(f) if (file.size(f) < 20L) unlink(f)
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
#' @importFrom readr read_file
#'
#' @keywords internal
#' @noRd
#'
initTranformers <- function() {

  # prepare V8, see ./inst/js/
  ct <- V8::v8()

  # get javascript for xml to ndjson
  ct$source(system.file("js/bundle.js", package = "ctrdata"))

  # function for xml to ndjson conversion
  ct$assign(
    "parsexml",
    # https://www.npmjs.com/package/xml2js#options
    V8::JS("function(xml, opts) {injs.parseString(xml, opts, function (err, result)
           { out = result; }); return JSON.stringify(out); }"))

  # native javascript function for euctr txt to ndjson conversion
  ct$eval(readr::read_file(system.file("js/euctr2ndjson.js", package = "ctrdata")))

  # assign into package private environment, see zzz.R
  assign("ct", ct, envir = .ctrdataenv)

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
            paste0('"', ids, '"', collapse = ","), ']}}'))
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
        on.exit(try(unlink(fLineOut), silent = TRUE), add = TRUE)
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
    query = "{}")

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
  if (!is.null(hist) &&
      nrow(hist)) {

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
