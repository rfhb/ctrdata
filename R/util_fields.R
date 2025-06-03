### ctrdata package

#### register field definitions ####

# mapping field names to typing function for typeField()
typeVars <- list(
  #
  ### . category ###
  #
  # - EUCTR
  # - CTGOV
  # - CTGOV2
  # - ISRCTN
  # - CTIS
  #
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
  "history.protocolSection.eligibilityModule.maximumAge"                  = "ctrDifftime",
  "history.protocolSection.eligibilityModule.minimumAge"                  = "ctrDifftime",
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
  "protocolSection.eligibilityModule.maximumAge"                  = "ctrDifftime",
  "protocolSection.eligibilityModule.minimumAge"                  = "ctrDifftime",
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
  "results.intentToPublish"       = "ctrDateTime",
  "trialDesign.overallStartDate"  = "ctrDateTime",
  "trialDesign.overallEndDate"    = "ctrDateTime",
  #
  # - CTIS
  "authorizedApplication.applicationInfo.decisionDate" = "ctrDate",
  "authorizedApplication.applicationInfo.decisions.decisionDate" = "ctrDate",
  "authorizedApplication.applicationInfo.partI.assessmentOutcomeDate" = "ctrDate",
  "authorizedApplication.applicationInfo.partIIInfo.mscInfo.assessmentOutcomeDate" = "ctrDate",
  "authorizedApplication.applicationInfo.partIIInfo.mscInfo.decisionDate" = "ctrDate",
  "authorizedApplication.applicationInfo.partIIInfo.mscInfo.firstDecisionDate" = "ctrDate",
  "authorizedApplication.applicationInfo.partIIInfo.mscInfo.revertDecisionDate" = "ctrDate",
  "authorizedApplication.applicationInfo.submissionDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.assessmentOutcomeDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.conclusionDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.associatedCtDocs.submissionDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedGlobalEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.decisionDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.trialEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.trialStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.assessmentOutcomeDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.decisionDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.firstDecisionDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.recruitmentRestartDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.revertDecisionDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.toDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialPeriod.trialEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialPeriod.trialStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialRecruitmentPeriod.fromDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "authorizedApplication.authorizedPartsII.mscInfo.trialRestartDate" = "ctrDate",
  "authorizedApplication.memberStatesConcerned.firstDecisionDate" = "ctrDate",
  "authorizedApplication.memberStatesConcerned.lastDecisionDate" = "ctrDate",
  "correctiveMeasures.correctiveMeasureNotifications.seriousBreachNotifications.submissionDate" = "ctrDate",
  "correctiveMeasures.correctiveMeasureNotifications.unexpectedEventNotifications.submissionDate" = "ctrDate",
  "correctiveMeasures.correctiveMeasureNotifications.urgentSafetyNotifications.submissionDate" = "ctrDate",
  "correctiveMeasures.revertedDate" = "ctrDate",
  "correctiveMeasures.sponsorActionRequiredByDate" = "ctrDate",
  "correctiveMeasures.sponsorSubmitDate" = "ctrDate",
  # "decisionDate" cannot be typed, is a string concatenating dates
  "events.seriousBreaches.awareDate" = "ctrDate",
  "events.seriousBreaches.breachDate" = "ctrDate",
  "events.seriousBreaches.submissionDate" = "ctrDate",
  "events.temporaryHaltList.haltDate" = "ctrDate",
  "events.temporaryHaltList.plannedRestartDate" = "ctrDate",
  "events.temporaryHaltList.submitDate" = "ctrDate",
  "events.trialEvents.events.date" = "ctrDate",
  "events.trialGlobalEndDate" = "ctrDate",
  "events.unexpectedEvents.awareDate" = "ctrDate",
  "events.unexpectedEvents.eventDate" = "ctrDate",
  "events.unexpectedEvents.submissionDate" = "ctrDate",
  "events.urgentSafetyMeasures.eventDate" = "ctrDate",
  "events.urgentSafetyMeasures.submissionDate" = "ctrDate",
  "publishDate" = "ctrDate",
  "results.clinicalStudyReports.procedureOutcomeDate" = "ctrDate",
  "results.clinicalStudyReports.submitDate" = "ctrDate",
  "results.laypersonResults.submissionDate" = "ctrDate",
  "results.laypersonResults.withdrawDate" = "ctrDate",
  "results.summaryResults.submissionDate" = "ctrDate",
  "startDateEU" = "ctrDate",
  #
  # ctis until 2024-06-17
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
  "applications.partI.productRoleGroupInfos.linkedProducts.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.linkedProducts.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "applications.partI.productRoleGroupInfos.linkedProducts.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.firstDecisionDate" = "ctrDate",
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
  "authorizedPartI.productRoleGroupInfos.linkedProducts.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.firstDecisionDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.linkedProducts.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.fromDate" = "ctrDate",
  "authorizedPartI.productRoleGroupInfos.linkedProducts.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.toDate" = "ctrDate",
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
  "authorizedPartsII.mscInfo.recruitmentRestartDate" = "ctrDate",
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
  "endDate" = "ctrDate",
  "lastUpdated" = "ctrDate",
  "layperson.submissionDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.fromDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.trialEndDate" = "ctrDate",
  "memberStatesConcerned.activeTrialPeriod.trialStartDate" = "ctrDate",
  "memberStatesConcerned.activeTrialRecruitmentPeriod.recruitmentEndDate" = "ctrDate",
  "memberStatesConcerned.activeTrialRecruitmentPeriod.recruitmentStartDate" = "ctrDate",
  "memberStatesConcerned.clinicalTrialStatusHistory.trialStatusDate" = "ctrDate",
  "memberStatesConcerned.firstDecisionDate" = "ctrDate",
  "memberStatesConcerned.fromDate" = "ctrDate",
  "memberStatesConcerned.recruitmentRestartDate" = "ctrDate",
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
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.withdrawDate" = "ctrDate",
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
  "summary.intermediateDate" = "ctrDate",
  "summary.submissionDate" = "ctrDate",
  "trialEndDate" = "ctrDate",
  "trialStartDate" = "ctrDate",
  "trialGlobalEnd.endDate" = "ctrDate",
  "trialGlobalEndDate" = "ctrDate",
  #
  #
  #### . logical ####
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
  "e823_other"          = "ctrYesNo",
  #
  "e83_the_trial_involves_single_site_in_the_member_state_concerned"    = "ctrYesNo",
  "e83_will_this_trial_be_conducted_at_a_single_site_globally"          = "ctrYesNo",
  "e83single_site_trial"                                                = "ctrYesNo", # 2025-02-10
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
  "endPoints.endPoint.countable"               = "ctrFalseTrue",
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
  "eligibility.healthy_volunteers" = "ctrYesNo",
  "has_expanded_access"            = "ctrYesNo",
  "oversight_info.has_dmc"         = "ctrYesNo",
  "oversight_info.is_fda_regulated_device" = "ctrFalseTrue",
  "oversight_info.is_fda_regulated_drug" = "ctrFalseTrue",
  "patient_data.sharing_ipd" = "ctrFalseTrue",
  "provided_document_section.provided_document.document_has_icf" = "ctrFalseTrue",
  "provided_document_section.provided_document.document_has_protocol" = "ctrFalseTrue",
  "provided_document_section.provided_document.document_has_sap" = "ctrFalseTrue",
  #
  # - CTGOV2
  "documentSection.largeDocumentModule.largeDocs.hasIcf"= "ctrFalseTrue",
  "documentSection.largeDocumentModule.largeDocs.hasProtocol"= "ctrFalseTrue",
  "documentSection.largeDocumentModule.largeDocs.hasSap"= "ctrFalseTrue",
  "hasResults" = "ctrFalseTrue",
  "protocolSection.eligibilityModule.healthyVolunteers"= "ctrFalseTrue",
  "protocolSection.oversightModule.isFdaRegulatedDevice"= "ctrFalseTrue",
  "protocolSection.oversightModule.isFdaRegulatedDrug"= "ctrFalseTrue",
  "protocolSection.oversightModule.oversightHasDmc"= "ctrFalseTrue",
  "protocolSection.statusModule.expandedAccessInfo.hasExpandedAccess"= "ctrFalseTrue",
  "resultsSection.moreInfoModule.certainAgreement.piSponsorEmployee"= "ctrFalseTrue",
  "resultsSection.moreInfoModule.certainAgreement.restrictiveAgreement"= "ctrFalseTrue",
  #
  # - ISRCTN
  "trialDescription.acknowledgment" = "ctrFalseTrue",
  "results.biomedRelated"           = "ctrFalseTrue",
  #
  # - CTIS
  "authorizedApplication.applicationInfo.decisions.isRMS" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.isLowIntervention" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.medicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.allSubstancesChemicals" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.devices.hasCeMark" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.isPaediatricFormulation" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.hasRecruitmentStarted" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.isProposedRms" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.isWillingAtDayThreeView" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.productRoleGroupInfos.products.therapies.isGmo" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.products.devices.hasCeMark" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.products.isPaediatricFormulation" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.hasRecruitmentStarted" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.isProposedRms" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.products.part1MedicinalProductRoleMscInfos.memberStateConcernedInfo.isWillingAtDayThreeView" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.products.therapies.isGmo" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.rowCountriesInfo.current" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.rowCountriesInfo.isoAlpha2Code" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.rowCountriesInfo.isoAlpha3Code" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.rowCountriesInfo.isoNumber" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.isCommercial" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.thirdParties.organisationAddress.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.sponsors.thirdParties.organisationAddress.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.hasDocument" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.isrctnNumber.id" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.isrctnNumber.number" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.isrctnNumber" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.scientificAdviceAndPip.scientificAdvices.competentAuthority.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.scientificAdviceAndPip.scientificAdvices.competentAuthority.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.isPrimary" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.secondaryEndPoints.isPrimary" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.medicalCondition.partIMedicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.isFemaleSubjects" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.isMaleSubjects" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.populationOfTrialSubjects.isVulnerablePopulationSelected" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialCategory.isLowIntervention" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.isBenefitRiskBalanceEndTrail" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.isBenefitRiskBalanceTemporaryHalt" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.isEndTrial" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.activeTrialPeriod.isTemporaryHalt" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.hasRecruitmentStarted" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.isProposedRms" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.mscInfo.isWillingAtDayThreeView" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.trialSites.organisationAddressInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.authorizedPartsII.trialSites.organisationAddressInfo.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedApplication.eudraCt.isTransitioned" = "ctrFalseTrue",
  "events.temporaryHaltList.isBenefitRiskBalanceChange" = "ctrFalseTrue",
  "events.temporaryHaltList.isPublished" = "ctrFalseTrue",
  "events.temporaryHaltList.isTreatmentStopped" = "ctrFalseTrue",
  "events.temporaryHaltList.reasonList.isCommentRequired" = "ctrFalseTrue",
  "events.temporaryHaltList.reasonList.isSmRequiredForRestart" = "ctrFalseTrue",
  "events.trialEvents.earlyTerminationReason.isLateCandidate" = "ctrFalseTrue",
  #
  # ctis until 2024-06-17
  "applications.ctMSCs.hasRecruitmentStarted" = "ctrFalseTrue",
  "applications.ctMSCs.isProposedRms" = "ctrFalseTrue",
  "applications.ctMSCs.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.ctMSCsByApplication.hasRecruitmentStarted" = "ctrFalseTrue",
  "applications.ctMSCsByApplication.isProposedRms" = "ctrFalseTrue",
  "applications.ctMSCsByApplication.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "applications.eudraCtInfo.hasVhp" = "ctrFalseTrue",
  "applications.eudraCtInfo.isTransitioned" = "ctrFalseTrue",
  "applications.isDossierUpdate" = "ctrFalseTrue",
  "applications.isMultiTrialSM" = "ctrFalseTrue",
  "applications.partI.isEditable" = "ctrFalseTrue",
  "applications.partI.isLowIntervention" = "ctrFalseTrue",
  "applications.partI.medicalConditions.isConditionRareDisease" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.hasDevice" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.isDraftUnauthProduct" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.isPaediatricFormulation" = "ctrFalseTrue",
  "applications.partI.productRoleGroupInfos.products.productDictionaryInfo.isSelectedInMultiTrialSM" = "ctrFalseTrue",
  "applications.partI.products.hasDevice" = "ctrFalseTrue",
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
  "applications.partI.trialDetails.associatedClinicalTrials.hasDocument" = "ctrFalseTrue",
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
  "applications.partIIInfo.mscInfo.hasRecruitmentStarted" = "ctrFalseTrue",
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
  "authorizedPartI.productRoleGroupInfos.products.hasDevice" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.isDraftUnauthProduct" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.isPaediatricFormulation" = "ctrFalseTrue",
  "authorizedPartI.productRoleGroupInfos.products.productDictionaryInfo.isSelectedInMultiTrialSM" = "ctrFalseTrue",
  "authorizedPartI.products.hasDevice" = "ctrFalseTrue",
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
  "authorizedPartI.trialDetails.associatedClinicalTrials.hasDocument" = "ctrFalseTrue",
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
  "authorizedPartsII.mscInfo.hasRecruitmentStarted" = "ctrFalseTrue",
  "authorizedPartsII.mscInfo.isProposedRms" = "ctrFalseTrue",
  "authorizedPartsII.mscInfo.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.isCreate" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.isSave" = "ctrFalseTrue",
  "authorizedPartsII.trialSites.organisationAddressInfo.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "baselineCharacteristics.ageCategoricalCharacteristic.readyForValues" = "ctrFalseTrue",
  "baselineCharacteristics.ageContinuousCharacteristic.readyForValues" = "ctrFalseTrue",
  "baselineCharacteristics.genderCategoricalCharacteristic.readyForValues" = "ctrFalseTrue",
  "baselineCharacteristics.studyCategoricalCharacteristics.studyCategoricalCharacteristic.readyForValues" = "ctrFalseTrue",
  "correctiveMeasures.isImmediateActionRequired" = "ctrFalseTrue",
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
  "endPoints.endPoint.readyForValues" = "ctrFalseTrue",
  "eudraCtInfo.hasVhp" = "ctrFalseTrue",
  "hasAmendmentApplied" = "ctrFalseTrue",
  "hasDeferrallApplied" = "ctrFalseTrue",
  "includesPaediatricSubjects" = "ctrFalseTrue",
  "memberStatesConcerned.hasRecruitmentStarted" = "ctrFalseTrue",
  "memberStatesConcerned.isProposedRms" = "ctrFalseTrue",
  "memberStatesConcerned.organisationInfo.isBusinessKeyValidated" = "ctrFalseTrue",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.isBenefitRisckBalanceChange" = "ctrFalseTrue",
  "mscTrialNotificationsInfoList.mscNotificationsListInfo.isPublished" = "ctrFalseTrue",
  "oversight_info.has_dmc" = "ctrFalseTrue",
  "primarySponsor.addresses.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.addresses.isCreate" = "ctrFalseTrue",
  "primarySponsor.addresses.isSave" = "ctrFalseTrue",
  "primarySponsor.addresses.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.contactPointForUnion.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.isCommercial" = "ctrFalseTrue",
  "primarySponsor.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.publicContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "primarySponsor.scientificContacts.organisation.isBusinessKeyValidated" = "ctrFalseTrue",
  "protocolSection.statusModule.expandedAccessInfo.hasExpandedAccess" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfiConsiderations.rfiConsiderations.isDeConsolidated" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfis.hasApplicationChanges" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfis.isDueInLessThanTwoDays" = "ctrFalseTrue",
  "publicEvaluation.partIIEvaluationList.partIIRfis.isEditingApplication" = "ctrFalseTrue",
  "publicEvaluation.partIRfiConsiderations.rfiConsiderations.isDeConsolidated" = "ctrFalseTrue",
  "publicEvaluation.partIRfis.hasApplicationChanges" = "ctrFalseTrue",
  "publicEvaluation.partIRfis.isDueInLessThanTwoDays" = "ctrFalseTrue",
  "publicEvaluation.partIRfis.isEditingApplication" = "ctrFalseTrue",
  "publicEvaluation.validationRfiConsiderations.rfiConsiderations.isDeConsolidated" = "ctrFalseTrue",
  "publicEvaluation.validationRfis.hasApplicationChanges" = "ctrFalseTrue",
  "publicEvaluation.validationRfis.isDueInLessThanTwoDays" = "ctrFalseTrue",
  "publicEvaluation.validationRfis.isEditingApplication" = "ctrFalseTrue",
  "trialChanges.hasGlobalAmendments" = "ctrFalseTrue",
  "trialChanges.hasGlobalInterruptions" = "ctrFalseTrue",
  "trialInformation.isGlobalEndOfTrialReached" = "ctrFalseTrue",
  #
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
  "clinical_results.baseline.analyzed_list.analyzed.count_list.count.value" = "ctrIntList",
  "clinical_results.baseline.measure_list.measure.class_list.class.analyzed_list.analyzed.count_list.count.value" = "ctrIntList",
  "clinical_results.outcome_list.outcome.measure.analyzed_list.analyzed.count_list.count.value" = "ctrIntList",
  "clinical_results.outcome_list.outcome.measure.class_list.class.analyzed_list.analyzed.count_list.count.value" = "ctrIntList",
  #
  # - ISRCTN
  "participants.targetEnrolment"     = "ctrInt",
  "participants.totalFinalEnrolment" = "ctrInt",
  #
  # - CTIS
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalExclusionCriteria.number" = "ctrInt",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.eligibilityCriteria.principalInclusionCriteria.number" = "ctrInt",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.number" = "ctrInt",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.secondaryEndPoints.number" = "ctrInt",
  "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialObjective.secondaryObjectives.number" = "ctrInt",
  "ctPublicStatusCode" = "ctrInt",
  "totalNumberEnrolled" = "ctrInt",
  #
  "authorizedApplication.authorizedPartI.rowSubjectCount" = "ctrInt",
  "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.associatedCtDocs.scanCount" = "ctrInt",
  "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.associatedCtDocs.versions.scanCount" = "ctrInt",
  "authorizedApplication.authorizedPartsII.recruitmentSubjectCount" = "ctrInt",
  #
  # ctis until 2024-06-17
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
  "authorizedPartsII.recruitmentSubjectCount" = "ctrInt",
  #
  # - CTGOV2
  "history.history_version.version_number" = "ctrInt",
  "protocolSection.designModule.enrollmentInfo.count" = "ctrInt",
  "resultsSection.baselineCharacteristicsModule.denoms.counts.value" = "ctrInt",
  "resultsSection.outcomeMeasuresModule.outcomeMeasures.denoms.counts.value" = "ctrInt"
  #
)


#### country code mapping ####
countryTable <- read.table(
  sep = ",",
  strip.white = TRUE,
  fill = TRUE,
  na.strings = "",
  quote = "",
  text = "
  1, Afghanistan, AF, AFG
  2, Aland Islands, AX, ALA
  3, Albania, AL, ALB
  4, Algeria, DZ, DZA
  5, American Samoa, AS, ASM
  6, Andorra, AD, AND
  7, Angola, AO, AGO
  8, Anguilla, AI, AIA
  9, Antarctica, AQ, ATA
 10, Antigua and Barbuda, AG, ATG
 11, Argentina, AR, ARG
 12, Armenia, AM, ARM
 13, Aruba, AW, ABW
 14, Australia, AU, AUS
 15, Austria, AT, AUT
 16, Azerbaijan, AZ, AZE
 17, Bahamas, BS, BHS
 18, Bahrain, BH, BHR
 19, Bangladesh, BD, BGD
 20, Barbados, BB, BRB
 21, Belarus, BY, BLR
 22, Belgium, BE, BEL
 23, Belize, BZ, BLZ
 24, Benin, BJ, BEN
 25, Bermuda, BM, BMU
 26, Bhutan, BT, BTN
 27, Bolivia, BO, BOL
 28, Bosnia and Herzegovina, BA, BIH
 29, Botswana, BW, BWA
 30, Bouvet Island, BV, BVT
 31, Brazil, BR, BRA
 32, British Virgin Islands, VG, VGB
 33, British Indian Ocean Territory, IO, IOT
 34, Brunei Darussalam, BN, BRN
 35, Bulgaria, BG, BGR
 36, Burkina Faso, BF, BFA
 37, Burundi, BI, BDI
 38, Cambodia, KH, KHM
 39, Cameroon, CM, CMR
 40, Canada, CA, CAN
 41, Cape Verde, CV, CPV
 42, Cayman Islands, KY, CYM
 43, Central African Republic, CF, CAF
 44, Chad, TD, TCD
 45, Chile, CL, CHL
 46, China, CN, CHN
 47, Hong KongSpecial Administrative Region of China, HK, HKG
 48, MacaoSpecial Administrative Region of China, MO, MAC
 49, Christmas Island, CX, CXR
 50, Cocos (Keeling) Islands, CC, CCK
 51, Colombia, CO, COL
 52, Comoros, KM, COM
 53, Congo (Brazzaville), CG, COG
 54, CongoDemocratic Republic of the, CD, COD
 55, Cook Islands, CK, COK
 56, Costa Rica, CR, CRI
 57, Cote d'Ivoire, CI, CIV
 58, Croatia, HR, HRV
 59, Cuba, CU, CUB
 60, Cyprus, CY, CYP
 61, Czech Republic, CZ, CZE
 62, Denmark, DK, DNK
 63, Djibouti, DJ, DJI
 64, Dominica, DM, DMA
 65, Dominican Republic, DO, DOM
 66, Ecuador, EC, ECU
 67, Egypt, EG, EGY
 68, El Salvador, SV, SLV
 69, Equatorial Guinea, GQ, GNQ
 70, Eritrea, ER, ERI
 71, Estonia, EE, EST
 72, Ethiopia, ET, ETH
 73, Falkland Islands (Malvinas), FK, FLK
 74, Faroe Islands, FO, FRO
 75, Fiji, FJ, FJI
 76, Finland, FI, FIN
 77, France, FR, FRA
 78, French Guiana, GF, GUF
 79, French Polynesia, PF, PYF
 80, French Southern Territories, TF, ATF
 81, Gabon, GA, GAB
 82, Gambia, GM, GMB
 83, Georgia, GE, GEO
 84, Germany, DE, DEU
 85, Ghana, GH, GHA
 86, Gibraltar, GI, GIB
 87, Greece, GR, GRC
 88, Greenland, GL, GRL
 89, Grenada, GD, GRD
 90, Guadeloupe, GP, GLP
 91, Guam, GU, GUM
 92, Guatemala, GT, GTM
 93, Guernsey, GG, GGY
 94, Guinea, GN, GIN
 95, Guinea-Bissau, GW, GNB
 96, Guyana, GY, GUY
 97, Haiti, HT, HTI
 98, Heard Island and Mcdonald Islands, HM, HMD
 99, Holy See (Vatican City State), VA, VAT
100, Honduras, HN, HND
101, Hungary, HU, HUN
102, Iceland, IS, ISL
103, India, IN, IND
104, Indonesia, ID, IDN
105, IranIslamic Republic of, IR, IRN
106, Iraq, IQ, IRQ
107, Ireland, IE, IRL
108, Isle of Man, IM, IMN
109, Israel, IL, ISR
110, Italy, IT, ITA
111, Jamaica, JM, JAM
112, Japan, JP, JPN
113, Jersey, JE, JEY
114, Jordan, JO, JOR
115, Kazakhstan, KZ, KAZ
116, Kenya, KE, KEN
117, Kiribati, KI, KIR
118, KoreaDemocratic People's Republic of, KP, PRK
119, KoreaRepublic of, KR, KOR
120, Kuwait, KW, KWT
121, Kyrgyzstan, KG, KGZ
122, Lao PDR, LA, LAO
123, Latvia, LV, LVA
124, Lebanon, LB, LBN
125, Lesotho, LS, LSO
126, Liberia, LR, LBR
127, Libya, LY, LBY
128, Liechtenstein, LI, LIE
129, Lithuania, LT, LTU
130, Luxembourg, LU, LUX
131, MacedoniaRepublic of, MK, MKD
132, Madagascar, MG, MDG
133, Malawi, MW, MWI
134, Malaysia, MY, MYS
135, Maldives, MV, MDV
136, Mali, ML, MLI
137, Malta, MT, MLT
138, Marshall Islands, MH, MHL
139, Martinique, MQ, MTQ
140, Mauritania, MR, MRT
141, Mauritius, MU, MUS
142, Mayotte, YT, MYT
143, Mexico, MX, MEX
144, MicronesiaFederated States of, FM, FSM
145, Moldova, MD, MDA
146, Monaco, MC, MCO
147, Mongolia, MN, MNG
148, Montenegro, ME, MNE
149, Montserrat, MS, MSR
150, Morocco, MA, MAR
151, Mozambique, MZ, MOZ
152, Myanmar, MM, MMR
153, Namibia, NA, NAM
154, Nauru, NR, NRU
155, Nepal, NP, NPL
156, Netherlands, NL, NLD
157, Netherlands Antilles, AN, ANT
158, New Caledonia, NC, NCL
159, New Zealand, NZ, NZL
160, Nicaragua, NI, NIC
161, Niger, NE, NER
162, Nigeria, NG, NGA
163, Niue, NU, NIU
164, Norfolk Island, NF, NFK
165, Northern Mariana Islands, MP, MNP
166, Norway, NO, NOR
167, Oman, OM, OMN
168, Pakistan, PK, PAK
169, Palau, PW, PLW
170, Palestinian TerritoryOccupied, PS, PSE
171, Panama, PA, PAN
172, Papua New Guinea, PG, PNG
173, Paraguay, PY, PRY
174, Peru, PE, PER
175, Philippines, PH, PHL
176, Pitcairn, PN, PCN
177, Poland, PL, POL
178, Portugal, PT, PRT
179, Puerto Rico, PR, PRI
180, Qatar, QA, QAT
181, Reunion, RE, REU
182, Romania, RO, ROU
183, Russian Federation, RU, RUS
184, Rwanda, RW, RWA
185, Saint-Barthelemy, BL, BLM
186, Saint Helena, SH, SHN
187, Saint Kitts and Nevis, KN, KNA
188, Saint Lucia, LC, LCA
189, Saint-Martin (French part), MF, MAF
190, Saint Pierre and Miquelon, PM, SPM
191, Saint Vincent and Grenadines, VC, VCT
192, Samoa, WS, WSM
193, San Marino, SM, SMR
194, Sao Tome and Principe, ST, STP
195, Saudi Arabia, SA, SAU
196, Senegal, SN, SEN
197, Serbia, RS, SRB
198, Seychelles, SC, SYC
199, Sierra Leone, SL, SLE
200, Singapore, SG, SGP
201, Slovakia, SK, SVK
202, Slovenia, SI, SVN
203, Solomon Islands, SB, SLB
204, Somalia, SO, SOM
205, South Africa, ZA, ZAF
206, South Georgia and the South Sandwich Islands, GS, SGS
207, South Sudan, SS, SSD
208, Spain, ES, ESP
209, Sri Lanka, LK, LKA
210, Sudan, SD, SDN
211, Suriname *, SR, SUR
212, Svalbard and Jan Mayen Islands, SJ, SJM
213, Swaziland, SZ, SWZ
214, Sweden, SE, SWE
215, Switzerland, CH, CHE
216, Syrian Arab Republic (Syria), SY, SYR
217, TaiwanRepublic of China, TW, TWN
218, Tajikistan, TJ, TJK
219, Tanzania *United Republic of, TZ, TZA
220, Thailand, TH, THA
221, Timor-Leste, TL, TLS
222, Togo, TG, TGO
223, Tokelau, TK, TKL
224, Tonga, TO, TON
225, Trinidad and Tobago, TT, TTO
226, Tunisia, TN, TUN
227, Turkey, TR, TUR
228, Turkmenistan, TM, TKM
229, Turks and Caicos Islands, TC, TCA
230, Tuvalu, TV, TUV
231, Uganda, UG, UGA
232, Ukraine, UA, UKR
233, United Arab Emirates, AE, ARE
234, United Kingdom, GB, GBR
235, United States of America, US, USA
236, United States Minor Outlying Islands, UM, UMI
237, Uruguay, UY, URY
238, Uzbekistan, UZ, UZB
239, Vanuatu, VU, VUT
240, Venezuela (Bolivarian Republic of), VE, VEN
241, Viet Nam, VN, VNM
242, Virgin IslandsUS, VI, VIR
243, Wallis and Futuna Islands, WF, WLF
244, Western Sahara, EH, ESH
245, Yemen, YE, YEM
246, Zambia, ZM, ZMB
247, Zimbabwe, ZW, ZWE
  ")
