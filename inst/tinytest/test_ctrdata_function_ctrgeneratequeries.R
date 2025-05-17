## RH 2025-01-26

# setup
source("setup_ctrdata.R")
if (!at_home()) exit_file("Reason: not at_home")

# tests
tf <- function() {

  # generate 1
  urls <- ctrGenerateQueries(
    condition = "cancer",
    intervention = "antibody",
    startAfter = "2000-01-01",
    startBefore = "2030-01-01",
    completedAfter = "2000-01-01",
    completedBefore = "2030-01-01",
    onlyWithResults = TRUE)

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer AND antibody&dateFrom=2000-01-01&dateTo=2030-01-01&resultsstatus=trials-with-results",
    "https://www.isrctn.com/search?&q=&filters=condition:cancer,intervention:antibody,GT+overallStartDate:2000-01-01,LE+overallStartDate:2030-01-01,GT+overallEndDate:2000-01-01,LE+overallEndDate:2030-01-01,primaryStudyDesign:Interventional,phase:Phase 0,phase:Phase I,phase:Phase II,phase:Phase III,phase:Phase IV,phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV,results:withResults",
    "https://clinicaltrials.gov/search?cond=cancer&intr=antibody&start=2000-01-01_2030-01-01&primComp=2000-01-01_2030-01-01&aggFilters=studyType:int,results:with",
    "https://clinicaltrials.gov/expert-search?term=AREA[ConditionSearch]\"cancer\" AND AREA[InterventionSearch]\"antibody\" AND AREA[StartDate]RANGE[2000-01-01,2030-01-01] AND AREA[CompletionDate]RANGE[2000-01-01,2030-01-01] AND (AREA[StudyType]INTERVENTIONAL) AND (NOT AREA[ResultsFirstPostDate]MISSING)",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"cancer\",\"containAll\":\"antibody\",\"eeaStartDateFrom\":\"2000-01-01\",\"eeaStartDateTo\":\"2030-01-01\",\"eeaEndDateFrom\":\"2000-01-01\",\"eeaEndDateTo\":\"2030-01-01\",\"hasClinicalStudyReport\":true}"
  ), urls)


  # generate 2
  urls <- ctrGenerateQueries(
    intervention = "bispecific",
    startBefore = "2030-01-01")

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=bispecific&dateTo=2030-01-01",
    "https://www.isrctn.com/search?&q=&filters=intervention:bispecific,LE+overallStartDate:2030-01-01,primaryStudyDesign:Interventional,phase:Phase 0,phase:Phase I,phase:Phase II,phase:Phase III,phase:Phase IV,phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV",
    "https://clinicaltrials.gov/search?&intr=bispecific&start=_2030-01-01&aggFilters=studyType:int",
    "https://clinicaltrials.gov/expert-search?term=AREA[InterventionSearch]\"bispecific\" AREA[StartDate]RANGE[MAX,2030-01-01] AND (AREA[StudyType]INTERVENTIONAL)",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"containAll\":\"bispecific\",\"eeaStartDateTo\":\"2030-01-01\"}"
  ), urls)


  # generate 3
  urls <- ctrGenerateQueries(
    condition = "cardiac failure",
    completedAfter = "2000-01-01")

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cardiac failure",
    "https://www.isrctn.com/search?&q=&filters=condition:cardiac failure,GT+overallEndDate:2000-01-01,primaryStudyDesign:Interventional,phase:Phase 0,phase:Phase I,phase:Phase II,phase:Phase III,phase:Phase IV,phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV",
    "https://clinicaltrials.gov/search?cond=cardiac failure&primComp=2000-01-01_&aggFilters=studyType:int",
    "https://clinicaltrials.gov/expert-search?term=AREA[ConditionSearch]\"cardiac failure\" AND AREA[CompletionDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL)",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"cardiac failure\",\"eeaEndDateFrom\":\"2000-01-01\"}"
    ), urls)


  # generate 4
  urls <- ctrGenerateQueries(
    condition = "cancer",
    recruitment = "ongoing",
    startAfter = "2000-01-01")

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&status=ongoing&status=trial-now-transitioned&status=suspended-by-ca&status=temporarily-halted&status=restarted&dateFrom=2000-01-01",
    "https://www.isrctn.com/search?&q=&filters=condition:cancer,trialStatus:ongoing,GT+overallStartDate:2000-01-01,primaryStudyDesign:Interventional,phase:Phase 0,phase:Phase I,phase:Phase II,phase:Phase III,phase:Phase IV,phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV",
    "https://clinicaltrials.gov/search?cond=cancer&start=2000-01-01_&aggFilters=status:act rec,studyType:int",
    "https://clinicaltrials.gov/expert-search?term=AREA[ConditionSearch]\"cancer\" AND (AREA[OverallStatus]\"ACTIVE_NOT_RECRUITING\" OR AREA[OverallStatus]\"ENROLLING_BY_INVITATION\" OR AREA[OverallStatus]\"RECRUITING\") AND AREA[StartDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL)",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"cancer\",\"status\":[2,3,4,6,7],\"eeaStartDateFrom\":\"2000-01-01\"}"
  ), urls)


  # generate 5
  urls <- ctrGenerateQueries(
    condition = "heart failure",
    phase = "phase 2+3",
    startAfter = "2000-01-01")

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=heart failure&phase=phase-two&phase=phase-three&dateFrom=2000-01-01",
    "https://www.isrctn.com/search?&q=&filters=condition:heart failure,phase:Phase II/III,GT+overallStartDate:2000-01-01,primaryStudyDesign:Interventional",
    "https://clinicaltrials.gov/search?cond=heart failure&start=2000-01-01_&aggFilters=phase:2 3,studyType:int",
    "https://clinicaltrials.gov/expert-search?term=AREA[ConditionSearch]\"heart failure\" AND (AREA[Phase]\"PHASE2\" OR AREA[Phase]\"PHASE3\") AND AREA[StartDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL)",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"heart failure\",\"trialPhaseCode\":[10],\"eeaStartDateFrom\":\"2000-01-01\"}"
    ), urls)


  # generate 6
  urls <- ctrGenerateQueries(
    condition = "heart failure",
    phase = "phase 2+3",
    population = "P+A",
    startAfter = "2000-01-01",
    countries = c("Germany", "SE"))

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=heart failure&phase=phase-two&phase=phase-three&age=children&age=adolescent&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18&age=adult&dateFrom=2000-01-01&country=de&country=se",
    "https://www.isrctn.com/search?&q=&filters=condition:heart failure,phase:Phase II/IIINA,GT+overallStartDate:2000-01-01,primaryStudyDesign:Interventional,recruitmentCountry:Germany,recruitmentCountry:Sweden",
    "https://clinicaltrials.gov/expert-search?term=AREA[ConditionSearch]\"heart failure\" AND (AREA[Phase]\"PHASE2\" OR AREA[Phase]\"PHASE3\") AND (AREA[StdAge]\"CHILD\" OR AREA[StdAge]\"ADULT\") AND AREA[StartDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[LocationCountry]\"Germany\" OR AREA[LocationCountry]\"Sweden\")",
    "https://clinicaltrials.gov/expert-search?term=AREA[ConditionSearch]\"heart failure\" AND (AREA[Phase]\"PHASE2\" OR AREA[Phase]\"PHASE3\") AND (AREA[StdAge]\"CHILD\" OR AREA[StdAge]\"ADULT\") AND AREA[StartDate]RANGE[2000-01-01,MAX] AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[LocationCountry]\"Germany\" OR AREA[LocationCountry]\"Sweden\")",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"heart failure\",\"trialPhaseCode\":[10],\"ageGroupCode\":[2,3],\"eeaStartDateFrom\":\"2000-01-01\",\"msc\":[84,214]}"
   ), urls)


  # generate 7
  urls <- ctrGenerateQueries(
    searchPhrase = "antibody AND covid",
    recruitment = "completed")

  # test
  expect_length(urls, 5L)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=\"antibody\" AND \"covid\"&status=completed",
    "https://www.isrctn.com/search?q=\"antibody\" AND \"covid\"&filters=trialStatus:completed,primaryStudyDesign:Interventional,phase:Phase 0,phase:Phase I,phase:Phase II,phase:Phase III,phase:Phase IV,phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV",
    "https://clinicaltrials.gov/search?titles=\"antibody\" AND \"covid\"&aggFilters=status:com,studyType:int",
    "https://clinicaltrials.gov/expert-search?term=(AREA[TitleSearch]\"antibody\" AND AREA[TitleSearch]\"covid\") AND (AREA[OverallStatus]\"COMPLETED\") AND (AREA[StudyType]INTERVENTIONAL)",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"containAll\":\"antibody, covid\",\"status\":[5,8]}"
  ), urls)


  # end tests

}
tf()
