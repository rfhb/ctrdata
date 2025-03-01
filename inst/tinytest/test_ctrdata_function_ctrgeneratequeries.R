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
  expect_length(urls, 4)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer AND antibody&dateFrom=2000-01-01&dateTo=2030-01-01&resultsstatus=trials-with-results",
    "https://clinicaltrials.gov/search?cond=cancer&intr=antibody&start=2000-01-01_2030-01-01&primComp=2000-01-01_2030-01-01&intr=antibody",
    "https://www.isrctn.com/search?&filters=condition:cancer,intervention:antibody,GT+overallStartDate:2000-01-01,LE+overallStartDate:2030-01-01,GT+overallEndDate:2000-01-01,LE+overallEndDate:2030-01-01,results:withResults&q=",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"cancer\",\"containAll\":\"antibody\",\"eeaStartDateFrom\":\"2000-01-01\",\"eeaStartDateTo\":\"2030-01-01\",\"eeaEndDateFrom\":\"2000-01-01\",\"eeaEndDateTo\":\"2030-01-01\",\"hasClinicalStudyReport\":true}"
  ), urls)


  # generate 2
  urls <- ctrGenerateQueries(
    intervention = "bispecific",
    startBefore = "2030-01-01")

  # test
  expect_length(urls, 4)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=bispecific&dateTo=2030-01-01",
    "https://clinicaltrials.gov/search?&intr=bispecific&start=_2030-01-01",
    "https://www.isrctn.com/search?&filters=intervention:bispecific,LE+overallStartDate:2030-01-01&q=",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"containAll\":\"bispecific\",\"eeaStartDateTo\":\"2030-01-01\"}"
  ), urls)


  # generate 3
  urls <- ctrGenerateQueries(
    condition = "cardiac failure",
    completedAfter = "2000-01-01",
    registers = c("CTGOV2", "EUCTR"))

  # test
  expect_length(urls, 2)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cardiac failure",
    "https://clinicaltrials.gov/search?cond=cardiac failure&primComp=2000-01-01_"
  ), urls)


  # generate 4
  urls <- ctrGenerateQueries(
    condition = "cancer",
    recruitment = "ongoing",
    startAfter = "2000-01-01")

  # test
  expect_length(urls, 4)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&status=ongoing&status=trial-now-transitioned&status=suspended-by-ca&status=temporarily-halted&status=restarted&dateFrom=2000-01-01",
    "https://clinicaltrials.gov/search?cond=cancer&start=2000-01-01_&aggFilters=status:act rec",
    "https://www.isrctn.com/search?&filters=condition:cancer,trialStatus:ongoing,GT+overallStartDate:2000-01-01&q=",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"cancer\",\"status\":[2,3,4,6,7],\"eeaStartDateFrom\":\"2000-01-01\"}"
  ), urls)


  # generate 5
  urls <- ctrGenerateQueries(
    condition = "heart failure",
    phase = "phase 2+3",
    startAfter = "2000-01-01")

  # test
  expect_length(urls, 4)

  # test
  expect_equivalent(c(
    "https://www.clinicaltrialsregister.eu/ctr-search/search?query=heart failure&phase=phase-two&phase=phase-three&dateFrom=2000-01-01",
    "https://clinicaltrials.gov/search?cond=heart failure&start=2000-01-01_&aggFilters=phase:2 3",
    "https://www.isrctn.com/search?&filters=condition:heart failure,phase:Phase II/III,GT+overallStartDate:2000-01-01&q=",
    "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={\"medicalCondition\":\"heart failure\",\"trialPhaseCode\":[10],\"eeaStartDateFrom\":\"2000-01-01\"}"
  ), urls)


  # sapply(urls, ctrOpenSearchPagesInBrowser)

  # end tests

}
tf()
