## RH 2019-09-28

#### SETUP ####
source("setup_ctrdata.R")

if (!checkInternet()) exit_file("Reason: no internet connectivity")

#### active substance ####

# get example
tmpTest <- ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")

# test
expect_true(all(c(
  "imatinib", "gleevec", "sti 571", "glivec", "CGP 57148", "st1571")
  %in% tmpTest))

# test
expect_error(
  ctrFindActiveSubstanceSynonyms(
    activesubstance = c("imatinib", "another"))
)

# test
expect_error(
  ctrFindActiveSubstanceSynonyms(
    activesubstance = iris)
)
