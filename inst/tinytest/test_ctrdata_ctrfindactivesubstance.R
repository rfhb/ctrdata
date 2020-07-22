## RH 2019-09-28

#### SETUP ####
# not needed:
# if(!at_home()) exit_file("skipping")
source("setup_ctrdata.R")

if (!check_internet()) exit_file("Reason: no internet connectivity")

#### active substance ####

# get example
tmp_test <- ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")

# test
expect_true(all(c(
  "imatinib", "gleevec", "sti 571", "glivec", "CGP 57148", "st1571")
  %in% tmp_test))

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
