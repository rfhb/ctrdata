# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved


#' @noRd
#' @export
#' @importFrom dplyr mutate case_when pull `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_detect_regex stri_split_fixed
.isPlatformTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsHere <- list(
    "euctr" = c(
      "a3_full_title_of_the_trial",
      "trialInformation.fullTitle",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title"
    ),
    "ctgov" = c(
      "official_title",
      "detailed_description.textblock",
      "clinical_results.participant_flow.period_list.period.title"
    ),
    "ctgov2" = c(
      "protocolSection.identificationModule.officialTitle",
      "protocolSection.descriptionModule.detailedDescription",
      "resultsSection.participantFlowModule.periods.title"
    ),
    "isrctn" = c(
      "trialDescription.title",
      "trialDescription.scientificTitle",
      "interventions.intervention.interventionType"
    ),
    "ctis" = c(
      # CTIS1
      "title",
      "applications.fullTitle",
      "authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle",
      "authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title",
      "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.armDetails.title",
      # CTIS2
      # "shortTitle" is mostly an uninformative study code
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle",
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title"
    ))

  # merge with fields needed for nested function
  fldsAdded <- suppressMessages(.numTestArmsSubstances())
  fldsNeeded <- sapply(names(fldsHere), function(i) na.omit(c(
    fldsHere[[i]], fldsAdded[[i]])), simplify = FALSE)
  fldsNeeded <- c("ctrname", fldsNeeded)


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates if the trial is likely a platform trial or not.
As operational definition, at least one of these criteria is true:

- trial has "platform", "basket", "umbrella", "multi-?arm" or "multi-?stage"
in its title or description (for ISRCTN, this is the only criterion; some
trials in EUCTR lack data in English)
- trial has more than 2 active arms with different investigational medicines,
after excluding comparator, auxiliary and placebo medicines (calculated with
function .numTestArmsSubstances(), not used for ISRCTN because it cannot be
calculated precisely)
- trial more than 2 periods, after excluding safety run-in, screening, enrolling,
extension and follow-up periods (for CTGOV and CTGOV2, this criterion requires
results-related data)

Requires that EUCTR results have been included in the collection, using
ctrLoadQueryIntoDb(queryterm = ..., euctrresults = TRUE, con = ...).

Requires packages dplyr and stringdist to be installed; stringdist is used for
evaluating names of active substances, which are considered similar when the
similarity is 0.8 or higher.

Returns a logical.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper definitions
  titleDefPlatform <- "basket|platform|umbrella|multi.?arm|multi.?stage"
  periodExclPlatform <- "safe|enrol|screen|follow|exten"
  minNumArmsDefPlatform <- 3L
  minNumPeriodsDefPlatform <- 3L

  # helper function
  `%>%` <- dplyr::`%>%`

  # helper function to handle
  # NA | FALSE -> NA
  # NA %orRmNa% FALSE -> FALSE
  `%orRmNa%` <- function (i1, i2) {
    mapply(
      function(e1, e2) {
        if (is.na(e1) && is.na(e2)) return(NA)
        any(na.omit(c(e1, e2)))
      }, i1, i2)
  }

  # apply nested function which provides values for each register
  # therefore the following code needs to check against register
  df$analysis_numTestArmsSubstances <- .numTestArmsSubstances(
    df = df)[[".numTestArmsSubstances"]]
  # remove columns needed exclusively for .numTestArmsSubstances
  df <- df[, -match(
    setdiff(unlist(fldsAdded), unlist(fldsHere)),
    names(df)), drop = FALSE]


  #### . EUCTR ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      a3_full_title_of_the_trial,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        trialInformation.fullTitle,
        titleDefPlatform, case_insensitive = TRUE),
    #
    helper_periodTitle = stringi::stri_split_fixed(
      subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title,
      " / "),
    #
    analysis_numberTestPeriods = lapply(
      helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    out = dplyr::case_when(
      ctrname == "EUCTR" ~ analysis_titleRelevant %orRmNa%
        (analysis_numTestArmsSubstances >= minNumArmsDefPlatform |
           analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
    )
  ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      official_title,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        detailed_description.textblock,
        titleDefPlatform, case_insensitive = TRUE),
    #
    helper_periodTitle = stringi::stri_split_fixed(
      clinical_results.participant_flow.period_list.period.title,
      " / "),
    #
    analysis_numberTestPeriods = lapply(
      helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    out = dplyr::case_when(
      ctrname == "CTGOV" ~ analysis_titleRelevant %orRmNa%
        (analysis_numTestArmsSubstances >= minNumArmsDefPlatform |
           analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
    )
  ) %>%
    dplyr::pull(out) -> df$ctgov


  #### . CTGOV2 ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      protocolSection.identificationModule.officialTitle,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        protocolSection.descriptionModule.detailedDescription,
        titleDefPlatform, case_insensitive = TRUE),
    #
    helper_periodTitle = stringi::stri_split_fixed(
      resultsSection.participantFlowModule.periods.title,
      " / "),
    #
    analysis_numberTestPeriods = lapply(
      helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    out = dplyr::case_when(
      ctrname == "CTGOV2" ~ analysis_titleRelevant %orRmNa%
        (analysis_numTestArmsSubstances >= minNumArmsDefPlatform |
           analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
    )
  ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### . ISRCTN ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      trialDescription.scientificTitle,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        trialDescription.title,
        titleDefPlatform, case_insensitive = TRUE),
    #
    analysis_isDrugTrial =
      stringi::stri_detect_regex(
        interventions.intervention.interventionType,
        "drug|biological|vaccine",
        case_insensitive = TRUE),
    #
    out = dplyr::case_when(
      ctrname == "ISRCTN" ~ analysis_isDrugTrial & analysis_titleRelevant)
  ) %>%
    dplyr::pull(out) -> df$isrctn


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      analysis_titleRelevant =
        #
        stringi::stri_detect_regex(
          title,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          applications.fullTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        #
        stringi::stri_detect_regex(
          authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle,
          titleDefPlatform, case_insensitive = TRUE),
      #
      periodTitle = dplyr::coalesce(
        as.character(authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title),
        authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title
      ),
      #
      helper_periodTitle = stringi::stri_split_fixed(
        periodTitle, " / "),
      #
      analysis_numberTestPeriods = lapply(
        helper_periodTitle,
        function(i) {
          i <- unique(tolower(na.omit(i)))
          i <- i[!grepl(periodExclPlatform, i)]
          i <- length(i)
          if (i) i else 1L
        }),
      #
      out = dplyr::case_when(
        ctrname == "CTIS" ~ analysis_titleRelevant %orRmNa%
          (analysis_numTestArmsSubstances >= minNumArmsDefPlatform |
             analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
      )
    ) %>%
    dplyr::pull(out) -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  df[[".isPlatformTrial"]] <- dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  )

  # keep only outcome columns
  df <- df[, c("_id", ".isPlatformTrial"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".isPlatformTrial"]], "logical"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .isPlatformTrial
