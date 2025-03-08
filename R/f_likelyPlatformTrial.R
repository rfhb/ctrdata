#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved

#' Calculate if study is likely a platform trial or not
#'
#' Trial concept calculated: platform trial, research platform.
#' As operational definition, at least one of these criteria is true:
#' a. trial has "platform", "basket", "umbrella", "multi.?arm", "multi.?stage"
#' or "master protocol"
#' in its title or description (for ISRCTN, this is the only criterion; some
#' trials in EUCTR lack data in English),
#' b. trial has more than 2 active arms with different investigational medicines,
#' after excluding comparator, auxiliary and placebo medicines (calculated with
#' \link{f.numTestArmsSubstances}; not used for ISRCTN because it cannot be
#' calculated precisely),
#' c. trial more than 2 periods, after excluding safety run-in, screening,
#' enrolling, extension and follow-up periods (for CTGOV and CTGOV2, this
#' criterion requires results-related data).
#' Requires that EUCTR results have been included in the collection, using
#' ctrLoadQueryIntoDb(queryterm = ..., euctrresults = TRUE, con = ...).
#' Requires packages dplyr and stringdist to be installed; stringdist is used
#' for evaluating names of active substances, which are considered similar when
#' the similarity is 0.8 or higher.
#'
#' Publication references considered:
#' E-PEARL WP2 2020 https://tinyurl.com/eupearld21terminology (which did not
#' include all basket trials in the definition, as done here)
#' Williams RJ et al. 2022 https://doi.org/10.1136/bmj-2021-067745

#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.likelyPlatformTrial`, a logical,
#' and `.idsRelatedTrials`, a list of identifiers of trials calculated to be
#' related (e.g., based on CTIS' `associatedClinicalTrials` and otherwise on
#' additional identifiers in the record).
#'
#' @export
#'
#' @importFrom dplyr mutate case_when pull left_join `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_detect_regex stri_split_fixed
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # fields needed
#' f.likelyPlatformTrial()
#'
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.likelyPlatformTrial",
#'   con = dbc)
#' }
#'
#'
f.likelyPlatformTrial <- function(df = NULL) {

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
      #
      "authorizedPartI.trialDetails.associatedClinicalTrials.parentClinicalTrialId",
      "authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber",
      "authorizedPartsII.mscInfo.clinicalTrialId",

      # CTIS2
      # "shortTitle" is mostly an uninformative study code
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle",
      "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title",
      #
      "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.parentClinicalTrialId",
      "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber",
      "authorizedApplication.authorizedPartsII.mscInfo.clinicalTrialId"

    ))

  # merge with fields needed for nested function
  fldsAdded <- suppressMessages(f.numTestArmsSubstances())
  fldsNeeded <- sapply(names(fldsHere), function(i) na.omit(c(
    fldsHere[[i]], fldsAdded[[i]])), simplify = FALSE)
  fldsNeeded <- c("ctrname", fldsNeeded)


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper definitions
  minNumArmsDefPlatform <- 3L
  minNumPeriodsDefPlatform <- 3L
  periodExclPlatform <- "safe|enrol|screen|follow|extens"
  titleDefPlatform <- paste0(
    "basket|platform|umbrella|multi.?arm|multi.?stage|",
    "master protocol|sub.?protocol|sub.?study"
  )

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
  df <- dplyr::left_join(
    df, f.numTestArmsSubstances(df = df), by = "_id")

  # remove columns needed exclusively for .numTestArmsSubstances
  df <- df[, -match(
    setdiff(unlist(fldsAdded), unlist(fldsHere)),
    names(df)), drop = FALSE]

  # apply nested function which provides a table
  # mapping all trial identifiers against all registers
  rowColsList <- function(...) {
    apply(..., 1, function(i) {
      i <- sort(unique(na.omit(unlist(i))))
      i[i != ""]
    })
  }

  # get mapping table
  df <- dplyr::left_join(
    df,
    .dbMapIdsTrials(con = parent.frame()$con) %>%
      dplyr::mutate(
        sponsorIds = stringi::stri_split_fixed(
          SPONSOR, " / ")) %>%
      dplyr::mutate(EUCTR = dplyr::if_else(
        !is.na(EUCTR), `_id`, EUCTR)) %>%
      dplyr::select(!c(`_id`, ctrname, SPONSOR)) %>%
      dplyr::mutate(.idsRelatedTrials = rowColsList(.)) %>%
      dplyr::select(!sponsorIds) %>%
      tidyr::pivot_longer(cols = !.idsRelatedTrials) %>%
      dplyr::select(!name) %>%
      dplyr::rename("_id" = value) %>%
      unique(),
    by = "_id"
  )


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
        (.numTestArmsSubstances >= minNumArmsDefPlatform |
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
        (.numTestArmsSubstances >= minNumArmsDefPlatform |
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
        (.numTestArmsSubstances >= minNumArmsDefPlatform |
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
          (.numTestArmsSubstances >= minNumArmsDefPlatform |
             analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
      )
    ) %>%
    dplyr::pull(out) -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  df[[".likelyPlatformTrial"]] <- dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  )

  # keep only outcome columns
  df <- df[, c(
    "_id",
    ".likelyPlatformTrial",
    ".idsRelatedTrials"
  ), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".likelyPlatformTrial"]], "logical"))
  stopifnot(inherits(df[[".idsRelatedTrials"]], "list"))
  stopifnot(ncol(df) == 3L)

  # return
  return(df)

} # end f.likelyPlatformTrial
