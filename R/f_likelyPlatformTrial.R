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
#' @importFrom dplyr mutate case_when pull left_join coalesce `%>%`
#' @importFrom stringdist stringsimmatrix
#' @importFrom stringi stri_count_fixed stri_detect_regex stri_split_fixed
#' @importFrom tidyr pivot_longer unnest
#' @importFrom rlang .data
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
  df2 <- .dbMapIdsTrials(con = parent.frame()$con)
  #
  # keep only mapped columns, split sponsor into list
  df2 %>%
    dplyr::select(!c("_id", "ctrname")) %>%
    dplyr::mutate(
      SPONSOR = stringi::stri_split_fixed(
        .data$SPONSOR, " / ")) -> df2
  #
  dplyr::mutate(
    # add all columns into single list
    df2, .idsRelatedTrials = rowColsList(
      dplyr::select(df2, names(df2))
    )) %>%
    # keep only the register columns
    dplyr::select(!c("SPONSOR")) %>%
    # expand list items into their own rows
    tidyr::unnest(.data$.idsRelatedTrials) %>%
    # wide to long
    tidyr::pivot_longer(
      cols = !".idsRelatedTrials") %>%
    # remove register name
    dplyr::select(!"name") %>%
    dplyr::filter(.data$value != "") %>%
    dplyr::filter(.data$value != .data$.idsRelatedTrials) %>%
    unique() %>%
    # turn rows of .idsRelatedTrials by
    # trial identifier into list which
    # does not include that identifier
    dplyr::summarise(.idsRelatedTrials = list(
      unique(.data$.idsRelatedTrials[
        .data$.idsRelatedTrials != .data$value
      ])), .by = .data$value) %>%
    dplyr::rename("_id" = .data$value) -> df2
  #
  # merge column into df
  df <- dplyr::left_join(
    df, df2, by = "_id")
  #
  # ensure .idsRelatedTrials is a list
  # and has standard value if empty
  df %>% dplyr::mutate(
    .idsRelatedTrials = lapply(
      .data$.idsRelatedTrials,
      function(i) if (is.null(i)) NA else i
    )) -> df


  #### . EUCTR ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      .data$a3_full_title_of_the_trial,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        .data$trialInformation.fullTitle,
        titleDefPlatform, case_insensitive = TRUE),
    #
    helper_periodTitle = stringi::stri_split_fixed(
      .data$subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title,
      " / "),
    #
    analysis_numberTestPeriods = lapply(
      .data$helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    out = dplyr::case_when(
      ctrname == "EUCTR" ~ .data$analysis_titleRelevant %orRmNa%
        (.data$.numTestArmsSubstances >= minNumArmsDefPlatform |
           .data$analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
    )
  ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      .data$official_title,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        .data$detailed_description.textblock,
        titleDefPlatform, case_insensitive = TRUE),
    #
    helper_periodTitle = stringi::stri_split_fixed(
      .data$clinical_results.participant_flow.period_list.period.title,
      " / "),
    #
    analysis_numberTestPeriods = lapply(
      .data$helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    out = dplyr::case_when(
      ctrname == "CTGOV" ~ analysis_titleRelevant %orRmNa%
        (.data$.numTestArmsSubstances >= minNumArmsDefPlatform |
           .data$analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
    )
  ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      .data$protocolSection.identificationModule.officialTitle,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        .data$protocolSection.descriptionModule.detailedDescription,
        titleDefPlatform, case_insensitive = TRUE),
    #
    helper_periodTitle = stringi::stri_split_fixed(
      .data$resultsSection.participantFlowModule.periods.title,
      " / "),
    #
    analysis_numberTestPeriods = lapply(
      .data$helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    out = dplyr::case_when(
      ctrname == "CTGOV2" ~ .data$analysis_titleRelevant %orRmNa%
        (.data$.numTestArmsSubstances >= minNumArmsDefPlatform |
           .data$analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
    )
  ) %>%
    dplyr::pull("out") -> df$ctgov2


  #### . ISRCTN ####
  df %>% dplyr::mutate(
    #
    analysis_titleRelevant = stringi::stri_detect_regex(
      .data$trialDescription.scientificTitle,
      titleDefPlatform, case_insensitive = TRUE) %orRmNa%
      stringi::stri_detect_regex(
        .data$trialDescription.title,
        titleDefPlatform, case_insensitive = TRUE),
    #
    analysis_isDrugTrial =
      stringi::stri_detect_regex(
        .data$interventions.intervention.interventionType,
        "drug|biological|vaccine",
        case_insensitive = TRUE),
    #
    out = dplyr::case_when(
      ctrname == "ISRCTN" ~ .data$analysis_isDrugTrial &
        .data$analysis_titleRelevant)
  ) %>%
    dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      analysis_titleRelevant =
        #
        stringi::stri_detect_regex(
          .data$title,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          .data$applications.fullTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          .data$authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          .data$authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        #
        stringi::stri_detect_regex(
          .data$authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle,
          titleDefPlatform, case_insensitive = TRUE) %orRmNa%
        stringi::stri_detect_regex(
          .data$authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle,
          titleDefPlatform, case_insensitive = TRUE),
      #
      periodTitle = dplyr::coalesce(
        as.character(.data$authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title),
        .data$authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title
      ),
      #
      helper_periodTitle = stringi::stri_split_fixed(
        .data$periodTitle, " / "),
      #
      analysis_numberTestPeriods = lapply(
        .data$helper_periodTitle,
        function(i) {
          i <- unique(tolower(na.omit(i)))
          i <- i[!grepl(periodExclPlatform, i)]
          i <- length(i)
          if (i) i else 1L
        }),
      #
      out = dplyr::case_when(
        ctrname == "CTIS" ~ .data$analysis_titleRelevant %orRmNa%
          (.data$.numTestArmsSubstances >= minNumArmsDefPlatform |
             .data$analysis_numberTestPeriods >= minNumPeriodsDefPlatform)
      )
    ) %>%
    dplyr::pull("out") -> df$ctis


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
