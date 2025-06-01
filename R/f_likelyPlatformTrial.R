#### history ####
# 2025-01-27 first partly working version
# 2025-02-08, 2025-05-31 improved

#' Calculate if study is likely a platform trial or not
#'
#' Trial concept calculated: platform trial, research platform.
#' As operational definition, at least one of these criteria is true:
#' a. trial has "platform", "basket", "umbrella", "multi.?arm", "multi.?stage"
#' or "master protocol" in its title or description (for ISRCTN, this is the
#' only criterion; some trials in EUCTR lack data in English),
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
#' for evaluating terms in brackets in the trial title, where trials may be
#' related if the term similarity is 0.7 or higher.
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
#' and two complementary columns, each with lists of identifiers found within
#' the database collection: `.likelyRelatedTrials` (based on other identifiers
#' of the trial record, including `associatedClinicalTrials` from CTIS) and
#' `.maybeRelatedTrials` (based on similar short terms within a first set of
#' brackets or before a colon in the trial title).
#'
#' @export
#'
#' @importFrom dplyr mutate case_when pull left_join coalesce `%>%`
#' @importFrom stringi stri_count_fixed stri_detect_regex stri_split_fixed
#' @importFrom stringdist stringsimmatrix
#' @importFrom tidyr pivot_longer unnest
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.likelyPlatformTrial()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.likelyPlatformTrial",
#'   con = dbc)
#' trialsDf
#'
f.likelyPlatformTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsHere <- list(
    "euctr" = c(
      # "a3_full_title_of_the_trial",
      # "trialInformation.fullTitle",
      "subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title"
    ),
    "ctgov" = c(
      # "official_title",
      # "detailed_description.textblock",
      "clinical_results.participant_flow.period_list.period.title"
    ),
    "ctgov2" = c(
      # "protocolSection.identificationModule.officialTitle",
      # "protocolSection.descriptionModule.detailedDescription",
      "resultsSection.participantFlowModule.periods.title"
    ),
    "isrctn" = c(
      # "trialDescription.title",
      # "trialDescription.scientificTitle",
      "interventions.intervention.interventionType"
    ),
    "ctis" = c(
      # CTIS1
      # "applications.fullTitle",
      # "authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle",
      # "authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title",
      # "authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.armDetails.title",
      # "authorizedPartI.trialDetails.associatedClinicalTrials.parentClinicalTrialId",
      "authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber",
      # "authorizedPartsII.mscInfo.clinicalTrialId",
      #
      # CTIS2
      # "shortTitle" is mostly an uninformative study code
      # "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.fullTitle",
      # "authorizedApplication.authorizedPartI.trialDetails.clinicalTrialIdentifiers.publicTitle",
      "authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title",
      # "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.parentClinicalTrialId",
      "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber"
      # "authorizedApplication.authorizedPartsII.mscInfo.clinicalTrialId"
    ))

  # merge with fields needed for nested function
  flst1 <- suppressMessages(f.numTestArmsSubstances())
  flst2 <- suppressMessages(f.trialTitle())
  keys <- unique(c(names(flst1), names(flst2)))
  fldsAdded <- setNames(mapply(c, flst1[keys], flst2[keys]), keys)
  #
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
  indexThreshold <- 0.7
  platformThreshold <- 0.3
  minNumArmsDefPlatform <- 2L
  minNumPeriodsDefPlatform <- 2L
  periodExclPlatform <- "safe|enrol|screen|follow|extens"
  titleDefPlatform <- paste0(
    "basket|platform|umbrella|multi.?arm|multi.?stage|",
    "master protocol|sub.?protocol|sub.?study"
  )

  # helper function
  `%>%` <- dplyr::`%>%`

  # apply nested function which provides values for each register
  # therefore the following code needs to check against register
  if(!any(names(df) == ".numTestArmsSubstances")) df <- dplyr::left_join(
    df, f.numTestArmsSubstances(df = df), by = "_id")

  # apply nested function which provides values for each register
  # therefore the following code needs to check against register
  if(!any(names(df) == ".trialTitle")) df <- dplyr::left_join(
    df, f.trialTitle(df = df), by = "_id")

  # remove columns needed exclusively for nested functions
  df <- df[, -match(
    setdiff(unlist(fldsAdded), unlist(fldsHere)),
    names(df)), drop = FALSE]

  # helper
  rowColsList <- function(...) {
    apply(..., 1, function(i) {
      i <- na.omit(unlist(i))
      if (!length(i)) return(NA_character_)
      i <- stringi::stri_split_regex(
        i, "(, | / )",
        omit_empty = TRUE,
        simplify = TRUE)
      i <- sort(unique(na.omit(i)))
      i <- i[i != ""]
      return(i)
    }, simplify = FALSE)
  }

  # get and mangle mapping table, add ids associated trials
  df2 <- .dbMapIdsTrials(con = parent.frame()$con)
  df2 <- dplyr::left_join(df2, df[, c("_id",
    "authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber",
    "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber"
  )], by = "_id")
  # collapse into single column that is a list
  df2$.likelyRelatedTrials <- rowColsList(
    dplyr::select(df2, c(
      registerList,
      "authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber",
      "authorizedApplication.authorizedPartI.trialDetails.associatedClinicalTrials.ctNumber"
    )))
  # expand rows for each list item
  df2 <- tidyr::unnest(
    df2, cols = .data$.likelyRelatedTrials)
  # mangle, revert to one list in one row per trial
  df2 <- df2 %>%
    dplyr::select(c("_id", "ctrname", ".likelyRelatedTrials")) %>%
    tidyr::pivot_longer(cols = !"_id") %>%
    # since value has no country suffix as needed for _id
    dplyr::filter(.data$name != "EUCTR") %>%
    dplyr::filter(.data$name != "ctrname") %>%
    dplyr::filter(.data$value != "") %>%
    dplyr::filter(.data$value != .data$`_id`) %>%
    dplyr::select(!"name") %>%
    unique() %>%
    dplyr::rename(".likelyRelatedTrials" = .data$value) %>%
    dplyr::summarise(
      .likelyRelatedTrials = list(.data$.likelyRelatedTrials),
      .by = .data$`_id`)

  # merge column into df
  tnc <- nrow(df)
  df <- dplyr::left_join(
    df, df2,
    by = "_id")
  # safety check
  stopifnot(tnc == nrow(df))

  # simplify and replace NULL with NA
  df[[".likelyRelatedTrials"]][
    !sapply(df[[".likelyRelatedTrials"]], length)] <- NA

  # helper function, column vector
  indexSimilarX <- function(x) {

    # calculate similarities
    t <- stringdist::stringsimmatrix(
      x, x, method = c(
        # help("stringdist-metrics")
        "osa", "lv", "dl", "hamming", "lcs", "qgram",
        "cosine", "jaccard", "jw","soundex")[9])

    # blank diagonal
    diag(t) <- NA_integer_

    # find indices above threshold similarity
    apply(t, 1, function(r) {
      r <- seq_along(r)[r >= indexThreshold]
      r <- r[!is.na(r)]
      if (!length(r)) NA_integer_ else r
    }, simplify = FALSE)

  } # indexSimilarX

  # analyse trial title,
  # number of arms and substances
  df %>% dplyr::mutate(
    #
    # is title relevant
    analysis_titleRelevant = stringi::stri_detect_regex(
      .data$.trialTitle, titleDefPlatform, case_insensitive = TRUE),
    #
    #
    # TODO An ... (AG10) in ... Phase 3 ATTRibute-CM Trial (AG10-301)
    # TODO Master:A ... Disease MACARONI-23 ISA:A Phase ...
    #
    # get from title bracketed references, e.g. as per Williams RJ
    # Refining ..., TransPORTEC platform trials – p53abn - RED (RAINBO-p53abnRED) Randomized
    # ... canceR patients with ... disease (UMBRELLA)
    titleBracketedRefs = stringi::stri_replace_first_regex(
      .data$.trialTitle, "^.+[(](.*?)[)].+$", "$1"
    ),
    titleBracketedRefs = if_else(
      .data$titleBracketedRefs == .data$.trialTitle |
        nchar(.data$titleBracketedRefs) < 5L,
      NA_character_, gsub("[^a-zA-Z0-9]", "", .data$titleBracketedRefs)
    ),
    #
    # get delimited reference such as:
    # RAINBO: Refining ... features, TransPORTEC platform trials (MMRd-GREEN)
    # CARE1: FIRST LINE ... IN PATIENTS
    # RECLAIM: an Adaptive Platform Trial ... Post-Acute ... (PASC)
    # COSENSE-1: A ... precision medicine platform ...
    # NeoART – A phase Ib/II platform ... adenocarcinoma
    titleColonedRefs = stringi::stri_replace_first_regex(
      .data$.trialTitle, "^(.+?)(: | - ).+$", "$1"
    ),
    titleColonedRefs = if_else(
      .data$titleColonedRefs == .data$.trialTitle |
        nchar(.data$titleColonedRefs) < 5L |
        nchar(.data$titleColonedRefs) > 25L,
      NA_character_, gsub("[^a-zA-Z0-9]", "", .data$titleColonedRefs)
    ),
    #
    # select one of the terms found
    titleRefs = dplyr::if_else(
      !is.na(.data$titleBracketedRefs),
      .data$titleBracketedRefs, .data$titleColonedRefs
    ),
    #
    # identify rows of possibly related trials
    # 'contained in' e.g. RAINBO-p53abnRED, RAINBO
    # is realised by using Jaro Winkler distance
    .maybeRelatedTrials = indexSimilarX(.data$titleRefs),
    #
    # turn row indices to ids of possibly related trials
    .maybeRelatedTrials = mapply(
      function(a, b, c) if (all(is.na(a)))
        NA_character_ else unique(na.omit(c(.data$`_id`[a], b, c))),
      a = .data$.maybeRelatedTrials,
      b = .data$.likelyRelatedTrials,
      c = .data$`_id`,
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    ),
    #
    #
    # is number of test arms and substances relevant
    analysis_numArmsSubstancesRelevant =
      .data$.numTestArmsSubstances > minNumArmsDefPlatform
    #
  ) -> df


  #### . EUCTR ####
  df %>% dplyr::mutate(
    #
    helper_periodTitle = stringi::stri_split_fixed(
      .data$subjectDisposition.postAssignmentPeriods.postAssignmentPeriod.title,
      " / "),
    #
    analysis_numberTestPeriods = sapply(
      .data$helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    .analysis_numberTestPeriods = dplyr::case_when(
      ctrname == "EUCTR" ~ .data$analysis_numberTestPeriods)
  ) -> df


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    helper_periodTitle = stringi::stri_split_fixed(
      .data$clinical_results.participant_flow.period_list.period.title,
      " / "),
    #
    analysis_numberTestPeriods = sapply(
      .data$helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    .analysis_numberTestPeriods = dplyr::case_when(
      ctrname == "CTGOV" ~ .data$analysis_numberTestPeriods)
    #
  ) -> df


  #### . CTGOV2 ####
  df %>% dplyr::mutate(
    #
    helper_periodTitle = stringi::stri_split_fixed(
      .data$resultsSection.participantFlowModule.periods.title,
      " / "),
    #
    analysis_numberTestPeriods = sapply(
      .data$helper_periodTitle,
      function(i) {
        i <- unique(tolower(na.omit(i)))
        i <- i[!grepl(periodExclPlatform, i)]
        i <- length(i)
        if (i) i else NA_integer_
      }),
    #
    .analysis_numberTestPeriods = dplyr::case_when(
      ctrname == "CTGOV2" ~ .data$analysis_numberTestPeriods)
    #
  ) -> df


  #### . ISRCTN ####
  df %>% dplyr::mutate(
    #
    analysis_isDrugTrial =
      stringi::stri_detect_regex(
        .data$interventions.intervention.interventionType,
        "drug|biological|vaccine",
        case_insensitive = TRUE),
    #
    analysis_titleRelevant = dplyr::case_when(
      ctrname == "ISRCTN" ~ .data$analysis_isDrugTrial &
        .data$analysis_titleRelevant)
  ) -> df


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      periodTitle = dplyr::coalesce(
        as.character(.data$authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title),
        .data$authorizedApplication.authorizedPartI.trialDetails.protocolInformation.studyDesign.periodDetails.title
      ),
      #
      helper_periodTitle = stringi::stri_split_fixed(
        .data$periodTitle, " / "),
      #
      analysis_numberTestPeriods = sapply(
        .data$helper_periodTitle,
        function(i) {
          i <- unique(tolower(na.omit(i)))
          i <- i[!grepl(periodExclPlatform, i)]
          i <- length(i)
          if (i) i else 1L
        }),
      #
      .analysis_numberTestPeriods = dplyr::case_when(
        ctrname == "CTIS" ~ .data$analysis_numberTestPeriods)
      #
    ) -> df


  # summarise
  df$analysis_numPeriodsRelevant <-
    df$.analysis_numberTestPeriods > minNumPeriodsDefPlatform
  #
  df$.likelyPlatformTrial <- rowSums(
    df[, c(
      "analysis_titleRelevant",
      "analysis_numArmsSubstancesRelevant",
      "analysis_numPeriodsRelevant"
    ), drop = FALSE], na.rm = TRUE
  )
  #
  df$.likelyPlatformTrial <- (
    df$.likelyPlatformTrial / 3L) >= platformThreshold

  # keep only outcome columns
  df <- df[, c(
    "_id",
    ".likelyPlatformTrial",
    ".likelyRelatedTrials",
    ".maybeRelatedTrials"
  ), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".likelyPlatformTrial"]], "logical"))
  stopifnot(inherits(df[[".likelyRelatedTrials"]], "list"))
  stopifnot(inherits(df[[".maybeRelatedTrials"]], "list"))
  stopifnot(ncol(df) == 4L)

  # return
  return(df)

} # end f.likelyPlatformTrial
