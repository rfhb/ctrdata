#### history ####
# 2025-01-26 first version

#' Calculate objectives of a study
#'
#' Trial concept calculated: objectives of the trial, by searching for text
#' fragments found in fields describing its purpose, objective, background
#' or hypothesis, after applying .isMedIntervTrial, because the text
#' fragments are tailored to medicinal product interventional trials.
#' This is a simplification, and it is expected that the criteria will be
#' further refined. The text fragments only apply to English.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.trialObjectives`, which is
#' a string with letters separated by a space, such as
#' E (efficacy, including cure, survival, effectiveness);
#' A (activity, including reponse, remission, seroconversion);
#' S (safety); PK; PD (including biomarker);
#' D (dose-finding, determining recommended dose);
#' LT (long-term); and FU (follow-up).
#'
#' @export
#'
#' @importFrom dplyr if_else mutate case_when rename left_join `%>%`
#'
#' @examples
#' # fields needed
#' f.trialObjectives()
#'
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.trialObjectives",
#'   con = dbc)
#' }
#'
f.trialObjectives <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsHere <- list(
    "euctr" = c(
      "e21_main_objective_of_the_trial" # free text
      # "trialInformation.mainObjective", # incomplete, results
      # "x4_clinical_trial_type", # e.g. outside EU/EEA, EEA CTA
      # "e7131_other_trial_type_description" # rarely filled, free text short
    ),
    "ctgov" = c(
      "detailed_description.textblock", # free text but more background than purpose
      "study_design_info.primary_purpose" # keyword
    ),
    "ctgov2" = c(
      "protocolSection.descriptionModule.detailedDescription", # free text clearly stating purposes
      "protocolSection.designModule.designInfo.primaryPurpose" # keyword, e.g. TREATMENT
    ),
    "isrctn" = c(
      "trialDescription.studyHypothesis", # free text
      "trialDesign.trialType" # keywords, e.g. Treatment
    ),
    "ctis" = c(
      # CTIS1
      "authorizedPartI.trialDetails.trialInformation.trialObjective.mainObjective", # free text
      "authorizedPartI.trialDetails.trialInformation.trialObjective.trialScopes.otherDescription", # keywords, rarely filled
      # CTIS2
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialObjective.mainObjective", # free text
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialObjective.trialScopes.otherDescription" # keywords, rarely filled
    ))

  # merge with fields needed for nested function
  fldsAdded <- suppressMessages(f.isMedIntervTrial())
  fldsNeeded <- sapply(names(fldsHere), function(i) na.omit(c(
    fldsHere[[i]], fldsAdded[[i]])), simplify = FALSE)


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # apply nested function which provides values for each register
  # therefore the following code needs to check against register
  # TODO
  # df$isMedIntervTrial <- f.isMedIntervTrial(
  #   df = df)[[".isMedIntervTrial"]]
  df <- dplyr::left_join(
    df, f.isMedIntervTrial(df = df), by = "_id")

  # helper function
  `%>%` <- dplyr::`%>%`

  # helper function similar to unite
  pasteCols <- function(...) apply(
    ..., 1, function(i) paste(na.omit(i), collapse = " "))


  # all registers


  #### . CTGOV ####
  # https://clinicaltrials.gov/data-api/about-api/api-migration#model
  # Treatment
  # Prevention
  # Diagnostic
  # Educational/Counseling/Training
  # Supportive Care
  # Screening
  # Health Services Research
  # Basic Science
  # Device Feasibility
  # Other

  #### . CTGOV2 ####
  # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-types-section
  # TREATMENT - Treatment
  # PREVENTION - Prevention
  # DIAGNOSTIC - Diagnostic
  # ECT - Educational/Counseling/Training
  # SUPPORTIVE_CARE - Supportive Care
  # SCREENING - Screening
  # HEALTH_SERVICES_RESEARCH - Health Services Research
  # BASIC_SCIENCE - Basic Science
  # DEVICE_FEASIBILITY - Device Feasibility
  # OTHER - Other

  #### . ISRCTN ####
  # trialDesign.trialType
  # Treatment
  # Prevention
  # Diagnostic
  # Other
  # Quality of life
  # Not Specified


  # merge column contents
  df %>%
    dplyr::select(unlist(fldsHere, use.names = FALSE)) %>%
    dplyr::mutate(txt = pasteCols(.), txt = gsub("NA.?", "", txt)) %>%
    dplyr::pull(txt) -> df$txt

  # identify symbols
  df %>%
    dplyr::select(
      `_id`, txt, .isMedIntervTrial) %>%
    dplyr::mutate(
      addObjectives = "",
      # symbols are accumulated
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl("efficac|efficien|effective|benefit|survival| cure|protection|death| OS ", txt, TRUE), "E ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl("safety|tolerabil|mtd|side effect|feasibility|AES|adverse ", txt, TRUE), "S ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl(paste0(
          "pharmacodynam|mtd|recommended dose|function|biomarker|improvement|",
          "expression|immunohistochemistry|IHC|reduction|level.? of"
        ), txt, TRUE), "PD ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl("pharmacokine|pk |absor[bp]", txt, TRUE), "PK ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl("long.?term|long.?last", txt, TRUE), "LT ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl("follow.?up", txt, TRUE), "FU ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl(paste0(
          "response|activity|progression|immunogenic|remission|seroconversion|EFS|PFS|DFS|RFS|CR[ \\)]|",
          "time on|time to|time in|recurrence|CCR|ORR|incidence|score|respond|quality of life|PRO|",
          "disabilit|control|immunity"
        ), txt, TRUE), "A ", "")),
      addObjectives = paste0(addObjectives, dplyr::if_else(
        grepl("dose.find|dose.range|rptd|determine.*dose|dose.determ|rp2d|recommended dose", txt, TRUE), "D ", "")),
      #
      out = dplyr::case_when(.isMedIntervTrial ~ trimws(addObjectives), .default = NA_character_)
      #
    ) %>%
    dplyr::rename(.trialObjectives = out) %>%
    dplyr::select(`_id`, .trialObjectives) -> df
  # dplyr::pull(out) -> vct


  #### checks ####
  stopifnot(inherits(df[[".trialObjectives"]], "character"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.trialObjectives
