# function definition for dfCalculate

#### history ####
# 2025-01-27 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else case_when mutate pull `%>%`
#' @importFrom stringi stri_detect_regex stri_detect_fixed
.isMedIntervTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "ctrname"
    ),
    "ctgov" = c(
      # "oversight_info.is_fda_regulated_drug",
      "intervention.intervention_type",
      "study_type"
    ),
    "ctgov2" = c(
      # "protocolSection.oversightModule.isFdaRegulatedDrug",
      "protocolSection.armsInterventionsModule.interventions.type",
      "protocolSection.designModule.studyType"
    ),
    "isrctn" = c(
      # "trialDesign.trialType",
      "interventions.intervention.interventionType",
      "trialDesign.primaryStudyDesign"
    ),
    "ctis" = c(
      "ctrname"
    ))

  # not relevant after inspection:
  #
  # CTGOV
  # "study_design_info.intervention_model" e.g. parallel
  # "study_design_info.allocation",
  # "study_design_info.primary_purpose",
  #
  # CTGOV2
  # "protocolSection.armsInterventionsModule.armGroups.type" not specific
  # "protocolSection.oversightModule.isFdaRegulatedDrug" often empty
  # "protocolSection.oversightModule.isFdaRegulatedDevice"
  #
  # ISRCTN
  # "interventions.intervention.description"


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates if record is a medicine-interventional trial, investigating one or
more medicine, whether biological or not.

For EUCTR and CTIS, this corresponds to all records as per the
definition of the EU Clinical Trial Regulation.

For CTGOV and CTGOV2, this is based on drug or biological as type
of intervention, and interventional as type of study.

For ISRCTN, this is based on drug or biological as type
of intervention, and interventional as type of study.

Returns a logical.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### ..EUCTR ####
  # all in EUCTR correspond to definition
  df %>% dplyr::mutate(
    #
    analysis_isDrugTrial =
      dplyr::case_when(ctrname == "EUCTR" ~ TRUE),
    #
    out = analysis_isDrugTrial
  ) %>%
    dplyr::pull(out) -> df$euctr


  #### ..CTGOV ####
  df %>% dplyr::mutate(
    #
    analysis_isDrugTrial =
      stringi::stri_detect_regex(
        intervention.intervention_type,
        "drug|biological", case_insensitive = TRUE) &
      stringi::stri_detect_fixed(
        study_type,
        "interventional", case_insensitive = TRUE),
    #
    out = analysis_isDrugTrial
  ) %>%
  dplyr::pull(out) -> df$ctgov


  #### ..CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      analysis_isDrugTrial =
        stringi::stri_detect_regex(
          protocolSection.armsInterventionsModule.interventions.type,
          "drug|biological", case_insensitive = TRUE) &
        stringi::stri_detect_fixed(
          protocolSection.designModule.studyType,
          "interventional", case_insensitive = TRUE),
      #
      out = analysis_isDrugTrial
    ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### ..ISRCTN ####
  df %>%
    dplyr::mutate(
      #
      analysis_isDrugTrial =
        stringi::stri_detect_fixed(
          interventions.intervention.interventionType,
          "drug", case_insensitive = TRUE) &
        stringi::stri_detect_regex(
          trialDesign.primaryStudyDesign,
          "interventional|biological|vaccine", case_insensitive = TRUE),
      #
      out = analysis_isDrugTrial
    ) %>%
    dplyr::pull(out) -> df$isrctn


  #### ..CTIS ####
  # all in EUCTR correspond to definition
  df %>% dplyr::mutate(
    #
    analysis_isDrugTrial =
      dplyr::case_when(ctrname == "CTIS" ~ TRUE),
    #
    out = analysis_isDrugTrial
  ) %>%
    dplyr::pull(out) -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  )


  #### checks ####
  stopifnot(is.logical(vct) || all(is.na(vct)))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .isMedIntervTrial
