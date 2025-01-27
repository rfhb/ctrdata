# function definition for dfCalculate

#### history ####
# 2025-01-27 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate rowwise ungroup `%>%`
.isMedIntervenTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "ctrname"
    ),
    "ctgov" = c(
      "intervention.intervention_type",
      "study_type"
    ),
    "ctgov2" = c(
      "protocolSection.armsInterventionsModule.interventions.type",
      "protocolSection.designModule.studyType"
    ),
    "isrctn" = c(
      "interventions.intervention.interventionType"
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

For EUCTR and CTIS, this corresponds to the clinical trials as per the
definition of the EU Clinical Trial Regulation.

For CTGOV and CTGOV2, this is based


and on the variable isFDARegulated, available since 2017.


Builds on Lasch-F et al. https://doi.org/10.1002/cpt.2534

Returns a logical.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(unlist(fldsNeeded)))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)


  #### ..EUCTR ####
  # all in EUCTR correspond to definition
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_isDrugTrial = TRUE,
      out = dplyr::if_else(ctrname == "EUCTR", analysis_isDrugTrial, NA)
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$euctr


  #### ..CTGOV ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_isDrugTrial = grepl(
        "drug|biological",
        intervention.intervention_type,
        ignore.case = TRUE
      ) &
        grepl(
          "interventional",
          study_type,
          ignore.case = TRUE
        ),
      out = dplyr::if_else(is.na(intervention.intervention_type), NA, analysis_isDrugTrial)
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctgov


  #### ..CTGOV2 ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_isDrugTrial = grepl(
        "drug|biological",
        protocolSection.armsInterventionsModule.interventions.type,
        ignore.case = TRUE
      ) &
        grepl(
          "interventional",
          protocolSection.designModule.studyType,
          ignore.case = TRUE
        ),
      out = dplyr::if_else(is.na(protocolSection.designModule.studyType), NA, analysis_isDrugTrial)
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctgov2


  #### ..ISRCTN ####
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_isDrugTrial = grepl(
        "drug",
        interventions.intervention.interventionType,
        ignore.case = TRUE
      ),
      out = dplyr::if_else(is.na(interventions.intervention.interventionType), NA, analysis_isDrugTrial)
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$isrctn


  #### ..CTIS ####
  # all in EUCTR correspond to definition
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      analysis_isDrugTrial = TRUE,
      out = dplyr::if_else(ctrname == "CTIS", analysis_isDrugTrial, NA)
    ) %>%
    dplyr::ungroup() %>%
    .[["out"]] -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  )


  #### checks ####
  stopifnot(is.logical(vct))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .isMedIntervenTrial
