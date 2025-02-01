# function definition for dfCalculate

#### history ####
# 2025-01-26 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate
.startDate <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "n_date_of_competent_authority_decision",
      "trialInformation.recruitmentStartDate"
    ),
    "ctgov" = c(
      "start_date"
    ),
    "ctgov2" = c(
      "protocolSection.statusModule.startDateStruct.date"
    ),
    "isrctn" = c(
      "participants.recruitmentStart",
      "trialDesign.overallStartDate"
    ),
    "ctis" = c(
      "startDateEU",
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate"
    ))

  # not relevant after inspection:
  #
  # ISRCTN
  # "trialDesign.overallStartDate"


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the date of start of the trial, based on the start of recruitment,
of if not available, on the opinion of the competent authority.
Returns a date.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # mangle more than one field per register
  df$euctr <- dplyr::mutate(
    df,
    out = dplyr::if_else(
      !is.na(trialInformation.recruitmentStartDate),
      trialInformation.recruitmentStartDate,
      n_date_of_competent_authority_decision
    )
  )[["out"]]

  # mangle more than one field per register
  df$isrctn <- dplyr::mutate(
    df,
    out = dplyr::if_else(
      !is.na(participants.recruitmentStart),
      participants.recruitmentStart,
      trialDesign.overallStartDate
    )
  )[["out"]]

  # mangle more than one field per register
  df$ctis <- dplyr::mutate(
    df,
    out = dplyr::if_else(
      !is.na(startDateEU),
      startDateEU,
      authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialDuration.estimatedRecruitmentStartDate
    )
  )[["out"]]

  # keep only single fields per register or
  # have been mangled to new single field
  fldsIndicator <- sapply(fldsNeeded, length) > 1L
  fldsNeeded <- ifelse(fldsIndicator, names(fldsNeeded), fldsNeeded)

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = unlist(fldsNeeded, use.names = FALSE)
  )


  #### checks ####
  stopifnot(inherits(vct, "Date") || inherits(vct, "POSIXct"))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .startDate
