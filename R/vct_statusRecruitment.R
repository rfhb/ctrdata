# function definition for dfCalculate

#### history ####
# 2025-01-26 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when
.statusRecruitment <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "p_end_of_trial_status"
      #
      # in some trials, e.g. 2014-003556-31-GB,
      # this fields includes a text description
      # while p_end_of_trial_status is empty:
      # "subjectDisposition.recruitmentDetails"
    ),
    "ctgov" = c(
      "last_known_status",
      "overall_status"
    ),
    "ctgov2" = c(
      "protocolSection.statusModule.overallStatus"
    ),
    "isrctn" = c(
      "participants.recruitmentEnd",
      "participants.recruitmentStart",
      "participants.recruitmentStatusOverride"
    ),
    "ctis" = c(
      "ctPublicStatus"
    ))

  # not relevant after inspection:
  #
  # EUCTR
  # "x5_trial_status",
  # "subjectDisposition.recruitmentDetails",
  # CTGOV
  # "clinical_results.participant_flow.recruitment_details",
  # CTGOV2
  # "protocolSection.statusModule.whyStopped",
  # "resultsSection.outcomeMeasuresModule.outcomeMeasures.reportingStatus",
  # "resultsSection.participantFlowModule.recruitmentDetails",
  # ISRCTN
  # "trialDesign.overallStatusOverride",
  # CTIS
  # "ctPublicStatusCode",
  # "ctStatus",
  # "recruitmentStatus"


  #### describe ####
  if (is.null(df)) {

    txt <- '
Calculates the status at the time of loading the trial records.
Maps the categories that are in fields which specify the state of recruitment.
Simplifies them into categories "ongoing", "completed" and "other."
Returns an ordered factor.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # mangle more than one field per register
  df$ctgov <- dplyr::mutate(
    df,
    out = dplyr::if_else(
      !is.na(last_known_status),
      last_known_status, overall_status)
  )[["out"]]

  # mangle more than one field per register
  df$isrctn <- dplyr::mutate(
    df,
    out = dplyr::case_when(
      !is.na(participants.recruitmentStatusOverride) ~
        participants.recruitmentStatusOverride,
      Sys.Date() >
        participants.recruitmentEnd ~ "Completed",
      participants.recruitmentEnd >
        participants.recruitmentStart ~ "Ongoing",
      Sys.Date() <
        participants.recruitmentStart ~ "Planned"
    )
  )[["out"]]

  # merge, last update 2025-01-27
  mapped_values <- list(
    "ongoing" = c(
      # EUCTR
      "Recruiting", "Active", "Ongoing",
      "Temporarily Halted", "Restarted",
      # CTGOV
      "Active, not recruiting", "Enrolling by invitation",
      "Not yet recruiting",
      # CTGOV2
      "ACTIVE_NOT_RECRUITING", "ENROLLING_BY_INVITATION",
      "RECRUITING", "TEMPORARILY_NOT_AVAILABLE",
      # ISRCTN
      "Ongoing",
      # CTIS
      "Ongoing, recruiting", "Ongoing, recruitment ended",
      "Ongoing, not yet recruiting", "Authorised, not started"
    ),
    #
    "completed" = c(
      "Completed", "COMPLETED",
      "Ended"),
    #
    "other" = c(
      "GB - no longer in EU/EEA", "Trial now transitioned",
      "Withdrawn", "Suspended", "No longer available",
      "SUSPENDED", "NO_LONGER_AVAILABLE", "WITHDRAWN",
      "WITHHELD", "UNKNOWN",
      "Terminated", "TERMINATED", "Prematurely Ended",
      "Stopped",
      "Under evaluation")
  )

  # keep only single fields per register or
  # have been mangled to new single field
  fldsIndicator <- sapply(fldsNeeded, length) > 1L
  fldsNeeded <- ifelse(fldsIndicator, names(fldsNeeded), fldsNeeded)

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = unlist(fldsNeeded, use.names = FALSE),
    levelslist = mapped_values
  )


  #### checks ####
  stopifnot(is.factor(vct))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .statusRecruitment
