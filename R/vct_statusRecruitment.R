# function definition for dfCalculate

#### history ####
# 2025-01-26 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when case_match pull `%>%`
.statusRecruitment <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "trialInformation.globalEndOfTrialPremature",
      "trialInformation.isGlobalEndOfTrialReached",
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
      # "ctPublicStatus",
      # "ctStatus"
      "ctPublicStatusCode"
    ))


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

  # helper function
  `%>%` <- dplyr::`%>%`


  #### .EUCTR ####
  df %>% dplyr::mutate(
    out = dplyr::case_when(
      trialInformation.globalEndOfTrialPremature ~ "Prematurely Ended",
      trialInformation.isGlobalEndOfTrialReached ~ "Completed",
      .default = p_end_of_trial_status
    )
  ) %>%
    dplyr::pull(out) -> df$euctr


  #### .CTGOV ####
  df %>% dplyr::mutate(
    out = as.character(
      # type is logical if all NA
      dplyr::if_else(
        !is.na(last_known_status),
        last_known_status, overall_status)
    )
  ) %>%
    dplyr::pull(out) -> df$ctgov

  #### .CTGOV2 ####
  df$ctgov2 <- as.character(
    # type is logical if all NA
    df$protocolSection.statusModule.overallStatus)

  #### .ISRCTN ####
  df %>% dplyr::mutate(
    out = dplyr::case_when(
      !is.na(participants.recruitmentStatusOverride)
      ~ as.character(participants.recruitmentStatusOverride),
      Sys.Date() >
        participants.recruitmentEnd ~ "Completed",
      participants.recruitmentEnd >
        participants.recruitmentStart ~ "Ongoing",
      Sys.Date() <
        participants.recruitmentStart ~ "Planned"
    )
  ) %>%
    dplyr::pull(out) -> df$isrctn

  #### .CTIS ####
  df %>% dplyr::mutate(
    helper_ctPublicStatusCode = dplyr::case_match(
      ctPublicStatusCode,
      1 ~ "Under evaluation",
      2 ~ "Authorised, recruitment pending",
      3 ~ "Authorised, recruiting",
      4 ~ "Ongoing, recruiting",
      5 ~ "Ongoing, recruitment ended",
      6 ~ "Temporarily halted",
      7 ~ "Suspended",
      8 ~ "Ended",
      9 ~ "Expired",
      10 ~ "Revoked",
      11 ~ "Not authorised",
      12 ~ "Cancelled"
    ),
    out = helper_ctPublicStatusCode
  ) %>%
    dplyr::pull(out) -> df$ctis


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
      "Ongoing, recruiting", "Temporarily halted",
      "Ongoing, not yet recruiting", "Authorised, not started",
      "Authorised, recruitment pending", "Authorised, recruiting"
    ),
    #
    "completed" = c(
      "Ongoing, recruitment ended",
      "Completed", "COMPLETED",
      "Ended"),
    #
    "other" = c(
      "Under evaluation", "Not Authorised", "Not authorised",
      "Expired", "Revoked", "Cancelled", "Withdrawn",
      "GB - no longer in EU/EEA", "Trial now transitioned",
      "Suspended", "No longer available",
      "SUSPENDED", "NO_LONGER_AVAILABLE",
      "WITHDRAWN", "WITHHELD", "UNKNOWN",
      "Terminated", "TERMINATED", "Prematurely Ended", "Stopped")
  )

  # merge into vector (ordered factor)
  vct <- dfMergeVariablesRelevel(
    df = df,
    colnames = names(fldsNeeded),
    levelslist = mapped_values
  )


  #### checks ####
  stopifnot(is.factor(vct))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .statusRecruitment
