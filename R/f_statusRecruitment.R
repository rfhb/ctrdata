#### history ####
# 2025-01-26 first version

#' Calculate status of recruitment of a study
#'
#' Trial concept calculated: status of recruitment at the time of loading
#' the trial records. Maps the categories that are in fields which specify
#' the state of recruitment. Simplifies the status into three categories.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.statusRecruitment`, which is
#' a factor with levels `ongoing` (includes active, not yet recruiting;
#' temporarily halted; suspended; authorised, not started and similar),
#' `completed` (includes ended; ongoing, recruitment ended),
#' `ended early` (includes prematurely ended, terminated early) and
#' `other` (includes revoked, withdrawn, planned, stopped).
#'
#' @export
#'
#' @importFrom dplyr if_else mutate case_when case_match pull `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.statusRecruitment()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.statusRecruitment",
#'   con = dbc)
#' trialsDf
#'
f.statusRecruitment <- function(df = NULL) {

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
      # "decisionDate",
      "authorizedApplication.memberStatesConcerned.mscName", # for counting notifications
      "mscTrialNotificationsInfoList.mscNotificationsListInfo.notificationType", # CTIS1 e.g. early termination
      "events.trialEvents.events.notificationType", # CTIS2, e.g. early termination
      "ctPublicStatusCode", # ctPublicStatusCode is in CTIS1 and CTIS2
      "ctStatus" # text in CTIS1 but in CTIS2, same as ctPublicStatusCode
    ))


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### . EUCTR ####
  df %>% dplyr::mutate(
    helper = dplyr::case_when(
      .data$trialInformation.globalEndOfTrialPremature ~ "Prematurely Ended",
      .data$trialInformation.isGlobalEndOfTrialReached ~ "Completed",
      .default = as.character(.data$p_end_of_trial_status)
    ),
    out = tolower(.data$helper)
  ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    helper = as.character(
      # type is logical if all NA
      dplyr::if_else(
        !is.na(.data$last_known_status),
        .data$last_known_status,
        .data$overall_status)
    ),
    out = tolower(.data$helper)
  ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  df$ctgov2 <- tolower(as.character(
    # type is logical if all NA
    # "trial is terminated (that is, stopped prematurely)"
    df$protocolSection.statusModule.overallStatus))


  #### . ISRCTN ####
  df %>% dplyr::mutate(
    helper = dplyr::case_when(
      !is.na(.data$participants.recruitmentStatusOverride) ~
        as.character(.data$participants.recruitmentStatusOverride),
      Sys.Date() > .data$participants.recruitmentEnd ~ "Completed",
      Sys.Date() < .data$participants.recruitmentStart ~ "Planned",
      .data$participants.recruitmentEnd > .data$participants.recruitmentStart ~ "Ongoing"
    ),
    out = tolower(.data$helper)
  ) %>%
    dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  df %>% dplyr::mutate(
    helper_ctPublicStatusCode = dplyr::case_match(
      as.integer(.data$ctPublicStatusCode),
      1 ~ "Under evaluation",
      2 ~ "Authorised, recruitment pending",
      3 ~ "Authorised, recruiting",
      4 ~ "Ongoing, recruiting",
      5 ~ "Ongoing, recruitment ended",
      6 ~ "Temporarily halted",
      7 ~ "Suspended",
      8 ~ "Ended", # includes early termination
      9 ~ "Expired",
      10 ~ "Revoked",
      11 ~ "Not authorised",
      12 ~ "Cancelled"
    ),
    helper_event1 = sapply(
      .data$mscTrialNotificationsInfoList.mscNotificationsListInfo.notificationType,
      function(i) grepl("^(|Global end of trial / )Early Termination", i, ignore.case = TRUE)
    ),
    helper_event2 = sapply(
      .data$events.trialEvents.events.notificationType,
      function(i) stringi::stri_count_fixed(paste0(i, collapse = " "), "EARLY_TERMINATION")
    ),
    helper_event3 = sapply(
      stringi::stri_split_fixed(
        .data$authorizedApplication.memberStatesConcerned.mscName,
        " / "), function(i) if (all(is.na(i))) NA else length(i)
    ),
    helper = dplyr::case_when(
      .data$helper_event1 ~ "terminated early",
      .data$helper_event2 == .data$helper_event3 ~ "terminated early",
      is.na(.data$helper_ctPublicStatusCode) &
        !is.na(.data$ctStatus) ~ as.character(.data$ctStatus),
      .default = as.character(.data$helper_ctPublicStatusCode)
    ),
    out = tolower(.data$helper)
  ) %>%
    dplyr::pull("out") -> df$ctis


  # merge, last update 2025-02-08
  mapped_values <- list(
    "ongoing" = c(
      "active", "active_not_recruiting", "active, not recruiting",
      "authorised, not started", "authorised, recruiting",
      "authorised, recruitment pending", "enrolling by invitation",
      "enrolling_by_invitation", "not yet recruiting", "ongoing",
      "ongoing, not yet recruiting", "ongoing, recruiting", "recruiting",
      "restarted", "suspended", "temporarily halted", "temporarily_not_available"
    ),
    #
    "completed" = c(
      "completed", "ended", "ongoing, recruitment ended"
    ),
    #
    "ended early" = c(
      "prematurely ended", "terminated early", "terminated"
    ),
    #
    "other" = c(
      "cancelled", "expired", "gb - no longer in eu/eea", "no longer available",
      "no_longer_available", "not authorised", "revoked",
      "stopped", "trial now transitioned", "under evaluation",
      "unknown", "withdrawn", "withheld"
    )
  )

  # check for unmapped values:
  # setdiff(unique(dfMergeVariablesRelevel(df, names(fldsNeeded))), unlist(mapped_values))


  # merge into vector (factor)
  df[[".statusRecruitment"]] <- dfMergeVariablesRelevel(
    df = df,
    colnames = names(fldsNeeded),
    levelslist = mapped_values
  )

  # keep only outcome columns
  df <- df[, c("_id", ".statusRecruitment"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".statusRecruitment"]], "factor"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.statusRecruitment
