#### history ####
# 2025-08-09 first version

#' Calculate phase of a clinical trial
#'
#' Trial concept calculated: phase of a clinical trial as per ICH E8(R1).
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.trialPhase`, which is
#' an ordered factor with levels `phase 1`, `phase 1+2`, `phase 2`,
#' `phase 2+3`, `phase 2+4`, `phase 3`, `phase 3+4`, `phase 1+2+3`,
#' `phase 4`, `phase 1+2+3+4`.
#'
#' @export
#'
#' @importFrom dplyr if_else mutate case_when `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.trialPhase()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.trialPhase",
#'   con = dbc)
#' trialsDf
#'
f.trialPhase <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e71_human_pharmacology_phase_i",
      "e72_therapeutic_exploratory_phase_ii",
      "e73_therapeutic_confirmatory_phase_iii",
      "e74_therapeutic_use_phase_iv"
    ),
    "ctgov" = c(
      "phase"
    ),
    "ctgov2" = c(
      "protocolSection.designModule.phases"
    ),
    "isrctn" = c(
      "interventions.intervention.phase"
    ),
    "ctis" = c(
      "authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase",
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase"
    ))


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  df <- fctChkFlds(df, fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      euctrphase = case_when(
        .data$e71_human_pharmacology_phase_i &
          .data$e72_therapeutic_exploratory_phase_ii &
          .data$e73_therapeutic_confirmatory_phase_iii ~ "phase 1+2+3",
        .data$e71_human_pharmacology_phase_i &
          .data$e72_therapeutic_exploratory_phase_ii ~ "phase 1+2",
        .data$e72_therapeutic_exploratory_phase_ii &
          .data$e73_therapeutic_confirmatory_phase_iii ~ "phase 2+3",
        .data$e71_human_pharmacology_phase_i ~ "phase 1",
        .data$e72_therapeutic_exploratory_phase_ii ~ "phase 2",
        .data$e73_therapeutic_confirmatory_phase_iii ~ "phase 3",
        .data$e74_therapeutic_use_phase_iv ~ "phase 4",
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("euctrphase") -> df$euctrphase

  #### . CTIS ####
  df$authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase <-
    as.character(df$authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase)
  df$authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase <-
    as.character(df$authorizedApplication.authorizedPartI.trialDetails.trialInformation.trialCategory.trialPhase)
  #
  # ctis value list
  #  1 Human Pharmacology (Phase I) - First administration to humans
  #  2 Human Pharmacology (Phase I) - Bioequivalence Study
  #  3 Human Pharmacology (Phase I) - Other
  #  4 Therapeutic exploratory (Phase II)
  #  5 Therapeutic confirmatory (Phase III)
  #  6 Therapeutic use (Phase IV)
  #  7 Phase I and Phase II (Integrated) - First administration to humans
  #  8 Phase I and Phase II (Integrated) - Bioequivalence Study
  #  9 Phase I and Phase II (Integrated) - Other
  # 10 Phase II and Phase III (Integrated)
  # 11 Phase III and phase IV (Integrated)


  # calculate for all
  phase_values <- list(
    "phase 1" = c("Early Phase 1", "Phase 1", "Phase I",
                  "PHASE1", "1", "2", "3"),
    "phase 1+2" = c("Phase 1/Phase 2", "Phase I/II",
                    "Phase I and Phase II (Integrated)- Other",
                    "7", "8", "9", "PHASE1 / PHASE2"),
    "phase 2" = c("Phase 2", "Phase II", "PHASE2",
                  "Therapeutic exploratory (Phase II)", "4"),
    "phase 2+3" = c("Phase 2/Phase 3", "Phase II/III", "PHASE2 / PHASE3",
                    "Phase II and Phase III (Integrated)", "10"),
    "phase 2+4" = c("Phase 2/Phase 4", "Phase II/IV",
                    "PHASE2 / PHASE4"),
    "phase 3" = c("Phase 3", "Phase III",
                  "Therapeutic confirmatory (Phase III)",
                  "PHASE3", "5"),
    "phase 3+4" = c("Phase 3/Phase 4", "Phase III/IV",
                    "PHASE3 / PHASE4", "11"),
    "phase 1+2+3" = c("Phase 1/Phase 2/Phase 3",
                      "PHASE1 / PHASE2 / PHASE3"),
    "phase 4" = c("Phase 4", "Phase IV", "Therapeutic use (Phase IV)",
                  "PHASE4", "6"),
    "phase 1+2+3+4" = c("Phase 1/Phase 2/Phase 3/Phase 4",
                        "PHASE1 / PHASE2 / PHASE3 / PHASE4")
    # returned as NA = c("Not Applicable", "Not Specified")
  )

  # merge
  df[[".trialPhase"]] <- ordered(
    dfMergeVariablesRelevel(
      df = df,
      colnames = c(
        "euctrphase",
        unlist(fldsNeeded[-1], use.names = FALSE)
      ),
      levelslist = phase_values
    ),
    levels = names(phase_values)
  )
  # keep only outcome columns
  df <- df[, c("_id", ".trialPhase"), drop = FALSE]

  # check missing levels:
  # setdiff(unique(vct), names(phase_values))

  #### checks ####
  stopifnot(inherits(df[[".trialPhase"]], "ordered"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.trialPhase
