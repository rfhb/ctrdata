#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved

#' Calculate type of control data collected in a study
#'
#' Trial concept calculated: type or class of the lead or main sponsor of the
#' trial. Some information is not yet mapped (e.g., "NETWORK" in CTGOV2).
#' No specific field is available in ISRCTN. If several sponsors, sponsor type
#' is deemed `for profit` if any sponsor is commercial.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.sponsorType`, which is
#' a factor with levels `for profit`, `not for profit` or `other`.
#'
#' @export
#'
#' @importFrom dplyr mutate case_when case_match coalesce pull `%>%`
#' @importFrom stringi stri_split_fixed
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.sponsorType()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.sponsorType",
#'   con = dbc)
#' trialsDf
#'
f.sponsorType <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "b1_sponsor.b31_and_b32_status_of_the_sponsor"
    ),
    "ctgov" = c(
      "sponsors.lead_sponsor.agency_class"
    ),
    "ctgov2" = c(
      # see also Enumeration types
      # https://clinicaltrials.gov/data-api/about-api/study-data-structure
      # which shows for
      # protocolSection.sponsorCollaboratorsModule.leadSponsor.class
      # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-AgencyClass
      "protocolSection.sponsorCollaboratorsModule.leadSponsor.class"
    ),
    "isrctn" = c(
      "ctrname"
    ),
    "ctis" = c(
      # CTIS1
      "sponsorType",
      "primarySponsor.commercial",
      # CTIS2
      "authorizedApplication.authorizedPartI.sponsors.isCommercial"
    ))

  # helper definitions
  # sponsor type
  stc <- "for profit"
  stn <- "not for profit"
  sto <- "other"

  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)


  #### . EUCTR ####
  df %>% dplyr::mutate(
    #
    helper = strsplit(
      as.character(.data$b1_sponsor.b31_and_b32_status_of_the_sponsor),
      split = " / "),
    #
    out = sapply(
      helper, function(r) {
        if (all(is.na(r))) return(NA_character_)
        if (any(r == "Commercial")) return(stc)
        if (all(r == "Non-Commercial")) return(stn)
        return(NA_character_)
      }
    )
  ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    out = dplyr::case_match(
      as.character(.data$sponsors.lead_sponsor.agency_class),
      c("NIH", "U.S. Fed") ~ stn,
      c("Industry") ~ stc,
      c("Indiv", "Ambig", "Other", "Unknown") ~ sto,
      .default = NA_character_
    )
  ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  #
  # https://clinicaltrials.gov/data-api/about-api/study-data-structure
  # protocolSection.sponsorCollaboratorsModule.leadSponsor.class:
  # NIH - NIH
  # FED - FED
  # OTHER_GOV - OTHER_GOV
  # INDIV - INDIV
  # INDUSTRY - INDUSTRY
  # NETWORK - NETWORK
  # AMBIG - AMBIG
  # OTHER - OTHER
  # UNKNOWN - UNKNOWN
  #
  df %>% dplyr::mutate(
    #
    out = dplyr::case_match(
      as.character(.data$protocolSection.sponsorCollaboratorsModule.leadSponsor.class),
      c("NIH", "FED", "OTHER_GOV") ~ stn,
      c("INDUSTRY") ~ stc,
      c("INDIV", "AMBIG", "OTHER", "UNKNOWN") ~ sto,
      .default = NA_character_
    )
  ) %>%
    dplyr::pull("out") -> df$ctgov2


  #### . ISRCTN ####
  df %>% dplyr::mutate(
    #
    out = dplyr::case_match(
      as.character(.data$ctrname),
      "ISRCTN" ~ sto,
      .default = NA_character_
    )
  ) %>%
    dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  ncs <- c(
    "Hospital/Clinic/Other health care facility",
    "Patient organisation/association",
    "Educational Institution",
    "Health care"
  )
  cos <- c(
    "Industry",
    "Laboratory/Research/Testing facility",
    "Pharmaceutical company",
    "Non-Pharmaceutical company",
    "Pharmaceutical association/federation"
  )
  #
  df %>%
    dplyr::mutate(
      helper1 = !sapply(
        # seems systematically filled
        .data$authorizedApplication.authorizedPartI.sponsors.isCommercial, any),
      helper2 = sapply(
        # CTIS1?
        .data$primarySponsor.commercial,
        function(i) i == "Non-Commercial"),
      helper3 = sapply(
        stringi::stri_split_fixed(
          # filled only sometimes
          .data$sponsorType, ", "),
        function(i) if (all(is.na(i))) NA else any(i %in% ncs)
      ),
      # sequence matters, first
      # value determines result
      helper4 = dplyr::coalesce(
        .data$helper3, .data$helper2, .data$helper1
      ),
      #
      out = dplyr::case_when(
        .data$helper4 ~ stn,
        !.data$helper4 ~ stc,
        !is.na(.data$helper4) ~ sto,
        .default = NA_character_
      )
    ) %>%
    dplyr::pull("out") -> df$ctis


  # keep only register names
  fldsNeeded <- names(fldsNeeded)
  fldsNeeded <- intersect(fldsNeeded, names(df))

  # merge into vector (ordered factor)
  df[[".sponsorType"]] <- factor(
    dfMergeVariablesRelevel(
      df = df,
      colnames = fldsNeeded
    ),
    levels = c(stn, stc, sto)
  )

  # keep only outcome columns
  df <- df[, c("_id", ".sponsorType"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".sponsorType"]], "factor"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .sponsorType
