#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved

#' Calculate type of control data collected in a study
#'
#' Trial concept calculated: type or class of the lead or main sponsor of the
#' trial. Some information is not yet mapped (e.g., "NETWORK" in CTGOV2).
#' No specific field is available in ISRCTN.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.sponsorType`, which is
#' a factor with levels `For profit`, `Not for profit` or `Other`.
#'
#' @export
#'
#' @importFrom dplyr mutate case_when case_match coalesce pull `%>%`
#' @importFrom stringi stri_split_fixed
#'
#' @examples
#' # fields needed
#' f.sponsorType()
#'
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.sponsorType",
#'   con = dbc)
#' }
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
  stc <- "For profit"
  stn <- "Not for profit"
  sto <- "Other"

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
    out = dplyr::case_when(
      b1_sponsor.b31_and_b32_status_of_the_sponsor == "Commercial" ~ stc,
      b1_sponsor.b31_and_b32_status_of_the_sponsor == "Non-Commercial" ~ stn
    )
  ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####
  df %>% dplyr::mutate(
    #
    out = dplyr::case_match(
      sponsors.lead_sponsor.agency_class,
      c("NIH", "U.S. Fed") ~ stn,
      c("Industry") ~ stc,
      c("Indiv", "Ambig", "Other", "Unknown") ~ sto
    )
  ) %>%
    dplyr::pull(out) -> df$ctgov


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
      protocolSection.sponsorCollaboratorsModule.leadSponsor.class,
      c("NIH", "FED", "OTHER_GOV") ~ stn,
      c("INDUSTRY") ~ stc,
      c("INDIV", "AMBIG", "OTHER", "UNKNOWN") ~ sto
    )
  ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### . ISRCTN ####
  df %>% dplyr::mutate(
    #
    out = dplyr::case_match(
      ctrname,
      "ISRCTN" ~ sto
    )
  ) %>%
    dplyr::pull(out) -> df$isrctn


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
        authorizedApplication.authorizedPartI.sponsors.isCommercial, any),
      helper2 = sapply(
        # CTIS1?
        primarySponsor.commercial,
        function(i) i == "Non-Commercial"),
      helper3 = sapply(
        stringi::stri_split_fixed(
          # filled only sometimes
          df$sponsorType, ", "),
        function(i) if (all(is.na(i))) NA else any(i %in% ncs)
      ),
      # sequence matters, first
      # value determines result
      helper4 = dplyr::coalesce(
        helper3, helper2, helper1
      ),
      #
      out = dplyr::case_when(
        helper4 ~ stn,
        !helper4 ~ stc,
        !is.na(helper4) ~ sto
      )
    ) %>%
    dplyr::pull(out) -> df$ctis


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
