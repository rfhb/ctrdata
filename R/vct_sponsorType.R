# function definition for dfCalculate
# function definition for dfCalculate

#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved


#' @noRd
#' @export
#' @importFrom dplyr mutate case_when case_match coalesce pull `%>%`
#' @importFrom stringi stri_split_fixed
.sponsorType <- function(df = NULL) {

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

    txt <- paste0('
Calculates the type or class of the lead or main sponsor of the trial.

Some information is not yet mapped (e.g., "NETWORK" in CTGOV2).
No specific field is available in ISRCTN (which retrieves information on
the type of funder via http://data.crossref.org/fundingdata/registry).

Returns a factor with levels "',
                  paste(c(stc, stn, sto), collapse = '", "'),
                  '"'
    )

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

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
  vct <- factor(dfMergeVariablesRelevel(
    df = df,
    colnames = fldsNeeded
  ),
  levels = c(stn, stc, sto))


  #### checks ####
  stopifnot(is.factor(vct))
  stopifnot(length(vct) == nrow(df))

  # return
  return(vct)

} # end .sponsorType
