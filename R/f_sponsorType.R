#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved

#' Calculate type of sponsor of a study
#'
#' Trial concept calculated: type or class of the sponsor(s) of the study.
#' No specific field is available in ISRCTN; thus, sponsor type is set to
#' `other`. Note: If several sponsors, sponsor type is deemed `for profit`
#' \emph{if any sponsor is commercial}.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.sponsorType`, which is
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
      "authorizedApplication.authorizedPartI.sponsors.commercial",
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
      .data$helper, function(r) {
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
  # lists are likely derived from OMS;
  # items are not in CTIS List Values
  #
  cos <- c(
    "Industry",
    "Laboratory/Research/Testing facility",
    "Pharmaceutical company",
    "Non-Pharmaceutical company",
    "Pharmaceutical association/federation"
  )
  #
  # TODO categories currently not used
  ncs <- c(
    "Hospital/Clinic/Other health care facility",
    "Patient organisation/association",
    "Educational Institution",
    "Health care"
  )
  #
  df %>%
    dplyr::mutate(
      # helpers are true if any sponsor is commercial
      #
      helper1 = sapply(
        # seems systematically filled, CT.04.02 Field: Organisation type
        .data$authorizedApplication.authorizedPartI.sponsors.isCommercial, any),
      #
      helper2 = sapply(
        # field possibly from CTIS1
        .data$primarySponsor.commercial,
        function(i) i == "Commercial"),
      #
      helper3 = sapply(
        stringi::stri_split_fixed(
          .data$sponsorType, ", "),
        function(i) if (all(is.na(i))) NA else any(i %in% cos)
      ),
      #
      helper4 = sapply(
        stringi::stri_split_fixed(
          .data$authorizedApplication.authorizedPartI.sponsors.commercial, " / "),
        function(i) {
          ii <- (i == "Commercial") | (i == TRUE)
          if (all(is.na(ii))) NA else any(ii)
        }),
      #
      helper5 = mapply(
        function(h1, h2, h3, h4) {
          if (all(is.na(c(h1, h2, h3, h4)))) return(NA)
          sum(h1, h2, h3, h4, na.rm = TRUE)
        },
        h1 = .data$helper1, h2 = .data$helper2,
        h3 = .data$helper3, h4 = .data$helper4
      ),
      #
      out = dplyr::case_when(
        .data$helper5 >= 1L ~ stc,
        !(.data$helper5 >= 1L) ~ stn,
        !is.na(.data$helper5) ~ sto,
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
