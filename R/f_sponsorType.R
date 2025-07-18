#### history ####
# 2025-01-27 first partly working version
# 2025-02-08 improved
# 2025-07-18 changed categorisation

#' Calculate type of sponsor of a study
#'
#' Trial concept calculated: type or class of the sponsor(s) of the study.
#' No specific field is available in ISRCTN; thus, sponsor type is set to
#' `other`. Note: If several sponsors, sponsor type is deemed `mixed`
#' \emph{if there is both, a commercial and a non-commercial sponsor(s)}.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.sponsorType`, which is
#' a factor with levels `for profit`, `not for profit`, `mixed` (not and for
#' profit sponsors) or `other`.
#'
#' @export
#'
#' @importFrom dplyr mutate case_match pull `%>%`
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


  # Ravinetto-R et al. 2015 https://doi.org/10.1186/s12914-015-0073-8
  # "Previously there was a description of the features of non-commercial
  # clinical trials (i.e. those conducted without the participation of the
  # pharmaceutical industry) and of non-commercial sponsors (i.e. universities,
  # hospitals, public scientific organisations, non-profit institutions,
  # patient organisations or individual researchers) [17].
  # The new [EU] regulation is much more explicit in acknowledging the
  # importance of clinical trials conducted by non-commercial sponsors,
  # which often rely on external funding from funds or charities."


  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      # EudraCT protocol related data dictionary.xls
      # "A commercial sponsor is a person or organisation that takes
      # responsibility for a trial which is part of the development programme
      # for a marketing authorisation of a medicinal product at the time of
      # the application"
      "b1_sponsor.b31_and_b32_status_of_the_sponsor"
    ),
    "ctgov" = c(
      "sponsors.collaborator.agency_class",
      "sponsors.lead_sponsor.agency_class"
    ),
    "ctgov2" = c(
      # see also Enumeration types
      # https://clinicaltrials.gov/data-api/about-api/study-data-structure
      # which shows for
      # protocolSection.sponsorCollaboratorsModule.leadSponsor.class
      # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-AgencyClass
      "protocolSection.sponsorCollaboratorsModule.collaborators.class",
      "protocolSection.sponsorCollaboratorsModule.leadSponsor.class"
    ),
    "isrctn" = c(
      # parties.funderId is an oid
      "ctrname"
    ),
    "ctis" = c(
      "sponsorType",
      "primarySponsor.commercial", # CTIS1?
      "primarySponsor.isCommercial", # CTIS1?
      "coSponsors.commercial", # CTIS1?
      "coSponsors.isCommercial", # CTIS1?
      "authorizedApplication.authorizedPartI.sponsors.commercial",
      "authorizedApplication.authorizedPartI.sponsors.isCommercial"
    ))

  # helper definitions
  # sponsor type
  stc <- "for profit"
  stn <- "not for profit"
  stm <- "mixed"
  sto <- "other"

  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  df <- fctChkFlds(df, fldsNeeded)


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
        # values are limited to Commercial or Non-commercial
        if (any(r == "Commercial") & any(r == "Non-Commercial")) return(stm)
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
    helper = strsplit(
      as.character(.data$sponsors.collaborator.agency_class),
      split = " / "),
    #
    out = mapply(
      function(a, b, c) {
        r <- na.omit(unique(a, b))
        if (all(is.na(r))) return(NA_character_)
        if (any(r == "Industry") & any(grepl("Non-Commercial|NIH|U.S. Fed", r))) return(stm)
        if (any(r == "Industry")) return(stc)
        if (all(grepl("Non-Commercial|NIH|U.S. Fed", r))) return(stn)
        return(sto)
      },
      a = .data$sponsors.lead_sponsor.agency_class,
      b = .data$helper,
      USE.NAMES = FALSE
    )
    #
  ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####

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

  df %>% dplyr::mutate(
    #
    helper = strsplit(
      as.character(.data$protocolSection.sponsorCollaboratorsModule.collaborators.class),
      split = " / "),
    #
    out = mapply(
      function(a, b, c) {
        r <- na.omit(unique(a, b))
        if (all(is.na(r))) return(NA_character_)
        if (any(r == "INDUSTRY") & any(grepl("NIH|FED|OTHER_GOV|FED|NETWORK", r))) return(stm)
        if (any(r == "INDUSTRY")) return(stc)
        if (all(grepl("NIH|FED|OTHER_GOV|FED|NETWORK", r))) return(stn)
        return(sto)
      },
      a = .data$protocolSection.sponsorCollaboratorsModule.leadSponsor.class,
      b = .data$helper,
      USE.NAMES = FALSE
    )
    #
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
  ncs <- c(
    "Hospital/Clinic/Other health care facility",
    "Patient organisation/association",
    "Educational Institution",
    "Health care"
  )
  #
  df %>%
    dplyr::mutate(
      #
      helper1 = sapply(
        stringi::stri_split_fixed(.data$sponsorType, ", "),
        function(i) {
          r <- na.omit(i)
          if (!length(r)) return(NA_character_)
          if (any(r %in% cos) & any(r %in% ncs)) return(stm)
          if (all(r %in% cos)) return(stc)
          if (all(r %in% ncs)) return(stn)
        }),
      #
      helper2 = sapply(
        .data$primarySponsor.commercial,
        function(i) {
          if (is.na(i)) return(NA_character_)
          if (i == "Commercial") stc else stn
        }),
      helper3 = sapply(
        .data$primarySponsor.isCommercial,
        function(i) {
          if (is.na(i)) return(NA_character_)
          if (i == TRUE) stc else stn
        }),
      #
      helper4 = sapply(
        stringi::stri_split_fixed(
          .data$coSponsors.commercial, " / "),
        function(i) {
          r <- na.omit(i)
          if (!length(r)) return(NA)
          if (any(r == "Commercial") & any(r  == "Non-Commercial")) return(stm)
          if (all(r == "Commercial")) return(stc)
          if (all(r == "Non-Commercial")) return(stn)
        }),
      helper5 = sapply(
        .data$coSponsors.isCommercial,
        function(i) {
          r <- na.omit(i)
          if (!length(r)) return(NA)
          if (any(r == TRUE) & any(r == FALSE)) return(stm)
          if (all(r)) return(stc)
          if (!all(r)) return(stn)
        }),
      #
      helper6 = sapply(
        stringi::stri_split_fixed(
          .data$authorizedApplication.authorizedPartI.sponsors.commercial, " / "),
        function(i) {
          r <- na.omit(i)
          if (!length(r)) return(NA)
          if (any(r == "Commercial") & any(r  == "Non-Commercial")) return(stm)
          if (all(r == "Commercial")) return(stc)
          if (all(r == "Non-Commercial")) return(stn)
        }),
      helper7 = sapply(
        .data$authorizedApplication.authorizedPartI.sponsors.isCommercial,
        function(i) {
          r <- na.omit(i)
          if (!length(r)) return(NA)
          if (any(r == TRUE) & any(r == FALSE)) return(stm)
          if (all(r)) return(stc)
          if (!all(r)) return(stn)
        }),      #
      out = mapply(
        function(h1, h2, h3, h4, h5, h6, h7) {
          r <- na.omit(c(h1, h2, h3, h4, h5, h6, h7))
          if (!length(r)) return(NA)
          if (any(r == "for profit") & any(r  == "not for profit")) return(stm)
          if (all(r == "for profit")) return(stc)
          if (all(r == "not for profit")) return(stn)
        },
        h1 = .data$helper1,
        h2 = .data$helper2,
        h3 = .data$helper3,
        h4 = .data$helper4,
        h5 = .data$helper5,
        h6 = .data$helper6,
        h7 = .data$helper7,
        USE.NAMES = FALSE
      )
      #
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
    levels = c(stn, stm, stc, sto)
  )

  # keep only outcome columns
  df <- df[, c("_id", ".sponsorType"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".sponsorType"]], "factor"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .sponsorType
