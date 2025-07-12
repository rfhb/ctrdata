#### history ####
# 2025-02-10 first version

#' Calculate number of sites of a study
#'
#' Trial concept calculated: number of the sites where the trial is conducted.
#' EUCTR lacks information on number of sites outside of the EEA;
#' for each non-EEA country mentioned, at least one site is assumed.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and `.numSites`, an integer.
#'
#' @export
#'
#' @importFrom dplyr if_else mutate case_when `%>%`
#' @importFrom stringi stri_extract_all_regex stri_split_regex
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.numSites()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.numSites",
#'   con = dbc)
#' trialsDf
#'
f.numSites <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "e83single_site_trial",
      "e841_number_of_sites_anticipated_in_member_state_concerned",
      "e85_the_trial_involves_multiple_member_states",
      "e851_number_of_sites_anticipated_in_the_eea",
      "e863_trial_sites_planned_in"
    ),
    "ctgov" = c(
      "location.facility.address.city"
    ),
    "ctgov2" = c(
      "protocolSection.contactsLocationsModule.locations.city"
    ),
    "isrctn" = c(
      "participants.trialCentres.trialCentre.name"
    ),
    "ctis" = c(
      # CTIS1
      "authorizedPartsII.trialSites.id",
      # CTIS2
      "authorizedApplication.authorizedPartsII.trialSites.id"
    ))

  # not relevant after inspection:
  #


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

  # helper function
  lengthWoNA <- function(...) {
    if (all(is.na(...))) return(NA)
    return(length(na.omit(...)))
  }


  #### . EUCTR ####
  nonEEA <- countryTable[countryTable$V3 %in% countriesActive, ][["V2"]]
  df %>%
    dplyr::mutate(
      helper_nonEEA = stringi::stri_split_regex(
        stringi::stri_replace_all_regex(
          .data$e863_trial_sites_planned_in,
          pattern = paste0(c(nonEEA, "United", "Federation", "Republic"), collapse = "|"),
          replacement = ""),
        # including a letter to avoid including empty strings
        "[a-z] +"),
      #
      helper_numNonEEA = sapply(
        .data$helper_nonEEA, lengthWoNA),
      #
      helper_numEEA = dplyr::case_when(
        !is.na(.data$e851_number_of_sites_anticipated_in_the_eea) ~
          .data$e851_number_of_sites_anticipated_in_the_eea,
        !is.na(.data$e841_number_of_sites_anticipated_in_member_state_concerned) &
          !.data$e85_the_trial_involves_multiple_member_states ~
          .data$e841_number_of_sites_anticipated_in_member_state_concerned,
        .data$e83single_site_trial ~ 1L,
        .default = NA_integer_
      ),
      #
      out = mapply(
        function(h1, h2) if (all(is.na(c(h1, h2)))) NA else sum(h1, h2, na.rm = TRUE),
        h1 = .data$helper_numNonEEA,
        h2 = .data$helper_numEEA
        )
    ) %>%
    dplyr::pull("out") -> df$euctr


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      helper_cities = stringi::stri_split_regex(
        .data$location.facility.address.city,
        " / "),
      helper_numCities = sapply(.data$helper_cities, length),
      out = dplyr::if_else(
        !is.na(.data$location.facility.address.city),
        .data$helper_numCities, NA_integer_)
    ) %>%
    dplyr::pull("out") -> df$ctgov


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      helper_cities = stringi::stri_split_regex(
        .data$protocolSection.contactsLocationsModule.locations.city,
        " / "),
      helper_numCities = sapply(.data$helper_cities, length),
      out = dplyr::if_else(
        !is.na(.data$protocolSection.contactsLocationsModule.locations.city),
        .data$helper_numCities, NA_integer_)
    ) %>%
    dplyr::pull("out") -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      helper_numCities = stringi::stri_split_regex(
        .data$participants.trialCentres.trialCentre.name,
        " / ") %>%
        sapply(length),
      helper_shortCut = stringi::stri_extract_all_regex(
        .data$participants.trialCentres.trialCentre.name,
        # trial 76463425 reads, "and 58 other sites"
        "[0-9]+[^/]+?sites") %>%
        stringi::stri_extract_all_regex("[0-9]+") %>%
        sapply(function(i) max(0L, as.numeric(i), na.rm = TRUE)),
      out = dplyr::if_else(
        !is.na(.data$participants.trialCentres.trialCentre.name),
        .data$helper_numCities + .data$helper_shortCut, NA_integer_)
    ) %>%
  dplyr::pull("out") -> df$isrctn


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      helper_numSitesCtis1 = sapply(
        .data$authorizedPartsII.trialSites.id, lengthWoNA),
      helper_numSitesCtis2 = sapply(
        .data$authorizedApplication.authorizedPartsII.trialSites.id, lengthWoNA),
      out = dplyr::case_when(
        !is.na(.data$authorizedPartsII.trialSites.id) ~
          .data$helper_numSitesCtis1,
        !is.na(.data$authorizedApplication.authorizedPartsII.trialSites.id) ~
          .data$helper_numSitesCtis2,
        .default = NA_integer_
      )
    ) %>%
    dplyr::pull("out") -> df$ctis


  # merge into vector
  df[[".numSites"]] <- as.integer(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)[names(fldsNeeded) != ""]
    )
  )

  # keep only outcome columns
  df <- df[, c("_id", ".numSites"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".numSites"]], "integer"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .numSites
