# function definition for dfCalculate

#### history ####
# 2025-02-10 first version


#' @noRd
#' @export
#' @importFrom dplyr if_else mutate case_when `%>%`
#' @importFrom stringi stri_extract_all_regex stri_split_regex
.numSites <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))


  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
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

    txt <- '
Calculates the number of the sites where the trial is conducted.
EUCTR lacks information on number of sites outside of the EEA;
for each non-EEA country mentioned, at least one site is assumed.

Returns an integer.
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


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      helper_nonEEA = stringi::stri_split_regex(
        stringi::stri_replace_all_regex(
          e863_trial_sites_planned_in,
          pattern = "United|Federation|Republic",
          replacement = ""),
        " +"),
      helper_numNonEEA = sapply(
        helper_nonEEA, length),
      out = helper_numNonEEA +
        e851_number_of_sites_anticipated_in_the_eea
    ) %>%
    dplyr::pull(out) -> df$euctr


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      helper_cities = stringi::stri_split_regex(
        location.facility.address.city,
        " / "),
      helper_numCities = sapply(helper_cities, length),
      out = dplyr::if_else(
        !is.na(location.facility.address.city),
        helper_numCities, NA_integer_)
    ) %>%
    dplyr::pull(out) -> df$ctgov


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      helper_cities = stringi::stri_split_regex(
        protocolSection.contactsLocationsModule.locations.city,
        " / "),
      helper_numCities = sapply(helper_cities, length),
      out = dplyr::if_else(
        !is.na(protocolSection.contactsLocationsModule.locations.city),
        helper_numCities, NA_integer_)
    ) %>%
    dplyr::pull(out) -> df$ctgov2


  #### . ISRCTN ####
  df %>%
    dplyr::mutate(
      helper_numCities = stringi::stri_split_regex(
        participants.trialCentres.trialCentre.name,
        " / ") %>%
        sapply(length),
      helper_shortCut = stringi::stri_extract_all_regex(
        participants.trialCentres.trialCentre.name,
        # trial 76463425 reads, "and 58 other sites"
        "[0-9]+[^/]+?sites") %>%
        stringi::stri_extract_all_regex("[0-9]+") %>%
        sapply(function(i) max(0L, as.numeric(i), na.rm = TRUE)),
      out = dplyr::if_else(
        !is.na(participants.trialCentres.trialCentre.name),
        helper_numCities + helper_shortCut, NA_integer_)
    ) %>%
  dplyr::pull(out) -> df$isrctn


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      helper_numSitesCtis1 = sapply(
        authorizedPartsII.trialSites.id, length),
      helper_numSitesCtis2 = sapply(
        authorizedApplication.authorizedPartsII.trialSites.id, length),
      out = dplyr::case_when(
        !is.na(authorizedPartsII.trialSites.id) ~
          helper_numSitesCtis1,
        !is.na(authorizedApplication.authorizedPartsII.trialSites.id) ~
          helper_numSitesCtis2
      )
    ) %>%
    dplyr::pull(out) -> df$ctis


  # merge into vector
  df[[".numSites"]] <- as.integer(
    dfMergeVariablesRelevel(
      df = df,
      colnames = names(fldsNeeded)
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
