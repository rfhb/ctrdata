#### history ####
# 2025-07-18 first version

#' Calculate the external from a study's register record
#'
#' Trial concept calculated: Calculates the links e.g. to publications or
#' other external files referenced from a study record.
#' Requires loading results-related information for EUCTR.
#' Note that documents stored in registers can be downloaded directly,
#' see \link{ctrLoadQueryIntoDb}.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @returns data frame with columns `_id` and new column `.externalLinks`
#' (character).
#'
#' @export
#'
#' @importFrom dplyr mutate coalesce `%>%`
#' @importFrom rlang .data
#'
#' @examples
#' # fields needed
#' f.hasResults()
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.externalLinks",
#'   con = dbc)
#' trialsDf
#'
f.externalLinks <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  #### fields ####
  fldsNeeded <- list(
    "euctr" = c(
      "trialChanges.pubMedReferenceNumbers.pmid"
    ),
    "ctgov" = c(
      "results_reference.citation",
      "link"
    ),
    "ctgov2" = c(
      "protocolSection.referencesModule.seeAlsoLinks.label",
      "protocolSection.referencesModule.seeAlsoLinks.url"
    ),
    "isrctn" = c(
      "results.publicationDetails"
    ),
    "ctis" = c(
      "applications.partI.trialDetails.references",
      "authorizedApplication.authorizedPartI.trialDetails.references",
      "authorizedPartI.trialDetails.references"
    )
  )


  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### helper ####

  # interleave several vectors by
  # first, second etc. position
  w <- function(...) c(rbind(...))

  #### calculate ####

  # check generic, do not edit
  df <- fctChkFlds(df, fldsNeeded)

  # helper function
  `%>%` <- dplyr::`%>%`


  #### . EUCTR ####
  df %>%
    dplyr::mutate(
      #
      # "trialChanges.pubMedReferenceNumbers"
      linksEuctr = sapply(
        .data$trialChanges.pubMedReferenceNumbers.pmid,
        function(r) if (all(is.na(r))) NA else
          paste0(na.omit(r), collapse = " "),
        USE.NAMES = FALSE)
      #
    ) -> df


  #### . CTGOV ####
  df %>%
    dplyr::mutate(
      #
      linksCtgov = mapply(
        function(a, b) {
          r <- na.omit(unique(w(
            unlist(a, use.names = FALSE),
            unlist(b, use.names = FALSE))))
          if (all(is.na(r))) NA else paste0(r, collapse = " ")
        },
        a = .data$results_reference.citation,
        b = .data$link,
        USE.NAMES = FALSE
      )
      #
    ) -> df


  #### . CTGOV2 ####
  df %>%
    dplyr::mutate(
      #
      linksCtgov2 = mapply(
        function(a, b) {
          r <- na.omit(unique(w(
            unlist(a, use.names = FALSE),
            unlist(b, use.names = FALSE))))
          if (all(is.na(r))) NA else paste0(r, collapse = " ")
        },
        a = .data$protocolSection.referencesModule.seeAlsoLinks.label,
        b = .data$protocolSection.referencesModule.seeAlsoLinks.url,
        USE.NAMES = FALSE
      )
      #
    ) -> df


  #### . ISRCTN ####

  # results.publicationDetails as is


  #### . CTIS ####
  df %>%
    dplyr::mutate(
      #
      linksCtis = mapply(
        function(a, b, c) {
            r <- na.omit(unique(w(
              unlist(a, use.names = FALSE),
              unlist(b, use.names = FALSE),
              unlist(c, use.names = FALSE))))
            if (all(is.na(r))) NA else paste0(r, collapse = " ")
          },
        a = .data$applications.partI.trialDetails.references,
        b = .data$authorizedApplication.authorizedPartI.trialDetails.references,
        c = .data$authorizedPartI.trialDetails.references,
        USE.NAMES = FALSE
      )
      #
    ) -> df


  #### merge ####
  df %>%
    dplyr::mutate(
      .externalLinks = dplyr::coalesce(
        .data$linksEuctr,
        .data$linksCtgov,
        .data$linksCtgov2,
        .data$results.publicationDetails,
        .data$linksCtis
      )
    ) -> df

  # keep only outcome columns
  df <- df[, c("_id", ".externalLinks"), drop = FALSE]


  #### checks ####
  stopifnot(ncol(df) == 2L)
  stopifnot(inherits(df[[".externalLinks"]], "character"))

  # return
  return(df)

} # end f.externalLinks
