### ctrdata package
### 2025-02-15

#' Generates queries that work across registers
#'
#' From high-level search terms provided by the user, generate specific queries
#' for each registers with which ctrdata works, see \link{ctrdata-registers}.
#' Search terms that are expanded to concepts such as from MeSH and MedDRA
#' by the search implementations in registers include the
#' intervention and condition, see \link{ctrdata-registers}.
#'
#' @param condition String with condition / disease
#' @param intervention String with intervention
#' @param startBefore String that can be interpreted as date, see example
#' @param startAfter String that can be interpreted as date
#' @param completedBefore String that can be interpreted as date (does not work
#' with EUCTR)
#' @param completedAfter String that can be interpreted as date (does not work
#' with EUCTR)
#' @param onlyWithResults Logical
#' @param registers Vector of register names, default all registers
#'
#' @returns Named vector of URLs for finding trials in the registers and as
#' input to functions \link{ctrLoadQueryIntoDb} and
#' \link{ctrOpenSearchPagesInBrowser}
#'
#' @export
#'
#' @examples
#'
#' urls <- ctrGenerateQueries(
#'   condition = "cancer",
#'   intervention = "antibody",
#'   startAfter = "2000-01-01",
#'   startBefore = "2030-01-01",
#'   completedAfter = "2000-01-01",
#'   completedBefore = "2030-01-01",
#'   onlyWithResults = TRUE)
#'
#' # open queries in register web interface
#' sapply(urls, ctrOpenSearchPagesInBrowser)
#'
#' # load queries into database collection
#' # sapply(urls, ctrLoadQueryIntoDb, con = dbc)
#'
ctrGenerateQueries <- function(

  condition = NULL,
  intervention = NULL,

  startBefore = NULL,
  startAfter = NULL,
  completedBefore = NULL,
  completedAfter = NULL,

  onlyWithResults = FALSE,

  registers = c("EUCTR", "ISRCTN", "CTIS", "CTGOV2")

) {

  # starting point, open CTIS last
  urls <- c(
    "CTGOV2" = "https://clinicaltrials.gov/search?",
    "EUCTR" = "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
    "ISRCTN" = "https://www.isrctn.com/search?",
    "CTIS" = "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"
  )

  # parameter: condition
  if (!is.null(condition)) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "cond=", condition)

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"medicalCondition":"', condition, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], condition)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=condition:", condition, ",")

  }

  # parameter: intervention
  if (!is.null(intervention)) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&intr=", intervention)

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"containAll":"', intervention, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], ifelse(!is.null(condition), " AND ", ""), intervention)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=intervention:", intervention, ",")

  }

  # parameter: startAfter
  if (!is.null(startAfter)) {

    startAfter <- strftime(startAfter, format = "%Y-%m-%d")

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&start=", startAfter, "_")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaStartDateFrom":"', startAfter, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&dateFrom=", startAfter)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=LE+overallStartDate:", startAfter, ",")

  }

  # parameter: startBefore
  if (!is.null(startBefore)) {

    startBefore <- strftime(startBefore, format = "%Y-%m-%d")

    urls["CTGOV2"] <- ifelse(
      grepl("&start=", urls["CTGOV2"]),
      sub("(&start=[-0-9]+_)", paste0("\\1", startBefore), urls["CTGOV2"]),
      paste0("&start=_", startBefore)
    )

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaStartDateTo":"', startBefore, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&dateTo=", startBefore)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=GT+overallStartDate:", startBefore, ",")

  }

  # parameter: completedBefore
  if (!is.null(completedBefore)) {

    completedBefore <- strftime(completedBefore, format = "%Y-%m-%d")

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&primComp=", completedBefore, "_")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaEndDateTo":"', completedBefore, '",')

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=LE+overallEndDate:", completedBefore, ",")

  }

  # parameter: completedAfter
  if (!is.null(completedAfter)) {

    completedAfter <- strftime(completedAfter, format = "%Y-%m-%d")

    urls["CTGOV2"] <- ifelse(
      grepl("&primComp=", urls["CTGOV2"]),
      sub("(&primComp=[-0-9]+_)", paste0("\\1", startBefore), urls["CTGOV2"]),
      paste0("&primComp=_", startBefore)
    )

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaEndDateFrom":"', completedAfter, '",')

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=GT+overallEndDate:", completedAfter, ",")

  }

  # parameter: onlyWithResults
  if (onlyWithResults) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&intr=", intervention)

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"hasClinicalStudyReport":true,')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&resultsstatus=trials-with-results")

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "filters=results:withResults,")

  }

  # finalising
  urls["CTIS"] <- paste0(sub(",$", "", urls["CTIS"]), '}')

  # finalising
  urls["ISRCTN"] <- paste0(
    gsub(",([$&])", "\\1", gsub(",filters=", ",", urls["ISRCTN"])),
    ifelse(!grepl("q=", urls["ISRCTN"]), "&q=", "")
  )

  # TODO delete
  # utils::URLdecode(urls)
  # clipr::write_clip(urls["CTIS"])
  # utils::browseURL(urls["CTIS"])

  # select
  urls <- urls[registers]

  # named vector
  return(urls)

} # end ctrGenerateQueries
