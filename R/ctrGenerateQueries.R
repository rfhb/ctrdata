### ctrdata package
### 2025-02-15

#' Generates queries that work across registers
#'
#' From high-level search terms provided by the user, generate specific queries
#' for each registers with which ctrdata works, see \link{ctrdata-registers}.
#' Search terms that are expanded to concepts such as from MeSH and MedDRA
#' by the search implementations in registers include the
#' 'intervention' and 'condition', see \link{ctrdata-registers}.
#' Logical operators only work with 'searchPhrase'.
#'
#' @param searchPhrase String with optional logical operators ("AND", "OR")
#' that will be searched in selected fields of registers that can handle logical
#' operators (general or title fields), should not include quotation marks
#' @param condition String with condition / disease
#' @param intervention String with intervention
#' @param phase String, e.g. "phase 2" (note that "phase 2+3" is a specific
#' category, not the union set of "phase 2" and "phase 3")
#' @param population String, e.g. "P" (paediatric), "A" (adult), "P+A"
#' (adult and paediatric), "E" (elderly), "P+A+E" participants can be recruited
#' @param recruitment String, one of "ongoing", "completed", "other" (
#' which includes "ended early" but this cannot be searched; use trial concept
#' \link{f.statusRecruitment} to identify this status)
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
#'   intervention = "antibody",
#'   phase = "phase 3",
#'   startAfter = "2000-01-01")
#'
#' # open queries in register web interface
#' sapply(urls, ctrOpenSearchPagesInBrowser)
#'
#' urls <- ctrGenerateQueries(
#'   searchPhrase = "antibody AND covid",
#'   recruitment = "completed")
#'
#' # count trials found
#' sapply(urls, ctrLoadQueryIntoDb, only.count = TRUE)
#'
#' # load queries into database collection
#' # sapply(urls, ctrLoadQueryIntoDb, con = dbc)
#'
#' # find research platform and platform trials
#' urls <- ctrGenerateQueries(
#'   searchPhrase = paste0(
#'    "basket OR platform OR umbrella OR master protocol OR ",
#'    "multiarm OR multistage OR subprotocol OR substudy OR ",
#'    "multi-arm OR multi-stage OR sub-protocol OR sub-study"),
#'  startAfter = "2010-01-01")
#'
#' # open queries in register web interface
#' sapply(urls, ctrOpenSearchPagesInBrowser)
#'
ctrGenerateQueries <- function(
    searchPhrase = NULL,
    condition = NULL,
    intervention = NULL,
    phase = NULL,
    population = NULL,
    recruitment = NULL,
    startBefore = NULL,
    startAfter = NULL,
    completedBefore = NULL,
    completedAfter = NULL,
    onlyWithResults = FALSE,
    registers = c("EUCTR", "ISRCTN", "CTIS", "CTGOV2")
) {

  # check
  stopifnot(all(registers %in% registerList[registerList != "CTGOV"]))


  #### start ####
  urls <- c(
    "CTGOV2" = "https://clinicaltrials.gov/search?",
    "EUCTR" = "https://www.clinicaltrialsregister.eu/ctr-search/search?query=",
    "ISRCTN" = "https://www.isrctn.com/search?",
    "CTIS" = "https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"
  )


  #### searchPhrase ####
  if (!is.null(searchPhrase)) {

    # mangle searchPhrase

    searchPhraseA <- strsplit(searchPhrase, "( OR | AND )")[[1]]

    searchPhraseB <- stringi::stri_replace_all_regex(
      searchPhrase,
      paste0("(", searchPhraseA, ")"),
      '"$1"',
      vectorize_all = FALSE
    )

    searchPhraseC <- gsub("( OR | AND )", ", ", searchPhrase)

    if (grepl(" AND ", searchPhrase) && grepl(" OR ", searchPhrase)) {
      warning(
        'Cannot use both "AND" and "OR" with CTIS. ',
        'Using "OR" to obtain largest set, can be ',
        "refined after ctrLoadQueryIntoDb(). ",
        call. = FALSE
      )
    }

    # ctgov2
    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "titles=", searchPhraseB)
    # "basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study"
    # title search 2372
    # https://clinicaltrials.gov/search?titles=%22basket%22%20OR%20%22platform%22%20OR%20%22umbrella%22%20OR%20%22master%20protocol%22%20OR%20%22multiarm%22%20OR%20%22multistage%22%20OR%20%22subprotocol%22%20OR%20%22substudy%22%20OR%20%22multi-arm%22%20OR%20%22multi-stage%22%20OR%20%22sub-protocol%22%20OR%20%22sub-study%22

    # ctis
    urls["CTIS"] <- paste0(
      urls["CTIS"], ifelse(
        grepl(" AND ", searchPhrase),
        '"containAll":"',
        '"containAny":"'
      ), searchPhraseC, '",')
    # "Contain any of these terms:" 289
    # https://euclinicaltrials.eu/ctis-public/search#searchCriteria={%22containAny%22:%22basket,%20platform,%20umbrella,%20multiarm,%20multistage,%20master%20protocol,%20subprotocol,%20substudy,%20multi-arm,%20multi-stage,%20sub-protocol,%20sub-study%22}
    # basket, platform, umbrella, master protocol, multiarm, multistage, subprotocol, substudy, multi-arm, multi-stage, sub-protocol, sub-study

    # euctr
    urls["EUCTR"] <- paste0(
      urls["EUCTR"], searchPhraseB)
    # "basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study"
    # general text search 2808
    # https://www.clinicaltrialsregister.eu/ctr-search/search?query=%22basket%22+OR+%22platform%22+OR+%22umbrella%22+OR+%22master+protocol%22+OR+%22multiarm%22+OR+%22multistage%22+OR+%22subprotocol%22+OR+%22substudy%22+OR+%22multi-arm%22+OR+%22multi-stage%22+OR+%22sub-protocol%22+OR+%22sub-study%22

    #
    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "q=", searchPhraseB)
    # "basket" OR "platform" OR "umbrella" OR "master protocol" OR "multiarm" OR "multistage" OR "subprotocol" OR "substudy" OR "multi-arm" OR "multi-stage" OR "sub-protocol" OR "sub-study"
    # text search 1426
    # https://www.isrctn.com/search?q=%22basket%22+OR+%22platform%22+OR+%22umbrella%22+OR+%22master+protocol%22+OR+%22multiarm%22+OR+%22multistage%22+OR+%22subprotocol%22+OR+%22substudy%22+OR+%22multi-arm%22+OR+%22multi-stage%22+OR+%22sub-protocol%22+OR+%22sub-study%22&searchType=advanced

  }


  #### clinical trial / interventional ####

  # ISRCTN not possible
  # EUCTR by definition
  # CTIS by definition
  # CTGOV interventional possible, but not limited to medicines


  #### condition ####
  if (!is.null(condition)) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "cond=", condition)

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"medicalCondition":"', condition, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], condition)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=condition:", condition)

  }


  #### intervention ####
  if (!is.null(intervention)) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&intr=", intervention)

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"containAll":"', intervention, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], ifelse(!is.null(condition), " AND ", ""), intervention)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=intervention:", intervention)

  }


  #### phase ####
  if (!is.null(phase)) {

    stopifnot(is.atomic(phase) && length(phase) == 1L)

    # see also f.trialPhase

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&aggFilters=phase:", c(
        "phase 1" = "0 1",
        "phase 1+2" = "0 1 2",
        "phase 2" = "2",
        "phase 2+3" = "2 3",
        "phase 3" = "3",
        "phase 3+4" = "3 4",
        "phase 1+2+3" = "0 1 2 3",
        "phase 4" = "4",
        "phase 1+2+3+4" = "0 1 2 3 4"
      )[phase]
    )

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"trialPhaseCode":[', c(
        "phase 1" = "1,2,3",
        "phase 1+2" = "7,8,9",
        "phase 2" = "4",
        "phase 2+3" = "10",
        "phase 3" = "5",
        "phase 3+4" = "11",
        "phase 1+2+3" = "10",
        "phase 4" = "6",
        "phase 1+2+3+4" = "6"
      )[phase],
      '],')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], c(
        "phase 1" = "&phase=phase-one",
        "phase 1+2" = "&phase=phase-one&phase=phase-two",
        "phase 2" = "&phase=phase-two",
        "phase 2+3" = "&phase=phase-two&phase=phase-three",
        "phase 3" = "&phase=phase-three",
        "phase 3+4" = "&phase=phase-three&phase=phase-four",
        "phase 1+2+3" = "&phase=phase-one&phase=phase-two&phase=phase-three",
        "phase 4" = "&phase=phase-four",
        "phase 1+2+3+4" = "&phase=phase-one&phase=phase-two&phase=phase-three&phase=phase-four"
      )[phase]
    )

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=phase:", c( # cannot accumulate
        "phase 1" = "Phase I",
        "phase 1+2" = "Phase I/II",
        "phase 2" = "Phase II",
        "phase 2+3" = "Phase II/III",
        "phase 3" = "Phase III",
        "phase 3+4" = "Phase III/IV",
        "phase 1+2+3" = "",
        "phase 4" = "Phase IV",
        "phase 1+2+3+4" = ""
      )[phase])

  }


  #### population ####
  if (!is.null(population)) {

    stopifnot(is.atomic(population) && length(population) == 1L)

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&aggFilters=ages:", c(
        "P" = "child",
        "A" = "adult",
        "E" = "older",
        "P+A" = "child adult",
        "A+E" = "adult older",
        "P+A+E" = "child adult older"
      )[population]
    )

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"ageGroupCode":[', c(
        # 1 = in utero
        "P" = "2",
        "A" = "3",
        "E" = "4",
        "P+A" = "2,3",
        "A+E" = "3,4",
        "P+A+E" = "2,3,4"
      )[population],
      '],')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], c(
        # &age=in-utero
        "A" = "&age=adult",
        "P" = "&age=children&age=adolescent&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18",
        "P+A" = "&age=children&age=adolescent&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18&age=adult",
        "E" = "&age=elderly",
        "A+E" = "&age=adult&age=elderly"
      )[population]
    )

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], c(
        # Not Specified
        # Other
        # Neonate
        # Mixed
        "A" = "&filters=ageRange:Adult",
        "P" = "&filters=ageRange:Child",
        "E" = "&filters=ageRange:Senior",
        "P+A+E" = "&filters=ageRange:All"
      )[population])

  }

  #### recruitment ####
  if (!is.null(recruitment)) {

    stopifnot(is.atomic(recruitment) && length(recruitment) == 1L)

    # see also f.statusRecruitment

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&aggFilters=status:", c(
        "ongoing" = "act rec",
        "completed" = "com",
        "other" = "ter sus wit unk not"
      )[recruitment]
    )

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"status":[', c(
        "ongoing" = "2,3,4,6,7",
        "completed" = "5,8",
        "other" = "1,9,10,11,12"
      )[recruitment],
      '],')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], c(
        "ongoing" = "&status=ongoing&status=trial-now-transitioned&status=suspended-by-ca&status=temporarily-halted&status=restarted",
        "completed" = "&status=completed",
        "other" = "&status=prematurely-ended&status=prohibited-by-ca&status=not-authorised"
      )[recruitment]
    )

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], c( # cannot accumulate
        "ongoing" = "&filters=trialStatus:ongoing",
        "completed" = "&filters=trialStatus:completed",
        "other" = "&filters=trialStatus:stopped" # this is most frequent other category
      )[recruitment])
  }


  #### startAfter ####
  if (!is.null(startAfter)) {

    startAfter <- strftime(startAfter, format = "%Y-%m-%d")

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&start=", startAfter, "_")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaStartDateFrom":"', startAfter, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&dateFrom=", startAfter)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=GT+overallStartDate:", startAfter)

  }


  #### startBefore ####
  if (!is.null(startBefore)) {

    startBefore <- strftime(startBefore, format = "%Y-%m-%d")

    urls["CTGOV2"] <- ifelse(
      grepl("&start=", urls["CTGOV2"]),
      sub("(&start=[-0-9]+_)", paste0("\\1", startBefore), urls["CTGOV2"]),
      paste0(urls["CTGOV2"], "&start=_", startBefore))

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaStartDateTo":"', startBefore, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&dateTo=", startBefore)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=LE+overallStartDate:", startBefore)

  }


  #### completedAfter ####
  if (!is.null(completedAfter)) {

    completedAfter <- strftime(completedAfter, format = "%Y-%m-%d")

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&primComp=", completedAfter, "_")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaEndDateFrom":"', completedAfter, '",')

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=GT+overallEndDate:", completedAfter)

  }


  #### completedBefore ####
  if (!is.null(completedBefore)) {

    completedBefore <- strftime(completedBefore, format = "%Y-%m-%d")

    urls["CTGOV2"] <- ifelse(
      grepl("&primComp=", urls["CTGOV2"]),
      sub("(&primComp=[-0-9]+_)", paste0("\\1", completedBefore), urls["CTGOV2"]),
      paste0(urls["CTGOV2"], "&primComp=_", completedBefore))

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaEndDateTo":"', completedBefore, '",')

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=LE+overallEndDate:", completedBefore)

  }


  #### onlyWithResults ####
  if (onlyWithResults) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&aggFilters=results:with")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"hasClinicalStudyReport":true,')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&resultsstatus=trials-with-results")

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=results:withResults")

  }

  # finalising CTIS
  urls["CTIS"] <- paste0(sub(",$", "", urls["CTIS"]), '}')

  # finalising CTGOV2
  ctgov2Filter <- stringi::stri_extract_all_regex(
    urls["CTGOV2"], "[,&]aggFilters=[^,&]+")[[1]]
  if (!all(is.na(ctgov2Filter))) urls["CTGOV2"] <- paste0(
    stringi::stri_replace_all_regex(
      urls["CTGOV2"], "[,&]aggFilters=[^,&]+", "")[[1]],
    "&aggFilters=",
    paste0(sub("&aggFilters=", "", ctgov2Filter), collapse = ","))

  # finalising ISRCTN
  isrctnFilter <- stringi::stri_extract_all_regex(
    urls["ISRCTN"], "[,&]filters=[^,&]+")[[1]]
  if (!all(is.na(isrctnFilter))) urls["ISRCTN"] <- paste0(
    stringi::stri_replace_all_regex(
      urls["ISRCTN"], "[,&]filters=[^,&]+", "")[[1]],
    "&filters=",
    paste0(sub("&filters=", "", isrctnFilter), collapse = ","))
  if (!grepl("q=", urls["ISRCTN"])) urls["ISRCTN"] <-
    paste0(urls["ISRCTN"], "&q=")

  # select but put CTIS last so that it would open
  # last and run script to retrieve the results
  urls <- urls[registerList[registerList %in% registers]]

  # named vector
  return(urls)

} # end ctrGenerateQueries
