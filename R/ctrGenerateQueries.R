### ctrdata package
### 2025-02-15

#' Generates queries that work across registers
#'
#' From high-level search terms provided by the user, generate specific queries
#' for each registers with which ctrdata works, see \link{ctrdata-registers}.
#' Search terms that are expanded to concepts such as from MeSH and MedDRA
#' by the search implementations in registers include the 'intervention' and
#' 'condition'. Logical operators only work with 'searchPhrase'.
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
#' @param startBefore String that can be interpreted as date (for EUCTR, when
#' trial was first registered)
#' @param startAfter String that can be interpreted as date (for EUCTR, when
#' trial was first registered)
#' @param completedBefore String that can be interpreted as date (does not work
#' with EUCTR)
#' @param completedAfter String that can be interpreted as date (does not work
#' with EUCTR)
#' @param onlyMedIntervTrials Logical, default \code{TRUE}, which indicates if
#' queries should search only for medicine interventional clinical trial
#' @param onlyWithResults Logical
#' @param countries Vector of country names, two- or three-letter ISO 3166 codes
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
#'   recruitment = "completed",
#'   )
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
#'  startAfter = "01/31/2010",
#'  countries = c("DE", "US", "United Kingdom"))
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
    onlyMedIntervTrials = TRUE,
    onlyWithResults = FALSE,
    countries = NULL
) {

  # check
  stopifnot(all(countries %in% unlist(countryTable)))

  # helper for date conversions
  queryDate <- function(x) {

    x <- lubridate::as_date(x, format = c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%m/%d/%Y",
      "%d/%m/%Y",
      "%d.%m.%Y"
    ))

    return(strftime(x, format = "%Y-%m-%d"))
  }

  # map to row in table
  countryIndex <- seq_len(nrow(countryTable))[apply(
    countryTable, 1, function(r)
      length(intersect(unlist(r), countries)) > 0L)]

  #### start ####
  urls <- c(
    "CTGOV2" = "https://clinicaltrials.gov/search?",
    # https://clinicaltrials.gov/find-studies/constructing-complex-search-queries
    # https://clinicaltrials.gov/data-api/about-api/search-areas
    "CTGOV2expert" = "https://clinicaltrials.gov/expert-search?term=",
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

    searchPhraseD <- stringi::stri_extract_all_regex(searchPhrase, "( OR | AND )")[[1]][1]

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

    # ctgov2expert
    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], " AND (",
      paste0('AREA[TitleSearch]"', searchPhraseA, '"', collapse = searchPhraseD),
      ") "
    )

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

    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], ' AND AREA[ConditionSearch]"', condition, '" ')

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

    # ctgov2expert
    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], ' AND AREA[InterventionSearch]"', intervention, '" ')

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

    # see also f_trialPhase.R

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

    # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-Phase
    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], ' AND (', c(
        "phase 1" = 'AREA[Phase]"EARLY_PHASE1" OR AREA[Phase]"PHASE1"',
        "phase 1+2" = 'AREA[Phase]"EARLY_PHASE1" OR AREA[Phase]"PHASE1" OR AREA[Phase]"PHASE2"',
        "phase 2" = 'AREA[Phase]"PHASE2"',
        "phase 2+3" = 'AREA[Phase]"PHASE2" OR AREA[Phase]"PHASE3"',
        "phase 3" = 'AREA[Phase]"PHASE3"',
        "phase 3+4" = 'AREA[Phase]"PHASE3" OR AREA[Phase]"PHASE4"',
        "phase 1+2+3" = 'AREA[Phase]"EARLY_PHASE1" OR AREA[Phase]"PHASE1" OR AREA[Phase]"PHASE2" OR AREA[Phase]"PHASE3"',
        "phase 4" = 'AREA[Phase]"PHASE4"',
        "phase 1+2+3+4" = 'AREA[Phase]"EARLY_PHASE1" OR AREA[Phase]"PHASE1" OR AREA[Phase]"PHASE2" OR AREA[Phase]"PHASE3" OR AREA[Phase]"PHASE4"'
      )[phase],
      ') ')

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

    # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-StandardAge
    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], ' AND (', c(
        "P" = 'AREA[StdAge]"CHILD"',
        "A" = 'AREA[StdAge]"ADULT"',
        "E" = 'AREA[StdAge]"OLDER_ADULT"',
        "P+A" = 'AREA[StdAge]"CHILD" OR AREA[StdAge]"ADULT"',
        "A+E" = 'AREA[StdAge]"ADULT" OR AREA[StdAge]"OLDER_ADULT"',
        "P+A+E" = 'AREA[StdAge]"CHILD" OR AREA[StdAge]"ADULT" OR AREA[StdAge]"OLDER_ADULT"'
      )[population],
      ') ')

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

    # https://clinicaltrials.gov/data-api/about-api/study-data-structure#enum-RecruitmentStatus
    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], ' AND (', c(
        "ongoing" = paste0(
          'AREA[OverallStatus]"ACTIVE_NOT_RECRUITING" OR ',
          'AREA[OverallStatus]"ENROLLING_BY_INVITATION" OR ',
          'AREA[OverallStatus]"RECRUITING"'),
        "completed" = 'AREA[OverallStatus]"COMPLETED"',
        "other" = paste0(
          'AREA[OverallStatus]"NOT_YET_RECRUITING" OR ',
          'AREA[OverallStatus]"SUSPENDED" OR ',
          'AREA[OverallStatus]"TERMINATED" OR ',
          'AREA[OverallStatus]"WITHDRAWN" OR ',
          'AREA[OverallStatus]"AVAILABLE"')
      )[recruitment],
      ') ')

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

    startAfter <- queryDate(startAfter)

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&start=", startAfter, "_")

    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], " AND AREA[StartDate]RANGE[",
      startAfter, ",MAX] ")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaStartDateFrom":"', startAfter, '",')

    # https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf
    # date when the trial was first entered into the EudraCT database
    # by a national competent authority or third country data provider
    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&dateFrom=", startAfter)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=GT+overallStartDate:", startAfter)

  }


  #### startBefore ####
  if (!is.null(startBefore)) {

    startBefore <- queryDate(startBefore)

    urls["CTGOV2"] <- ifelse(
      grepl("&start=", urls["CTGOV2"]),
      sub("(&start=[-0-9]+_)", paste0("\\1", startBefore), urls["CTGOV2"]),
      paste0(urls["CTGOV2"], "&start=_", startBefore))

    urls["CTGOV2expert"] <- ifelse(
      grepl("StartDate", urls["CTGOV2expert"]),
      sub("(AREA\\[StartDate\\]RANGE\\[[-0-9]+,)MAX", paste0("\\1", startBefore), urls["CTGOV2expert"]),
      paste0(urls["CTGOV2expert"], " AREA[StartDate]RANGE[MAX,",startBefore, "] "))

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaStartDateTo":"', startBefore, '",')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&dateTo=", startBefore)

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=LE+overallStartDate:", startBefore)

  }


  #### completedAfter ####
  if (!is.null(completedAfter)) {

    completedAfter <- queryDate(completedAfter)

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&primComp=", completedAfter, "_")

    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], " AND AREA[CompletionDate]RANGE[",
      completedAfter, ",MAX] ")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaEndDateFrom":"', completedAfter, '",')

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=GT+overallEndDate:", completedAfter)

  }


  #### completedBefore ####
  if (!is.null(completedBefore)) {

    completedBefore <- queryDate(completedBefore)

    urls["CTGOV2"] <- ifelse(
      grepl("&primComp=", urls["CTGOV2"]),
      sub("(&primComp=[-0-9]+_)", paste0("\\1", completedBefore), urls["CTGOV2"]),
      paste0(urls["CTGOV2"], "&primComp=_", completedBefore))

    urls["CTGOV2expert"] <- ifelse(
      grepl("CompletionDate", urls["CTGOV2expert"]),
      sub("(AREA\\[CompletionDate\\]RANGE\\[[-0-9]+,)MAX", paste0("\\1", completedBefore), urls["CTGOV2expert"]),
      paste0(urls["CTGOV2expert"], " AREA[CompletionDate]RANGE[MAX,",completedBefore, "] "))

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"eeaEndDateTo":"', completedBefore, '",')

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=LE+overallEndDate:", completedBefore)

  }


  #### onlyMedIntervTrials ####
  if (onlyMedIntervTrials)  {

    # not needed for CTIS, EUCTR

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&aggFilters=studyType:int")

    urls["CTGOV2expert"] <- paste0(
      # TODO could be further tightened as follows but unclear for CTGOV
      # https://www.clinicaltrials.gov/data-api/about-api/study-data-structure#enum-PrimaryPurpose
      # AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT))
      # AND (AREA[InterventionType](DRUG OR BIOLOGICAL))
      urls["CTGOV2expert"], "AND (AREA[StudyType]INTERVENTIONAL) ")

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"],
      "&filters=primaryStudyDesign:Interventional",
      if (is.null(phase)) paste0(
        # this was found to implement boolean or
        # phases as proxy for investigational medicines
        "&filters=phase:Phase 0,phase:Phase I,",
        "phase:Phase II,phase:Phase III,phase:Phase IV,",
        "phase:Phase I/II,phase:Phase II/III,phase:Phase III/IV"
      )
    )

  }


  #### onlyWithResults ####
  if (onlyWithResults) {

    urls["CTGOV2"] <- paste0(
      urls["CTGOV2"], "&aggFilters=results:with")

    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], "AND (NOT AREA[ResultsFirstPostDate]MISSING) ")

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"hasClinicalStudyReport":true,')

    urls["EUCTR"] <- paste0(
      urls["EUCTR"], "&resultsstatus=trials-with-results")

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"], "&filters=results:withResults")

  }


  #### countries ####
  if (!is.null(countries)) {

    urls["CTGOV2expert"] <- paste0(
      urls["CTGOV2expert"], " AND (",
      paste0(
        'AREA[LocationCountry]"',
        countryTable[countryIndex, 2],
        collapse = '" OR '
      ), '") '
    )

    if (length(countryIndex) == 1L) {

      urls["CTGOV2"] <- paste0(
        urls["CTGOV2"],
        "&country=", countryTable[countryIndex, 2][1]
      )

    } else {

      urls["CTGOV2"] <- urls["CTGOV2expert"]

    }

    urls["CTIS"] <- paste0(
      urls["CTIS"], '"msc":[',
      paste0(countryTable[countryIndex, 1], collapse = ","),
      '],')

    tmpC <- countryTable[countryIndex, 3]
    urls["EUCTR"] <- paste0(
      urls["EUCTR"],
      paste0(
        "&country=",
        c(tolower(tmpC[tmpC %in% countriesEUCTR]),
          if(!all(tmpC %in% countriesEUCTR)) "3rd"),
        collapse = "")
    )

    urls["ISRCTN"] <- paste0(
      urls["ISRCTN"],
      paste0("&filters=recruitmentCountry:",
             countryTable[countryIndex, 2], collapse = "")
    )

  }


  #### finalising ####

  # CTGOV2
  ctgov2Filter <- stringi::stri_extract_all_regex(
    urls["CTGOV2"], "[,&]aggFilters=[^,&]+")[[1]]
  if (!all(is.na(ctgov2Filter))) urls["CTGOV2"] <- paste0(
    stringi::stri_replace_all_regex(
      urls["CTGOV2"], "[,&]aggFilters=[^,&]+", "")[[1]],
    "&aggFilters=",
    paste0(sub("&aggFilters=", "", ctgov2Filter), collapse = ","))

  # CTGOVexpert
  urls["CTGOV2"] <- sub("search[?]term= AND ", "search?term=", urls["CTGOV2"])
  urls["CTGOV2expert"] <- sub("search[?]term= AND ", "search?term=", urls["CTGOV2expert"])

  # CTIS
  urls["CTIS"] <- paste0(sub(",$", "", urls["CTIS"]), '}')

  # ISRCTN
  isrctnFilter <- stringi::stri_extract_all_regex(
    urls["ISRCTN"], "[,&]filters=[^&]+")[[1]]
  if (!all(is.na(isrctnFilter))) urls["ISRCTN"] <- paste0(
    stringi::stri_replace_all_regex(
      urls["ISRCTN"], "[,&]filters=[^&]+", "")[[1]],
    "&filters=",
    paste0(sub("&filters=", "", isrctnFilter), collapse = ","))
  if (!grepl("q=", urls["ISRCTN"])) urls["ISRCTN"] <-
    sub("[?]([&])?", "?&q=\\1", urls["ISRCTN"])

  # all prettify normalise
  urls <- trimws(gsub("  +", " ", urls))

  # put CTIS last so that it would open
  urls <- urls[c("EUCTR", "ISRCTN", "CTGOV2", "CTGOV2expert", "CTIS")]

  # named vector
  return(urls)

} # end ctrGenerateQueries
