### ctrdata package
### utility functions

#### variable definitions ####
#
# prototype return structure
emptyReturn <- list(n = 0L, success = NULL, failed = NULL)
#
# EUCTR definitions
countriesEUCTR <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB", "IS", "LI",
  "NO", "3RD")
#
# regexpr
# - queryterm and urls
regQueryterm <- "[^-.a-zA-Z0-9=?+&#%_:\"/, ]"
# - EudraCT e.g. 2010-022945-52
regEuctr <- "[0-9]{4}-[0-9]{6}-[0-9]{2}"
# - CTGOV
regCtgov <- "NCT[0-9]{8}"
# - regIsrctn
# FIXME check if first digit is always non zero
regIsrctn <- "[1-9][0-9]{7}"
# - CTIS e.g. 2022-501549-57-00
regCtis <- "[0-9]{4}-[0-9]{6}-[0-9]{2}-[0-9]{2}"
#
# register list
registerList <- c("EUCTR", "CTGOV", "ISRCTN", "CTIS")
#
# mapping field names to typing function for typeField()
typeVars <- list(
  #
  # dates
  #
  # - ctrdata intern
  "record_last_import" = "ctrDateCtr",
  #
  # - EUCTR
  "n_date_of_competent_authority_decision" = "ctrDate",
  "n_date_of_ethics_committee_opinion"     = "ctrDate",
  "p_date_of_the_global_end_of_the_trial"  = "ctrDate",
  "firstreceived_results_date"             = "ctrDate",
  "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database" = "ctrDate",
  "trialInformation.primaryCompletionDate" = "ctrDate",
  "trialInformation.analysisStageDate"     = "ctrDateTime",
  "trialInformation.globalEndOfTrialDate"  = "ctrDateTime",
  "trialInformation.recruitmentStartDate"  = "ctrDateTime",
  #
  "e891_in_the_member_state_concerned_days"   = "ctrDifftimeDays",
  "e891_in_the_member_state_concerned_months" = "ctrDifftimeMonths",
  "e891_in_the_member_state_concerned_years"  = "ctrDifftimeYears",
  "e892_in_all_countries_concerned_by_the_trial_days"   = "ctrDifftimeDays",
  "e892_in_all_countries_concerned_by_the_trial_months" = "ctrDifftimeMonths",
  "e892_in_all_countries_concerned_by_the_trial_years"  = "ctrDifftimeYears",
  #
  # - CTGOV
  "completion_date"          = "ctrDateUs",
  "last_update_posted"       = "ctrDateUs",
  "last_update_submitted_qc" = "ctrDateUs",
  "last_update_submitted"    = "ctrDateUs",
  "primary_completion_date"  = "ctrDateUs",
  "results_first_posted"     = "ctrDateUs",
  "start_date"               = "ctrDateUs",
  "study_first_posted"       = "ctrDateUs",
  "verification_date"        = "ctrDateUs",
  "required_header.download_date" = "ctrDateUs",
  "eligibility.minimum_age" = "ctrDifftime",
  "eligibility.maximum_age" = "ctrDifftime",
  #
  # - ISRCTN
  "participants.recruitmentStart" = "ctrDateTime",
  "participants.recruitmentEnd"   = "ctrDateTime",
  "trialDesign.overallStartDate"  = "ctrDateTime",
  "trialDesign.overallEndDate"    = "ctrDateTime",
  #
  # - CTIS
  "startDateEU" = "ctrDate",
  "authorizationDate" = "ctrDate",
  #
  #
  # factors / logical
  #
  # - EUCTR Yes / No / Information not present in EudraCT
  "a7_trial_is_part_of_a_paediatric_investigation_plan" = "ctrYesNo",
  "dimp.d21_imp_to_be_used_in_the_trial_has_a_marketing_authorisation" = "ctrYesNo",
  "e13_condition_being_studied_is_a_rare_disease" = "ctrYesNo",
  "e23_trial_contains_a_substudy" = "ctrYesNo",
  #
  "e61_diagnosis"         = "ctrYesNo",
  "e62_prophylaxis"       = "ctrYesNo",
  "e63_therapy"           = "ctrYesNo",
  "e64_safety"            = "ctrYesNo",
  "e65_efficacy"          = "ctrYesNo",
  "e66_pharmacokinetic"   = "ctrYesNo",
  "e67_pharmacodynamic"   = "ctrYesNo",
  "e68_bioequivalence"    = "ctrYesNo",
  "e69_dose_response"     = "ctrYesNo",
  "e610_pharmacogenetic"  = "ctrYesNo",
  "e611_pharmacogenomic"  = "ctrYesNo",
  "e612_pharmacoeconomic" = "ctrYesNo",
  "e613_others"           = "ctrYesNo",
  #
  "e71_human_pharmacology_phase_i"         = "ctrYesNo",
  "e711_first_administration_to_humans"    = "ctrYesNo",
  "e712_bioequivalence_study"              = "ctrYesNo",
  "e713_other"                             = "ctrYesNo",
  "e72_therapeutic_exploratory_phase_ii"   = "ctrYesNo",
  "e73_therapeutic_confirmatory_phase_iii" = "ctrYesNo",
  "e74_therapeutic_use_phase_iv"           = "ctrYesNo",
  #
  "e81_controlled"      = "ctrYesNo",
  "e811_randomised"     = "ctrYesNo",
  "e812_open"           = "ctrYesNo",
  "e813_single_blind"   = "ctrYesNo",
  "e814_double_blind"   = "ctrYesNo",
  "e815_parallel_group" = "ctrYesNo",
  "e816_cross_over"     = "ctrYesNo",
  "e817_other"          = "ctrYesNo",
  "e822_placebo"        = "ctrYesNo",
  #
  "e83_the_trial_involves_single_site_in_the_member_state_concerned"    = "ctrYesNo",
  "e83_will_this_trial_be_conducted_at_a_single_site_globally"          = "ctrYesNo",
  "e84_the_trial_involves_multiple_sites_in_the_member_state_concerned" = "ctrYesNo",
  "e84_will_this_trial_be_conducted_at_multiple_sites_globally"         = "ctrYesNo",
  "e85_the_trial_involves_multiple_member_states"                       = "ctrYesNo",
  "e861_trial_being_conducted_both_within_and_outside_the_eea"          = "ctrYesNo",
  "e862_trial_being_conducted_completely_outside_of_the_eea"            = "ctrYesNo",
  "e87_trial_has_a_data_monitoring_committee"                           = "ctrYesNo",
  #
  "f11_trial_has_subjects_under_18"            = "ctrYesNo",
  "f111_in_utero"                              = "ctrYesNo",
  "f112_preterm_newborn_infants_up_to_gestational_age__37_weeks" = "ctrYesNo",
  "f113_newborns_027_days"                     = "ctrYesNo",
  "f114_infants_and_toddlers_28_days23_months" = "ctrYesNo",
  "f115_children_211years"                     = "ctrYesNo",
  "f116_adolescents_1217_years"                = "ctrYesNo",
  "f12_adults_1864_years"                      = "ctrYesNo",
  "f13_elderly_65_years"                       = "ctrYesNo",
  "f21_female"                                 = "ctrYesNo",
  "f22_male"                                   = "ctrYesNo",
  "f31_healthy_volunteers"                     = "ctrYesNo",
  "f32_patients"                               = "ctrYesNo",
  "f33_specific_vulnerable_populations"        = "ctrYesNo",
  "f331_women_of_childbearing_potential_not_using_contraception_" = "ctrYesNo",
  "f332_women_of_childbearing_potential_using_contraception"      = "ctrYesNo",
  "f333_pregnant_women"      = "ctrYesNo",
  "f334_nursing_women"       = "ctrYesNo",
  "f335_emergency_situation" = "ctrYesNo",
  "f336_subjects_incapable_of_giving_consent_personally" = "ctrYesNo",
  #
  "trialInformation.analysisForPrimaryCompletion" = "ctrFalseTrue",
  "trialInformation.partOfPIP"                 = "ctrFalseTrue",
  "trialInformation.art45Related"              = "ctrFalseTrue",
  "trialInformation.art46Related"              = "ctrFalseTrue",
  "trialInformation.longTermFollowUpPlanned"   = "ctrFalseTrue",
  "trialInformation.idmcInvolvement"           = "ctrFalseTrue",
  "trialInformation.isGlobalEndOfTrialReached" = "ctrFalseTrue",
  "trialInformation.globalEndOfTrialPremature" = "ctrFalseTrue",
  #
  # - CTGOV
  "has_expanded_access"            = "ctrYesNo",
  "oversight_info.has_dmc"         = "ctrYesNo",
  "eligibility.healthy_volunteers" = "ctrYesNo",
  #
  # - ISRCTN
  "trialDescription.acknowledgment" = "ctrFalseTrue",
  "results.biomedRelated"           = "ctrFalseTrue",
  #
  # - CTIS
  "hasDeferrallApplied" = "ctrFalseTrue",
  "hasAmendmentApplied" = "ctrFalseTrue",
  "eudraCtInfo.hasVhp" = "ctrFalseTrue",
  #
  # numbers
  #
  # - EUCTR
  "e824_number_of_treatment_arms_in_the_trial"  = "ctrInt",
  "e841_number_of_sites_anticipated_in_member_state_concerned" = "ctrInt",
  "e851_number_of_sites_anticipated_in_the_eea" = "ctrInt",
  "e891_in_the_member_state_concerned_years"    = "ctrInt",
  "e891_in_the_member_state_concerned_months"   = "ctrInt",
  "e891_in_the_member_state_concerned_days"     = "ctrInt",
  "e892_in_all_countries_concerned_by_the_trial_years"  = "ctrInt",
  "e892_in_all_countries_concerned_by_the_trial_months" = "ctrInt",
  "e892_in_all_countries_concerned_by_the_trial_days"   = "ctrInt",
  "f11_number_of_subjects_for_this_age_range"   = "ctrInt",
  "f1111_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1121_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1131_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1141_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1151_number_of_subjects_for_this_age_range" = "ctrInt",
  "f1161_number_of_subjects_for_this_age_range" = "ctrInt",
  "f121_number_of_subjects_for_this_age_range"  = "ctrInt",
  "f131_number_of_subjects_for_this_age_range"  = "ctrInt",
  "f41_in_the_member_state"          = "ctrInt",
  "f421_in_the_eea"                  = "ctrInt",
  "f422_in_the_whole_clinical_trial" = "ctrInt",
  #
  "trialInformation.populationAgeGroup.inUtero"               = "ctrInt",
  "trialInformation.populationAgeGroup.pretermNewbornInfants" = "ctrInt",
  "trialInformation.populationAgeGroup.newborns"              = "ctrInt",
  "trialInformation.populationAgeGroup.infantsAndToddlers"    = "ctrInt",
  "trialInformation.populationAgeGroup.children"              = "ctrInt",
  "trialInformation.populationAgeGroup.adolescents"           = "ctrInt",
  "trialInformation.populationAgeGroup.adults"                = "ctrInt",
  "trialInformation.populationAgeGroup.elderly65To84"         = "ctrInt",
  "trialInformation.populationAgeGroup.elderlyOver85"         = "ctrInt",
  #
  # - CTGOV
  "number_of_arms" = "ctrInt",
  "enrollment"     = "ctrInt",
  #
  # - ISRCTN
  "participants.targetEnrolment"     = "ctrInt",
  "participants.totalFinalEnrolment" = "ctrInt",
  #
  # - CTIS
  "totalNumberEnrolled" = "ctrInt"
  #
)



#### functions ####

#' Check, write, read cache object for ctrdata
#'
#' @param xname name of variable to read or write
#'
#' @param xvalue value of variable to write
#'
#' @param verbose set to `TRUE` to print debug info
#'
#' @keywords internal
#' @noRd
#'
#' @return value of variable or `NULL` if variable does not exist
#'
ctrCache <- function(xname, xvalue = NULL, verbose = FALSE) {

  # hidden environment .ctrdataenv created in onload.R

  # write or overwrite and exit early
  if (!is.null(xvalue)) {
    assign(x = xname, value = xvalue, envir = .ctrdataenv)
    if (verbose) message(" wrote ", xname, " to cache ")
    return(xvalue)
  }

  # check and read any value for xname variable
  if (verbose) message(" accessing cache...", appendLF = FALSE)
  if (exists(x = xname, envir = .ctrdataenv)) {
    tmp <- try(get(x = xname, envir = .ctrdataenv), silent = TRUE)
    if (inherits(tmp, "try-error")) return(NULL)
    if (verbose) message("\b\b\b, returning ", xname, " ", appendLF = FALSE)
    return(tmp)
  }

  # default
  return(NULL)
}


#' Check and prepare nodbi connection object for ctrdata
#'
#' @param con A connection object, see section
#' `Databases` in \link{ctrdata-package}
#'
#' @keywords internal
#'
#' @importFrom nodbi src_sqlite src_duckdb docdb_list
#' @importFrom utils capture.output
#'
#' @return Connection object as list, with collection
#'  element under root
#'
ctrDb <- function(
  con = nodbi::src_sqlite(
    collection = "ctrdata_auto_generated")) {

  ## postgres
  if (inherits(con, "src_postgres")) {

    if (is.null(con$collection)) {
      stop("Specify 'collection' with a table name, using ",
           "<nodbi src_postgres object>[[\"collection\"]] <- \"test\"), ",
           "for package ctrdata to work.",
           call. = FALSE)
    }

    # add database as element under root
    con <- c(con,
             "db" = con$dbname,
             "ctrDb" = TRUE)

    ## return
    return(structure(con,
                     class = c("src_postgres", "docdb_src")))
  }

  ## sqlite
  if (inherits(con, "src_sqlite")) {

    if (is.null(con$collection)) {
      stop("Specify parameter 'collection' with a table name, ",
           "such as nodbi::src_sqlite(collection = 'test'), ",
           "for package ctrdata to work.",
           call. = FALSE)
    }

    # check
    if (inherits(try(nodbi::docdb_list(con), silent = TRUE), "try-error")) {
      con <- nodbi::src_sqlite(dbname = con$dbname,
                               collection = con$collection)
    }

    # add database as element under root
    con <- c(con,
             "db" = con$dbname,
             "ctrDb" = TRUE)

    # print warning from nodbi::src_sqlite()
    if (grepl(":memory:", con$dbname)) {
      warning("Database not persisting,\ncopy to persistant database like ",
              "this:\n\nRSQLite::sqliteCopyDatabase(",
              "\n  from = <your in-memory-database-object>$con,",
              "\n  to = RSQLite::dbConnect(RSQLite::SQLite(),",
              "\n                          dbname = 'local_file.db'))\n",
              call. = FALSE,
              noBreaks. = FALSE,
              immediate. = TRUE)
    }

    ## return
    return(structure(con,
                     class = c("src_sqlite", "docdb_src")))
  }

  ## mongo
  if (inherits(con, "src_mongo")) {

    # rights may be insufficient to call info(),
    # hence this workaround that should always
    # work and be stable to retrieve name of
    # collection in the mongo connection
    # suppress... for reconnect info from mongolite
    coll <- suppressMessages(utils::capture.output(con$con)[1])
    coll <- sub("^.*'(.*)'.*$", "\\1", coll)

    # add collection as element under root
    con <- c(con,
             "collection" = coll,
             "ctrDb" = TRUE)

    ## return
    return(structure(con,
                     class = c("src_mongo", "docdb_src")))
  }

  ## duckdb
  if (inherits(con, "src_duckdb")) {

    if (is.null(con$collection)) {
      stop("Specify parameter 'collection' with a table name, ",
           "such as nodbi::src_duckdb(collection = 'test'), ",
           "for package ctrdata to work.",
           call. = FALSE)
    }

    # check
    if (inherits(try(nodbi::docdb_list(con), silent = TRUE), "try-error")) {
      con <- nodbi::src_duckdb(
        dbdir = attr(attr(con$con, "driver"), "dbdir"),
        collection = con$collection)
    }

    # add database as element under root
    con <- c(con,
             "db" = attr(attr(con$con, "driver"), "dbdir"),
             "ctrDb" = TRUE)

    # print warning about nodbi::src_duckdb()
    if (grepl(":memory:", attr(attr(con$con, "driver"), "dbdir"))) {
      warning("Database not persisting\n",
              call. = FALSE,
              noBreaks. = FALSE,
              immediate. = TRUE)

    }

    ## return
    return(structure(con,
                     class = c("src_duckdb", "docdb_src")))

  }

  ## unprepared for other nodbi adapters so far
  stop("Please specify in parameter 'con' a database connection. ",
       "crdata supports src_mongo(), src_sqlite(), src_postgres() and src_duckdb().",
       call. = FALSE)

} # end ctrDb


#' Open stored query in register or its search page
#'
#' Open advanced search pages of register(s), or execute search in browser
#'
#' @param url of search results page to show in the browser. To open the
#'   browser with a previous search, the output of \link{ctrGetQueryUrl}
#'   or \link{dbQueryHistory} can be used. Can be left empty
#'   to open the advanced search page of the \code{register}.
#'
#' @param register Register(s) to open, "EUCTR", "CTGOV", "ISRCTN" or "CTIS".
#'   Default is to open the advanced search page of the register.
#'
#' @param copyright (Optional) If set to \code{TRUE}, opens copyright pages of
#'   register(s).
#'
#' @param ... May include the deprecated \code{input} parameter.
#'
#' @export
#'
#' @return Always \code{TRUE}, invisibly.
#'
#' @examples
#'
#' # Open all and check copyrights before using registers
#' ctrOpenSearchPagesInBrowser(copyright = TRUE)
#'
#' # Open specific register advanced search page
#' ctrOpenSearchPagesInBrowser(register = "EUCTR")
#' ctrOpenSearchPagesInBrowser(register = "CTGOV")
#' ctrOpenSearchPagesInBrowser(register = "ISRCTN")
#' ctrOpenSearchPagesInBrowser(register = "CTIS")
#' ctrOpenSearchPagesInBrowser(url = "status=Ended", register = "CTIS")
#'
#' # Open all queries that were loaded into demo collection
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials")
#'
#' dbh <- dbQueryHistory(
#'   con = dbc)
#'
#' for (r in seq_len(nrow(dbh))) {
#'   ctrOpenSearchPagesInBrowser(dbh[r, ])
#' }
#'
ctrOpenSearchPagesInBrowser <- function(
  url = "",
  register = "",
  copyright = FALSE,
  ...) {

  ## FIXME migrate from previously used parameter "input"
  tmp <- list(...)
  tmp <- tmp[["input"]]
  if (length(tmp)) {
    url <- tmp
    warning("Parameter 'input' is deprecated, use 'url' instead.",
            call. = FALSE)
  }

  ## in case a browser is not available
  ctrOpenUrl <- function(u) {
    try(utils::browseURL(u), silent = TRUE)
  }

  ## check combination of arguments to select action

  # - open all registers if no parameter is specified
  if (all(register == "") && all(url == "")) {
    sapply(
      c("https://www.clinicaltrialsregister.eu/ctr-search/search",
        "https://clinicaltrials.gov/ct2/search/advanced",
        "https://www.isrctn.com/editAdvancedSearch",
        "https://euclinicaltrials.eu/app/#/search"),
      ctrOpenUrl)
  }

  # - open copyright or similar pages
  if (copyright) {
    sapply(
      c("https://www.clinicaltrialsregister.eu/disclaimer.html",
        "https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use",
        "https://www.isrctn.com/page/faqs#usingISRCTN",
        "https://euclinicaltrials.eu/data-protection-and-privacy/"),
      ctrOpenUrl)
  }

  # - open from url, or query and register
  if (is.atomic(url) && url != "") {
    url <- ctrGetQueryUrl(url = url, register = register)
  }

  # - get from a data frame, such as from
  #   ctrQueryHistoryInDb() or ctrGetQueryUrl()
  if (is.data.frame(url) &&
      all(substr(names(url), 1, 6) == "query-")) {
    nr <- nrow(url)
    if (nr > 1L) warning("Using last query",
                         call. = FALSE, immediate. = TRUE)
    register  <- url[nr, "query-register", drop = TRUE]
    url <- url[nr, "query-term", drop = TRUE]
  }

  # - open from url and register
  if (is.atomic(url) && url != "" && register != "") {
    url <- switch(
      register,
      "EUCTR" = paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?", url, "#tabs"),
      "CTGOV" = paste0("https://clinicaltrials.gov/ct2/results?", url),
      "ISRCTN" = paste0("https://www.isrctn.com/search?", url),
      "CTIS" = paste0("https://euclinicaltrials.eu/app/#/search?", url)
    )
    ctrOpenUrl(url)
    return(url)
  }

  # - open register
  if (is.atomic(url) && url == "" && register != "") {
    url <- switch(
      register,
      "EUCTR" = paste0("https://www.clinicaltrialsregister.eu/ctr-search/search"),
      "CTGOV" = paste0("https://clinicaltrials.gov/ct2/results/refine"),
      "ISRCTN" = paste0("https://www.isrctn.com/editAdvancedSearch"),
      "CTIS" = paste0("https://euclinicaltrials.eu/app/#/search")
    )
    ctrOpenUrl(url)
    return(url)
  }

  # if not returned before
  invisible(NULL)
}
# end ctrOpenSearchPagesInBrowser


#' Get query details
#'
#' Extracts query parameters and register name from parameter `url` or
#' from the clipboard, into which the URL of a register search was copied.
#'
#' @param url URL such as from the browser address bar.
#' If not specified, clipboard contents will be checked for
#' a suitable URL.
#' For automatically copying the user's query of a register
#' in a web browser to the clipboard, see
#' \ifelse{latex}{\out{\href{https://github.com/rfhb/ctrdata\#3-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://github.com/rfhb/ctrdata#3-script-to-automatically-copy-users-query-from-web-browser}{here}}.
#' Can also contain a query term such as from
#' \link{dbQueryHistory}()["query-term"].
#'
#' @param register Optional name of register (one of "EUCTR", "CTGOV",
#' "ISRCTN" or "CTIS") in case url is a query term
#'
#' @export
#'
#' @return A data frame (or tibble, if \code{dplyr} is loaded)
#' with column names `query-term` and `query-register`.
#' The data frame (or tibble) can be passed as such as parameter
#' `query-term` to \link{ctrLoadQueryIntoDb} and as parameter
#' `url` to \link{ctrOpenSearchPagesInBrowser}.
#'
#' @importFrom clipr read_clip
#' @importFrom dplyr as_tibble
#'
#' @examples
#'
#' # user copied into the clipboard the URL from
#' # the address bar of the browser that shows results
#' # from a query in one of the trial registers
#' try(ctrGetQueryUrl(), silent = TRUE)
#'
#' # extract query parameters from search result URL
#' # (URL was cut for the purpose of formatting only)
#' ctrGetQueryUrl(
#'   url = paste0("https://clinicaltrials.gov/ct2/results?",
#'   "cond=&term=AREA%5BMaximumAge%5D+RANGE%5B0+days%2C+28+days%5D",
#'   "&type=Intr&rslt=&age_v=&gndr=&intr=Drugs%2C+Investigational",
#'   "&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=",
#'   "&locn=&phase=2&rsub=&strd_s=01%2F01%2F2015&strd_e=01%2F01%2F2016",
#'   "&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&rfpd_s=&rfpd_e=&lupd_s=&lupd_e=&sort="))
#'
ctrGetQueryUrl <- function(
  url = "",
  register = "") {
  #
  # check parameters expectations
  if (!is.atomic(url) || !is.atomic(register) ||
      is.null(url) || is.null(register) ||
      !inherits(url, "character") || !inherits(register, "character") ||
      length(url) != 1L || length(register) != 1L ||
      is.na(url) || is.na(register)) {
    stop("ctrGetQueryUrl(): 'url' and / or 'register' ",
         "is not a single character string, url: '",
         url, "', register: '", register, "'",
         call. = FALSE)
  }
  #
  # if no parameter specified,
  # check clipboard contents
  if (nchar(url) == 0L && register != "CTIS") {
    url <- try(suppressWarnings(
      clipr::read_clip(
        allow_non_interactive = TRUE)),
      silent = TRUE)
    if (inherits(url, "try-error")) url <- ""
    if (is.null(url) || (length(url) != 1L) || (nchar(url) == 0L) ||
        grepl(regQueryterm, url) ||
        !grepl("^https://", url)) {
      stop("ctrGetQueryUrl(): no clinical trial register ",
           "search URL found in parameter 'url' or in clipboard.",
           call. = FALSE)
    }
    message("* Using clipboard content as register query URL: ", url)
  }
  #
  #
  if (register != "" && grepl("^http", url)) {
    warning("Full URL but also 'register' specified; ",
            "continuing with register = ''", immediate. = TRUE)
    register <- ""
  }
  #
  # identify domain and register short name
  registerFromUrl <- switch(
      sub("^https://[w]{0,3}[.]?([a-zA-Z.]+)/.*", "\\1", url),
      "clinicaltrialsregister.eu" = "EUCTR",
      "clinicaltrials.gov" = "CTGOV",
      "isrctn.com" = "ISRCTN",
      "beta.clinicaltrials.gov" = "BETACTGOV",
      "euclinicaltrials.eu" = "CTIS",
      "NONE")
  #
  # check parameters expectations
  if (register != "" && registerFromUrl != "NONE" && register != registerFromUrl) {
    stop("ctrGetQueryUrl(): 'url' and / or 'register' mismatch, url: '",
         deparse(url), "', register: '", deparse(register), "'",
         call. = FALSE)
  } else {
    if (registerFromUrl != "NONE") register <- registerFromUrl
  }
  #
  outdf <- function(qt, reg) {
    qt <- utils::URLdecode(qt)
    message("* Found search query from ", reg, ": ", qt)
    out <- data.frame(
      `query-term` = qt,
      `query-register` = reg,
      check.names = FALSE,
      stringsAsFactors = FALSE)
    if (any("dplyr" == .packages())) out <- dplyr::as_tibble(out)
    return(out)
  }
  # identify query term per register
  #
  if (register == "EUCTR") {
    # search result page
    queryterm <- sub(".*/ctr-search/search[?](.*)", "\\1", url)
    # single trial page
    queryterm <- sub(paste0(".*/ctr-search/trial/(", regEuctr, ")/.*"),
                     "\\1", queryterm)
    # remove any intrapage anchor, e.g. #tableTop
    queryterm <- sub("#.+$", "", queryterm)
    # sanity correction for naked terms
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)([ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
      "\\1query=\\2\\3", queryterm)
    # check if url was for results of single trial
    if (grepl(".*/results$", url)) {
      queryterm <- paste0(queryterm, "&resultsstatus=trials-with-results")
    }
    #
    return(outdf(queryterm, register))
  }
  #
  if (register == "CTGOV") {
    #
    queryterm <- sub(paste0(".*/ct2/show/[recodsult/]*(", regCtgov, ")([?][a-z]+.*|$)"),
                     "\\1", url)
    # single trial page
    if (grepl("[?][a-z]+=\\w+", url, perl = TRUE) &&
        grepl(paste0("^", regCtgov, "$"), queryterm)) {
      message("* Note: 'url' shows a single trial (and is returned by the ",
              "function) but also had search parameters: If interested in ",
              "search results, click 'Return to List' in browser and use ",
              "this as 'url'.")
    }
    # expert search page
    queryterm <- sub(".*/ct2/results/refine[?](.*)", "\\1", queryterm)
    # search results page
    queryterm <- sub(".*/ct2/results[?](.*)", "\\1", queryterm)
    # other results page
    queryterm <- sub("(.*)&Search[a-zA-Z]*=(Search|Find)[a-zA-Z+]*",
                     "\\1", queryterm)
    # remove empty parameters
    queryterm <- gsub("[a-z_0-9]+=&", "", queryterm)
    queryterm <- sub("&[a-z_0-9]+=$", "", queryterm)
    # correct naked terms
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)(\\w+|[a-zA-z0-9+-.:]+)($|&\\w+=\\w+)",
      "\\1term=\\2\\3", queryterm)
    #
    return(outdf(queryterm, register))
  }
  #
  if (register == "ISRCTN") {
    # single trial page
    queryterm <- sub(paste0("^.*/ISRCTN(", regIsrctn, ")$"),
                     "ISRCTN\\1", url)
    # search results page
    queryterm <- sub(".*/search[?](.*)", "\\1", queryterm)
    # remove unnecessary parameter
    queryterm <- sub("&searchType=[a-z]+-search", "", queryterm)
    # correct naked terms
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)(\\w+|[ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
      "\\1q=\\2\\3", queryterm)
    #
    return(outdf(queryterm, register))
  }
  #
  if (register == "BETACTGOV") {
    #
    stop("The beta website of ClinicalTrials.gov is not supported, ",
         "please use the classic website. Package 'ctrdata' is being ",
         "prepared to use the forthcoming website's functionality.")
    #
    return(invisible(NULL))
  }
  #
  if (register == "CTIS") {
    # url can be empty to retrieve all or looks like
    # https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup?ageGroupCode=3
    queryterm <- sub(
      "https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup[?]", "", url)
    # or https://euclinicaltrials.eu/app/#/search?status=Ended
    queryterm <- sub(
      "https://euclinicaltrials.eu/app/#/search[?]?", "", queryterm)
    # remove unnecessary components
    queryterm <- sub("&?paging=[-,0-9]+", "", queryterm)
    queryterm <- sub("&?sorting=[-a-zA-Z]+", "", queryterm)
    queryterm <- sub("&?isEeaOnly=false", "", queryterm)
    queryterm <- sub("&?isNonEeaOnly=false", "", queryterm)
    queryterm <- sub("&?isBothEeaNonEea=false", "", queryterm)
    #
    return(outdf(queryterm, register))
  }
  #
  # default / NONE
  warning("ctrGetQueryUrl(): no clinical trial register ",
          "search URL found in parameter 'url' or in clipboard.",
          call. = FALSE, immediate. = TRUE)
  #
  return(invisible(NULL))
}
# end ctrGetQueryUrl


#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name (INN), a trade or product name, or a company code(s).
#' Retrieves the substances which are searched for by the register
#' 'ClinicalTrials.Gov' for a given active substance.
#'
#' @param activesubstance An active substance, in an atomic character vector
#'
#' @return A character vector of the active substance (input parameter) and
#'  synonyms, or NULL if active substance was not found and may be invalid
#'
#' @importFrom httr GET
#' @importFrom rvest html_element html_table read_html
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#' # [1] "imatinib" "gleevec" "sti 571" "glivec" "CGP 57148" "st1571"
#'
#' }
ctrFindActiveSubstanceSynonyms <- function(activesubstance = "") {

  # check parameters
  if ((length(activesubstance) != 1L) ||
      !is.character(activesubstance) ||
      (nchar(activesubstance) == 0L)) {
    stop("ctrFindActiveSubstanceSynonyms(): ",
         "activesubstance should be a single string.",
         call. = FALSE)
  }

  # getting synonyms using httr since rvest::read_html
  # does not close network connection in case of 404
  ctgovfirstpageurl <-
    utils::URLencode(
      paste0("https://clinicaltrials.gov/ct2/results/details?term=",
             activesubstance))

  # set user agent for httr and curl to inform registers
  httr::set_config(httr::user_agent(
    paste0("ctrdata/", utils::packageDescription("ctrdata")$Version)))

  # get webpage
  tmp <- try({
    httr::GET(url = ctgovfirstpageurl)
  }, silent = TRUE)

  # check result
  if (tmp[["status_code"]] == 404L) {
    # 404 means active substance not found, thus early exit
    message("Check active substance '", activesubstance, "', may not exist.")
    return(NULL)
  }

  # make page content accessible to rvest
  tmp <- rvest::read_html(httr::content(tmp, as = "text"))

  # extract from table "Terms and Synonyms Searched:"
  tmp <- rvest::html_element(
    tmp, xpath = '//*[@id="searchdetail"]//table[1]')
  tmp <- rvest::html_table(tmp, fill = TRUE)
  asx <- tmp[["Terms"]]
  asx <- asx[!grepl(
    paste0("(more|synonyms|terms|", activesubstance, "|",
           paste0(unlist(strsplit(activesubstance, " "), use.names = FALSE),
                  collapse = "|"), ")"), asx,
    ignore.case = TRUE)]

  # prepare and return output
  asx <- c(activesubstance, asx)
  return(unique(asx))
}
# end ctrFindActiveSubstanceSynonyms


#' Show history of queries loaded into a database collection
#'
#' @inheritParams ctrDb
#'
#' @return A data frame (or tibble, if \code{dplyr} is loaded)
#'  with columns: `query-timestamp`, `query-register`,
#'  `query-records` (note: this is the number of records loaded when last
#'  executing \link{ctrLoadQueryIntoDb}, not the total record number) and
#'  `query-term`, with one row for each time \link{ctrLoadQueryIntoDb}
#'  loaded trial records into this collection.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#' @importFrom dplyr as_tibble
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' dbQueryHistory(con = dbc)
#'
dbQueryHistory <- function(con, verbose = FALSE) {

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # debug
  if (verbose) message("Running dbQueryHistory ...")

  hist <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{"_id": {"$eq": "meta-info"}}',
    fields = '{"queries": 1}')

  # check if meeting expectations
  if (is.null(hist) ||
      nrow(hist) == 0L) {
    #
    message("No history found in expected format.")
    #
    # return (class data.frame is expected)
    return(invisible(data.frame(NULL)))
    #
  }

  # access data frame of queries
  hist <- hist[["queries"]][[1]]

  # inform user
  if (verbose) {

    message("Number of queries in history of \"",
            con$collection, "\": ", nrow(hist))

    # total number of records in collection to inform user
    countall <- length(nodbi::docdb_query(
      src = con,
      key = con$collection,
      query =  '{"_id": {"$ne": "meta-info"}}',
      fields = '{"_id": 1}')[["_id"]])

    message("Number of records in collection \"",
            con$collection, "\": ", countall)

  }

  # return
  if (any("dplyr" == .packages())) return(dplyr::as_tibble(hist))
  return(hist)

}
# end ctrQueryHistoryInDb


#' Find names of fields in the database collection
#'
#' Given part of the name of a field of interest to the user, this
#' function returns the full field names used in records that were
#' previously loaded into a collection
#' (using \link{ctrLoadQueryIntoDb}). The field names can be fed
#' into function \link{dbGetFieldsIntoDf} to extract the data
#' from the collection into a data frame.
#' In addition to the full names of leaf fields (e.g.,
#' \code{clinical_results.outcome_list.outcome.measure.class_list.class.title})
#' this function also returns names of node fields (e.g.,
#' \code{clinical_results}). Data in node fields is typically complex
#' (multiply nested) and can be converted into individual data
#' elements by function \link{dfTrials2Long}, possibly followed
#' by function \link{dfName2Value}.
#'
#' For fields in EUCTR (protocol- and results-related information),
#' \url{https://eudract.ema.europa.eu/result.html}.
#'
#' For fields in CTGOV (protocol-related information), see
#' \url{https://prsinfo.clinicaltrials.gov/definitions.html}.
#'
#' For fields in ISRCTN (protocol-related information), see
#' \url{https://www.isrctn.com/page/definitions}.
#'
#' Note: Only when `dbFindFields` is first called after
#' \link{ctrLoadQueryIntoDb}, it will take a moment.
#'
#' @param namepart A plain string (can include a regular expression,
#' including Perl-style) to be searched for among all field names
#' (keys) in the collection. Use `".*` to find all fields.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#'
#' @inheritParams ctrDb
#'
#' @return Vector of strings with full names of field(s) found,
#' in alphabetical order by register. This is a named vector
#' where the names of the vector are the register names for
#' the respective fields.
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' dbFindFields(namepart = "date", con = dbc)
#'
dbFindFields <- function(namepart = "",
                         con,
                         verbose = FALSE) {

  ## sanity checks
  if (!is.atomic(namepart)) stop("'namepart' should be atomic.", call. = FALSE)
  if (length(namepart) > 1) stop("'namepart' should have one element.", call. = FALSE)
  if (namepart == "")       stop("Empty 'namepart' parameter.", call. = FALSE)

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  ## check if cache environment has entry for the database
  keyslist <- ctrCache(xname = paste0("keyslist_", con$db, "/", con$collection),
                       verbose = verbose)

  ## get cache reference value
  cacheRef <- as.character(rev(unlist(try(nodbi::docdb_query(
    src = con, key = con$collection, query = '{"_id": "meta-info"}',
    fields = '{"queries.query-timestamp": 1}'), silent = TRUE)))[1])

  ## invalidate cache
  cacheOutdated <- is.null(keyslist) || (cacheRef != ctrCache(
    xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
    verbose = verbose))

  ## get keyslist
  if (cacheOutdated) {

    # inform user
    message("Finding fields in database collection (may take some time)")

    # helpder function
    getNodes <- function(fn) {
      nodesList <- strsplit(fn, split = ".", fixed = TRUE)
      nodesList <- sapply(nodesList, function(i) {
        i <- i[-length(i)]
        sapply(
          seq_along(i),
          function(ii) paste0(i[1:ii], collapse = "."), USE.NAMES = FALSE)
      }, USE.NAMES = FALSE)
      return(unique(unlist(nodesList)))
    }

    # helper function
    normNames <- function(df) {
      out <- names(unlist(df))
      if (!length(out)) return("")
      out <- ifelse(
        # exception for euctr protocol and results fields
        test = grepl("65To84|Over85|under_18", out),
        yes = out,
        no = sub("[0-9]+$", "", out))
      out <- c(out, getNodes(out))
      return(sort(unique(out)))
    }

    # get all ids
    allIds <- nodbi::docdb_query(
      src = con, key = con$collection,
      fields = '{"_id": 1}', query = '{}')[["_id"]]

    # queries to be used
    queries <- list(
      "EUCTR" = c(
        '{"trialInformation.analysisStage.value": {"$regex": ".+"}}',
        paste0('{"_id": "', rev(allIds[grepl(paste0("^", regEuctr, "-[3]?[A-Z]{2}$"), allIds)])[1], '"}')),
      "CTGOV" = c(
        '{"results_first_submitted": {"$regex": ".+"}}',
        paste0('{"_id": "', rev(allIds[grepl(regCtgov, allIds)])[1], '"}')),
      "ISRCTN" = c(
        '{"results.publicationStage": "Results"}',
        paste0('{"_id": "', rev(allIds[grepl(regIsrctn, allIds)])[1], '"}')),
      "CTIS" = c(
        paste0('{"_id": "', sample(c("", allIds[grepl(regCtis, allIds)]), 1), '"}'),
        paste0('{"_id": "', sample(c("", allIds[grepl(regCtis, allIds)]), 1), '"}'))
    )

    # get names
    keyslist <- NULL
    for (q in seq_along(queries)) {
      # first use queries for records with results
      keysAdd <- normNames(nodbi::docdb_query(
        src = con, key = con$collection,
        query = queries[[q]][1], limit = 1L))
      # give keys name of register
      names(keysAdd) <- rep(names(queries)[q], length(keysAdd))
      keyslist <- c(keyslist, keysAdd)
      # second query for highest = latest _id
      keysAdd <- normNames(nodbi::docdb_query(
        src = con, key = con$collection,
        query = queries[[q]][2], limit = 1L))
      # give keys name of register
      names(keysAdd) <- rep(names(queries)[q], length(keysAdd))
      keyslist <- c(keyslist, keysAdd)
    }

    # clean empty entries and exclude _id for consistency
    # since different approaches above return _id or not
    keyslist <- keyslist[!duplicated(keyslist)]
    keyslist <- keyslist[keyslist != "_id" & keyslist != ""]
    keyslist <- sub("[.]$", "", keyslist)

    ## store keyslist to environment (cache)
    if (length(keyslist) > 1) {
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection),
        xvalue = keyslist, verbose = verbose)
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
        xvalue = cacheRef, verbose = verbose)
      message("Field names cached for this session.")
    }

  } else {

    message("Using cache of fields.")

  } # generate keyslist

  ## inform user of unexpected situation
  if ((length(keyslist) == 0) || all(keyslist == "")) {
    warning("No keys could be extracted, please check database ",
            "and collection: ", con$db, "/", con$collection, call. = FALSE)
  }

  ## now do the actual search and find for key name parts
  fields <- keyslist[grepl(pattern = namepart, x = keyslist,
                           ignore.case = TRUE, perl = TRUE)]

  # return value if no fields found
  if (!length(fields)) fields <- ""

  # return the match(es)
  return(fields)

} # end dbFindFields


#' Get identifiers of deduplicated trial records
#'
#' Records for a clinical trial can be loaded from more than one
#' register into a collection. The function returns identifiers of
#' records in the collection that were loaded from the
#' register(s) preferred by the user. All registers are recording
#' identifiers also from other registers, which are used by this
#' function to provide a vector of identifiers of deduplicated trials.
#'
#' Note that the content of records may differ between registers
#' (and, for EUCTR, between records for different Member States).
#' Such differences are not considered by this function.
#'
#' @param preferregister A vector of the order of preference for
#' registers from which to generate unique _id's, default
#' \code{c("EUCTR", "CTGOV", "ISRCTN", "CTIS")}
#'
#' @inheritParams dfFindUniqueEuctrRecord
#'
#' @param verbose If set to \code{TRUE}, prints out which fields
#'  of the registers are used to find corresponding trial records
#'
#' @importFrom nodbi docdb_query
#' @importFrom stats setNames
#'
#' @inheritParams ctrDb
#'
#' @return A named vector with strings of keys ("_id") of records in
#' the collection that represent unique trials, where names correspond
#' to the register of the record.
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' dbFindIdsUniqueTrials(con = dbc)
#'
dbFindIdsUniqueTrials <- function(
  preferregister = c("EUCTR", "CTGOV", "ISRCTN", "CTIS"),
  prefermemberstate = "DE",
  include3rdcountrytrials = TRUE,
  con,
  verbose = FALSE) {

  # parameter checks
  if (!all(preferregister %in% registerList)) {
    stop("'preferregister' not known: ", preferregister, call. = FALSE)
  }
  if (length(prefermemberstate) != 1L |
      !any(prefermemberstate == countriesEUCTR)) {
    stop("'prefermemberstate' not known: ", prefermemberstate, call. = FALSE)
  }
  # complete if preferregister does not have all
  preferregister <- unique(preferregister)
  preferregister <- union(preferregister, registerList)

  # objective: create a vector of database record identifiers (_id)
  # that represent unique records of clinical trials, based on user's
  # preferences for selecting the preferred from any multiple records

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # inform user
  message("Searching for duplicate trials... ")

  # fields for database query
  fields <- c(
    "ctrname",
    # euctr
    "a2_eudract_number",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a51_isrctn_international_standard_randomised_controlled_trial_number",
    "trialInformation.isrctnIdentifier",
    "a41_sponsors_protocol_code_number",
    # ctgov
    "id_info",
    # isrctn
    "externalRefs",
    "isrctn",
    # ctis
    "ctNumber",
    "eudraCtInfo.eudraCtCode"
  )

  # check if cache environment has entry for the database
  listofIds <- ctrCache(
    xname = paste0("listofids_", con$db, "/", con$collection),
                   verbose = FALSE)

  # get cache reference value
  cacheRef <- as.character(rev(unlist(try(nodbi::docdb_query(
    src = con, key = con$collection, query = '{"_id": "meta-info"}',
    fields = '{"queries.query-timestamp": 1}'), silent = TRUE)))[1])

  # cache validity
  cacheOutdated <- is.null(listofIds) || (cacheRef != ctrCache(
    xname = paste0("listofids_", con$db, "/", con$collection, "_timestamp"),
    verbose = FALSE))

  # inform user
  message(" - Getting all trial identifiers...", appendLF = FALSE)

  # cache outdated
  if (cacheOutdated) {

    # inform user
    message("\b\b\b (may take some time)...", appendLF = FALSE)

    # get identifiers
    listofIds <- try(suppressMessages(suppressWarnings(
      dbGetFieldsIntoDf(
        fields = fields,
        con = con,
        verbose = FALSE,
        stopifnodata = FALSE)
    )),
    silent = TRUE
    )

    # error check
    if (inherits(listofIds, "try-error") ||
        !length(listofIds) || !nrow(listofIds)) {
      stop("No records found, check collection '", con$collection, "'",
           call. = FALSE)
    }

    # write cache entries
    ctrCache(
      xname = paste0("listofids_", con$db, "/", con$collection),
      xvalue = listofIds, verbose = FALSE)
    ctrCache(
      xname = paste0("listofids_", con$db, "/", con$collection, "_timestamp"),
      xvalue = cacheRef, verbose = FALSE)

  } # if outdated

  # inform user
  message("\b\b\b, ", nrow(listofIds), " found in collection")

  # copy attributes
  attribsids <- attributes(listofIds)

  # target fields for further steps in this function
  fields <- c(
    "_id",
    "ctrname",
    # euctr
    "a2_eudract_number",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a51_isrctn_international_standard_randomised_controlled_trial_number",
    "trialInformation.isrctnIdentifier",
    "a41_sponsors_protocol_code_number",
    # ctgov
    "id_info.secondary_id",
    "id_info.org_study_id",
    "id_info.nct_id",
    "id_info.nct_alias",
    "id_info.secondary_id",
    "id_info.secondary_id",
    "id_info.org_study_id",
    # isrctn
    "externalRefs.eudraCTNumber",
    "externalRefs.clinicalTrialsGovNumber",
    "isrctn",
    "externalRefs.protocolSerialNumber",
    # ctis
    "ctNumber",
    "eudraCtInfo.eudraCtCode"
  )
  if (verbose) message(
    "\nFields used for finding corresponding register records of trials: ",
    "\n\n", paste0(fields, collapse = ", "), "\n")

  # add any missing columns
  missFields <- setdiff(fields, names(listofIds))
  if (length(missFields)) {
    missCols <- matrix(nrow = nrow(listofIds), ncol = length(missFields))
    missCols <- data.frame(missCols)
    names(missCols) <- missFields
    listofIds <- cbind(listofIds, missCols)
  }

  # replicate columns to make data frame fit subsequent steps
  listofIds <- listofIds[, fields, drop = FALSE]

  # rename columns for content mangling, needs to
  # correspond to columns and sequence in "fields"
  # for mapping identifiers across registers
  names(listofIds) <- c(
    "_id", "ctrname",
    # euctr
    "euctr.1", "ctgov.1a", "ctgov.1b", "isrctn.1a", "isrctn.1b", "sponsor.1",
    # ctgov
    "euctr.2a", "euctr.2b", "ctgov.2a", "ctgov.2b", "isrctn.2",
    "sponsor.2a", "sponsor.2b",
    # isrctn
    "euctr.3", "ctgov.3", "isrctn.3", "sponsor.3",
    # ctis
    "ctis.1", "euctr.4"
  )

  # keep only relevant content
  # - in certain raw value columns
  colsToMangle <- list(
    c("ctgov.1a", regCtgov),
    c("ctgov.1b", regCtgov),
    c("ctgov.2a", regCtgov),
    c("ctgov.2b", regCtgov),
    c("isrctn.1a", regIsrctn),
    c("isrctn.1b", regIsrctn),
    c("isrctn.2", regIsrctn),
    c("isrctn.3", regIsrctn),
    c("euctr.1", regEuctr),
    c("euctr.2a", regEuctr),
    c("euctr.2b", regEuctr),
    c("euctr.3", regEuctr),
    c("ctis.1", regCtis),
    c("euctr.4", regEuctr)
  )
  # - do mangling; prerequisite is
  #   that each of the columns holds
  #   a single character vector,
  #   possibly collapsed with " / "
  invisible(sapply(
    colsToMangle,
    function(ctm) {
      colMangled <- regmatches(
        listofIds[[ ctm[[1]] ]],
        regexec(ctm[[2]], listofIds[[ ctm[[1]] ]]))
      colMangled[!lengths(colMangled)] <- ""
      listofIds[[ ctm[[1]] ]] <<- unlist(colMangled)
    }))
  # - merge columns for register ids and sponsor ids
  for (reg in c(registerList, "SPONSOR")) {
    listofIds[[reg]] <- apply(listofIds[
      , grepl(paste0("^", reg, "[.][0-9]"), names(listofIds),
              ignore.case = TRUE), drop = FALSE], MARGIN = 1,
      function(r) gsub("^ ?/ | / ?$", "",
                       paste0(na.omit(unique(r)), collapse = " / ")))
  }
  # - delete raw columns
  listofIds <- listofIds[
    , c("_id", "ctrname", registerList, "SPONSOR"), drop = FALSE]

  # inform user
  message(" - Finding duplicates among registers' and sponsor ids...")

  # find duplicates
  colsToCheck <- match(c(preferregister, "SPONSOR"), names(listofIds))
  outSet <- NULL
  for (i in seq_along(preferregister)) {

    # to be added
    tmp <- listofIds[
      listofIds[["ctrname"]] == preferregister[i], , drop = FALSE]
    row.names(tmp) <- NULL

    # check if second etc. set has identifiers
    # in the previously rbind'ed sets
    if (i > 1L && nrow(tmp)) {

      # check for duplicates
      dupes <- mapply(
        function(c1, c2) {
          tmpIs <- intersect(
            unlist(strsplit(c1, " / ")),
            unlist(strsplit(c2, " / ")))
          if (length(tmpIs)) {
            # map found intersecting names back
            # to the rows of the input data frame
            grepl(paste0(tmpIs, collapse = "|"), c1)
          } else {
            rep(FALSE, times = length(c1))
          }},
        tmp[, colsToCheck, drop = FALSE],
        outSet[, colsToCheck, drop = FALSE]
      )
      # mangle dupes for marginal cases, e.g. one record
      if (length(dim(dupes)) == 1L) dupes <- t(dupes)
      # keep uniques
      tmp <- tmp[rowSums(dupes) == 0L, , drop = FALSE]
      rm(dupes)
    }

    # add to output set
    outSet <- rbind(outSet, tmp,
                    make.row.names = FALSE,
                    stringsAsFactors = FALSE)
  }
  rm(tmp)

  # keep necessary columns
  listofIds <- outSet[, c("_id", "EUCTR", "ctrname")]
  names(listofIds)[2] <- "a2_eudract_number"
  rm(outSet)

  # find unique, preferred country version of euctr
  listofIds <- dfFindUniqueEuctrRecord(
    df = listofIds,
    prefermemberstate = prefermemberstate,
    include3rdcountrytrials = include3rdcountrytrials)

  # prepare output
  listofIds <- setNames(
    object = listofIds[["_id"]],
    nm = listofIds[["ctrname"]])

  # count
  countIds <- table(names(listofIds))

  # copy attributes
  attributes(listofIds) <- attribsids[grepl("^ctrdata-", names(attribsids))]

  # avoid returning list() if none found
  if (length(listofIds) == 0) listofIds <- character()

  # inform user
  message(" - Keeping ", paste0(countIds, collapse = " / "), " records",
          " from ", paste0(names(countIds), collapse = " / "))
  message(
    "= Returning keys (_id) of ", length(listofIds),
    " records in collection \"", con$collection, "\"")

  # return
  return(sort(listofIds))

}
# end dbFindIdsUniqueTrials


#' Create data frame of specified fields from database collection
#'
#' Fields in the collection are retrieved into a data frame (or tibble).
#' Note that fields within the record of a trial can be hierarchical
#' and structured, that is, nested.
#' Names of fields can be found with \link{dbFindFields}.
#' The function uses the field names to appropriately type the values
#' that it returns, harmonising original values (e.g. "Information not present
#' in EudraCT" becomes `NA`, "Yes" becomes `TRUE`, "false" becomes `FALSE`,
#' date strings become class Date, number strings become numbers).
#' The function attempts so simplify the structure of some nested data and
#' may concatenate multiple strings in a field using " / " (see below);
#' for complex nested data, use function \link{dfTrials2Long} followed by
#' \link{dfName2Value} to extract the desired nested variable(s).
#'
#' @param fields Vector of one or more strings, with names of sought fields.
#' See function \link{dbFindFields} for how to find names of fields.
#' "item.subitem" notation is supported.
#'
#' @param stopifnodata Stops with an error (detaul \code{TRUE}) or with
#' a warning (\code{FALSE}) if the sought field is empty in all,
#' or not available in any of the records in the database collection.
#'
#' @param verbose Printing additional information if set to \code{TRUE};
#' (default \code{FALSE}).
#'
#' @inheritParams ctrDb
#'
#' @return A data frame (or tibble, if \code{dplyr} is loaded)
#' with columns corresponding to the sought fields.
#' A column for the record `_id` will always be included.
#' Each column can be either a simple data type (numeric, character, date)
#' or a list. For complicated lists, use function
#' \link{dfTrials2Long} followed by function \link{dfName2Value} to
#' extract values for nested variables.
#' The maximum number of rows of the returned data frame is equal to,
#' or less than the number of records of trials in the database
#' collection.
#'
#' @importFrom nodbi docdb_query
#' @importFrom stats na.omit
#' @importFrom dplyr as_tibble
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' # get fields that are nested within another field
#' # and can have multiple values with the nested field
#' dbGetFieldsIntoDf(
#'   fields = "b1_sponsor.b31_and_b32_status_of_the_sponsor",
#'   con = dbc)
#'
#' # fields that are lists of string values are
#' # returned by concatenating values with a slash
#' dbGetFieldsIntoDf(
#'   fields = "keyword",
#'   con = dbc)
#'
dbGetFieldsIntoDf <- function(fields = "",
                              con, verbose = FALSE,
                              stopifnodata = TRUE) {

  # check parameters
  if (!is.vector(fields) |
      !all(class(fields) %in% "character")) {
    stop("Input should be a vector of strings of field names.", call. = FALSE)
  }

  # remove NA, NULL if included in fields
  fields <- fields[!is.null(fields) & !is.na(fields)]

  # remove _id if included in fields
  fields <- fields["_id" != fields]

  # check if valid fields
  if (any(fields == "") | (length(fields) == 0)) {
    stop("'fields' contains empty elements; ",
         "please provide a vector of strings of field names. ",
         "Function dbFindFields() can be used to find field names. ",
         call. = FALSE)
  }

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # get all ids to enable Reduce which would fail
  # due to holes from NULLs from the merge step
  dft <- nodbi::docdb_query(
    src = con,
    key = con$collection,
    query = '{}',
    fields = paste0('{"_id": 1}'))

  # early exit if no records
  if (!nrow(dft)) stop(
    "No data found in collection \"", con$collection, "\"", call. = FALSE)

  # continue with data frame of _id's
  dft <- dft[dft[["_id"]] != "meta-info", "_id", drop = FALSE]

  # initialise output
  nFields <- length(fields)
  accumNames <- NULL

  # iterate over fields so that we can
  # use a custom function to merge results
  result <- lapply(
    seq_len(nFields),
    function(i) {
      #
      item <- fields[i]

      # user info
      message(ifelse(i > 1L, "\n", ""), item, "... ", appendLF = FALSE)
      #
      tmpItem <- try({

        # execute query
        dfi <- nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = '{"_id": {"$ne": "meta-info"}}',
          fields = paste0('{"_id": 1, "', item, '": 1}'))
        message("\b\b\b\b   \b\b\b ", appendLF = FALSE)

        # leave try() early if no results
        if (!nrow(dfi) || ncol(dfi) == 1L) stop(simpleError("No data"))

        # remove any rows without index variable
        dfi <- dfi[!is.na(dfi[["_id"]]), , drop = FALSE]

        # simplify and replace NULL with NA
        dfi[[2]][!sapply(dfi[[2]], length)] <- NA

        # simplify by extracting recursively any requested subitem
        itemSegments <- strsplit(item, "[.]")[[1]]
        itemSegments <- setdiff(itemSegments, names(dfi))
        for (iS in itemSegments) {
          message(". ", appendLF = FALSE)
          if ((length(names(dfi[[2]])) == 1L) &&
              (iS == names(dfi[[2]]))) {
            dfi[[2]] <- dfi[[2]][[iS]]
          } else {
            # e.g. for "primary_outcome.measure" from MongoDB
            tn <- unlist(sapply(dfi[[2]], names))
            if (length(unique(tn)) == 1L && (iS == tn[1])) {
              dfi[[2]] <- lapply(dfi[[2]], "[[", 1)
            } else {
              # no more predictable simplification possible:
              # break to leave for loop over itemSegments
              break
            }
          }
        }

        # simplify by expanding a resulting data frame
        if (length(unique(names(dfi[[2]]))) > 1L) {
          item <- paste0(item, ".", names(dfi[[2]]))
          dfi <- cbind("_id" = dfi[["_id"]], as.data.frame(dfi[[2]]))
          message(". ", appendLF = FALSE)
          emptyCols <- sapply(dfi, function(c) all(is.na(c)))
          emptyCols <- seq_along(emptyCols)[emptyCols]
          if (length(emptyCols)) dfi <- dfi[, -emptyCols, drop = FALSE]
          if (length(emptyCols)) item <- item[-(emptyCols - 1L)]
        }

        # name result set
        names(dfi) <- c("_id", item)

        # create NA output from template
        dfo <- dft

        # simplify by processing columns
        for (c in seq_len(ncol(dfi))[-1]) {

          # inform user
          if (c > 2L) message(". ", appendLF = FALSE)

          # data frames with single rows are lists
          # turn such lists back into data frames
          # e.g. location.facility but not location
          # thus check names per row, data frame should
          # have more than one column name
          tmpDfs <- sapply(dfi[[c]], class) == "data.frame"
          tmpLst <- sapply(dfi[[c]], class) == "list"
          tmpLen <- sapply(dfi[[c]][ !sapply(dfi[[c]], is.null) ], length)
          if (any(tmpDfs) && any(tmpLst) &&
              all(tmpLen > 1L) && length(unique(tmpLen)) == 1L) {
            dfi[[c]][tmpLst] <- lapply(
              dfi[[c]][tmpLst], function(i) data.frame(
                do.call(rbind, i), check.names = FALSE))
          }

          # special case: column is one-column data frame
          if (is.data.frame(dfi[[c]]) && (ncol(dfi[[c]]) == 1L) &&
              (nrow(dfi[[c]]) == nrow(dfi))) {
            tn <- names(dfi[[c]])
            dfi[[c]] <- dfi[[c]][, 1, drop = TRUE]
            names(dfi)[c] <- paste0(names(dfi)[c], ".", tn)
          }

          # mangle column if not simply character
          if (typeof(dfi[[c]]) != "character") {

            # simplify and replace NULL with NA
            dfi[[c]][!sapply(dfi[[c]], length)] <- NA

            # simplify column with one-column data frames or
            # one-item list e.g. "primary_outcome.measure"
            if (!is.data.frame(dfi[[c]]) &&
                all(sapply(dfi[[c]], function(r)
                  (!is.atomic(r)) &&
                  ((length(unlist(r)) <= 1L) ||
                  (is.data.frame(r) && ncol(r) == 1L && nrow(r) > 0L))
                ))) {
              dfi[[c]] <- sapply(
                dfi[[c]], function(i) {
                  if (length(i))
                    if (!is.null(ncol(i[[1]])) && ncol(i[[1]]) > 1L)
                      i[1] else i[[1]]
                  else NA},
                USE.NAMES = FALSE, simplify = TRUE)
            }

            # concatenate data if any rows are of type character
            # and if there is no more complex structure
            # (thus, vector of types is not a named vector)
            rowName <- sapply(dfi[[c]], function(i) is.null(names(i)))
            rowName2 <- sapply(names(rowName), function(i) is.null(i))
            rowType <- sapply(
              dfi[[c]], function(i) typeof(unlist(i, recursive = FALSE)))
            #
            if (all(rowName) & all(rowName2) &
                length(unique(rowName)) <= 1L &
                any(rowType == "character")) {
              #
              dfi[[c]] <- sapply(dfi[[c]], function(i)
                if (length(i) > 1L) {
                  rowI <- paste0(i[!is.na(i)], collapse = " / ")
                  if (nchar(rowI)) rowI else NA
                  } else if (length(i) && !is.na(i)) i else NA)
            }

            # list of one-element lists such as dates
            if (any(sapply(dfi[[c]], class) == "Date")) {
              dfi[[c]] <- unlist(dfi[[c]], recursive = FALSE, use.names = FALSE)
              dfi[[c]] <- as.Date(dfi[[c]], origin = "1970-01-01")
            }

          } # if typeof

          # type after if typeof
          if (typeof(dfi[[c]]) == "character") {
            dfi[[c]] <- typeField(dfi[[c]], names(dfi)[c])
          }

          # add a column into copy of NA template
          dfo[[c]] <- switch(
            class(dfi[[c]])[1],
            "Date" = as.Date(NA),
            "numeric" = as.numeric(NA),
            "character" = as.character(NA),
            "data.frame" = NA,
            "integer" = as.integer(NA),
            "list" = NA,
            "logical" = as.logical(NA),
            NA
          )

        } # for processing columns

        # add NA where dfi has no data to avoid NULL when
        # merging with Reduce below, which otherwise raises
        #  Error in `[<-.data.frame`(`*tmp*`, value, value = NA) :
        #  new columns would leave holes after existing columns
        names(dfo) <- names(dfi)
        dfi <- suppressWarnings(
          rbind(dfo[!(dfo[["_id"]] %in% dfi[["_id"]]), , drop = FALSE], dfi))
        # suppressing the following which is related to adding a list into a
        # column that has NAs from dfo; warning does not occur with reversing
        # to dfi, dfo[] so that it seems acceptable to suppress warnings
        # Warning messages:
        # 1: In value[[jj]][ri] <- if (is.factor(xij)) as.vector(xij) else xij :
        #   number of items to replace is not a multiple of replacement length
        # 2: In names(value[[jj]])[ri] <- nm :
        #   number of items to replace is not a multiple of replacement length

      },
      silent = TRUE) # tmpItem try

      # inform user
      if (inherits(tmpItem, "try-error") ||
          !nrow(dfi) || (ncol(dfi) == 1L) ||
          is.null(dfi[[2]]) || all(is.na(dfi[[2]]))) {

        # try-error occurred or no data retrieved
        if (stopifnodata) {
          if (inherits(tmpItem, "try-error") &&
              !attr(tmpItem, "condition")["message"] == "No data") message(
                "\nProcessing error: '", trimws(tmpItem[[1]]), "'\nThank you ",
                "for reporting it at https://github.com/rfhb/ctrdata/issues")
          message("")
          stop("No data could be extracted for '", paste0(item, collapse = "', '"), "'.",
               "\nUse dbGetFieldsIntoDf(..., stopifnodata = FALSE) to ignore the error.",
               "\nUse dbFindFields() to find fields that exist in the collection.",
               call. = FALSE)
        } else {
          message("* no data or extraction error *")
          # create empty data set
          dfi <- cbind(dft, NA)
          names(dfi) <- c("_id", fields[i])
        } # stopifnodata
      } # if

      # add to result unless item was
      # previously specified in fields
      if (i > 1L) {
        dna <- names(dfi)
        dni <- intersect(dna, accumNames)
        dnd <- setdiff(dna, accumNames)
        if (length(dni)) {
          message("(not included again: ",
                  paste0(dni, collapse = ", "), ") ",
                  appendLF = FALSE)
          dfi <- dfi[, dnd, drop = FALSE]
        }
      }
      accumNames <<- c(accumNames, names(dfi)[-1])
      dfi

    }) # end lapply

  # bring result lists into data frame, by record _id
  result <- Reduce(function(...) merge(..., all = TRUE, by = "_id"), result)

  # prune rows without _id
  result <- result[!is.na(result[["_id"]]), , drop = FALSE]

  # remove rows with only NAs; try because
  # is.na may fail for complex cells
  onlyNas <- try({apply(result[, -1, drop = FALSE], 1,
                        function(r) all(is.na(r)))}, silent = TRUE)
  if (!inherits(onlyNas, "try-error") && any(onlyNas)) {
    result <- result[!onlyNas, , drop = FALSE]
  }

  # inform user
  if (is.null(result) || !nrow(result)) {
    warning("No records with values for any specified field. ",
            call. = FALSE)
    return(NULL)
  }

  # remove row names
  row.names(result) <- NULL

  # sort, add meta data
  result <- addMetaData(
    result[order(result[["_id"]]), , drop = FALSE],
    con = con)

  # return
  if (any("dplyr" == .packages())) return(dplyr::as_tibble(result))
  return(result)

}
# end dbGetFieldsIntoDf


#' Get value for variable of interest
#'
#' Get information of interest (e.g., endpoint)
#' from long data frame of protocol- or result-related
#' trial information as returned by \link{dfTrials2Long}.
#' Parameters `valuename`, `wherename` and `wherevalue` are
#' matched using Perl regular expressions and ignoring case.
#'
#' @param df A data frame (or tibble) with four columns (`_id`,
#'  `identifier`, `name`, `value`) as returned by
#'  \link{dfTrials2Long}
#'
#' @param valuename A character string for the name of the field
#' that holds the value of the variable of interest
#' (e.g., a summary measure such as "endPoints.*tendencyValue.value")
#'
#' @param wherename (optional) A character string to identify the
#' variable of interest among those that repeatedly occur in a
#' trial record (e.g., "endPoints.endPoint.title")
#'
#' @param wherevalue (optional) A character string with the value of
#' the variable identified by `wherename` (e.g., "response")
#'
#' @return A data frame (or tibble, if \code{dplyr} is loaded)
#' that includes the values of interest, with columns
#' `_id`, `identifier`, `name`, `value` (and `where`, with the
#'  contents of `wherevalue` found at `wherename`).
#'  Contents of `value` are strings unless all its elements
#'  are numbers. The `identifier` is generated by function
#'  \link{dfTrials2Long} to identify matching elements, e.g.
#'  endpoint descriptions and measurements.
#'
#' @importFrom dplyr as_tibble
#' @importFrom stringi stri_detect_regex
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' dfwide <- dbGetFieldsIntoDf(
#'  fields = c(
#'   ## ctgov - typical results fields
#'   # "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
#'   # "clinical_results.baseline.group_list.group",
#'   # "clinical_results.baseline.analyzed_list.analyzed.units",
#'   "clinical_results.outcome_list.outcome",
#'   "study_design_info.allocation",
#'   ## euctr - typical results fields
#'   # "trialInformation.fullTitle",
#'   # "baselineCharacteristics.baselineReportingGroups.baselineReportingGroup",
#'   # "trialChanges.hasGlobalInterruptions",
#'   # "subjectAnalysisSets",
#'   # "adverseEvents.seriousAdverseEvents.seriousAdverseEvent",
#'   "endPoints.endPoint",
#'   "subjectDisposition.recruitmentDetails"
#'   ), con = dbc)
#'
#' dflong <- dfTrials2Long(df = dfwide)
#'
#' ## get values for the endpoint 'response'
#' dfName2Value(
#'   df = dflong,
#'   valuename = paste0(
#'     "clinical_results.*measurement.value|",
#'     "clinical_results.*outcome.measure.units|",
#'     "endPoints.endPoint.*tendencyValue.value|",
#'     "endPoints.endPoint.unit"
#'   ),
#'   wherename = paste0(
#'     "clinical_results.*outcome.measure.title|",
#'     "endPoints.endPoint.title"),
#'   wherevalue = "response")
#'
dfName2Value <- function(df, valuename = "",
                         wherename = "", wherevalue = "") {

  # check parameters
  if (valuename == "") {
    stop("'valuename' must be specified.",
         call. = FALSE)
  }
  if (!identical(names(df),
                 c("_id", "identifier", "name", "value"))) {
    stop("'df' does not seem to come from dfTrials2Long()",
         call. = FALSE)
  }

  # indices of valuename
  indexVnames <- which(grepl(valuename, df[["name"]],
                             perl = TRUE, ignore.case = TRUE))
  if (!length(indexVnames)) stop("No rows found for 'valuename' = ", valuename)

  # if no where... are specified, just
  # return rows where name corresponds
  # to valuename
  if (wherename == "" & wherevalue == "") {

    # get relevant rows
    out <- df[indexVnames, , drop = FALSE]

  } else {# if where... are specified, continue

    # get where... indices per trial
    indexRows <- which(
      grepl(wherename, df[["name"]], perl = TRUE, ignore.case = TRUE) &
      grepl(wherevalue, df[["value"]], perl = TRUE, ignore.case = TRUE))
    if (!length(indexRows)) stop("No rows found for 'wherename' and 'wherevalue'")

    # get trial ids and identifiers for where...
    indexCases <- df[indexRows, c("_id", "identifier", "value"), drop = FALSE]
    # for merging column with wherevalue information
    names(indexCases) <- c("_id", "identifier", "where")

    # get output iterate over trials
    out <- list(nrow(indexCases))
    out <- apply(
      indexCases, 1,
      function(i) {
        ids <- intersect(
          # trial id
          which(i[["_id"]] == df[["_id"]]),
          # indices of sought valuename
          indexVnames
        )
        if (length(ids)) {
          ids <- ids[
            # identifier to match starting from left and
            # do not match e.g. 22 for identifier 2
            stringi::stri_detect_regex(
              str = df[ids, "identifier", drop = TRUE],
              pattern = paste0("^", i[["identifier"]], "([.]|$)")
            )]
        }
        # return value
        if (length(ids)) {
          merge(
            # select rows from input data frame
            x = df[ids, , drop = FALSE],
            # add column with wherevalue
            y = indexCases[
              indexCases[["_id"]] == i[["_id"]] &
                indexCases[["identifier"]] == i[["identifier"]],
              c("_id", "where"), drop = FALSE],
            by = "_id"
          )
        }
      }
    )

    # bind into data frame
    out <- do.call(
      rbind,
      c(out, stringsAsFactors = FALSE, make.row.names = FALSE))

  } # if where...

  # value column is character
  # try to convert it to numeric
  tmp <- suppressWarnings(
    as.numeric(out[["value"]])
  )
  # use if converted ok
  if (all(is.na(tmp) == is.na(out[["value"]]))) {
    out["value"] <- tmp
  }
  # remove any duplicates such as
  # from duplicate where... criteria
  out <- unique(out)
  row.names(out) <- NULL

  # inform user
  message("Returning values for ", length(unique(out[["_id"]])),
          " out of ", length(unique(df[["_id"]])), " trials")

  # return
  if (any("dplyr" == .packages())) return(dplyr::as_tibble(out))
  return(out)

} # end dfName2Value


#' Convert data frame with trial records into long format
#'
#' The function works with procotol- and results- related information.
#' It converts lists and other values that are in a data frame returned
#' by \link{dbGetFieldsIntoDf} into individual rows of a long data frame.
#' From the resulting data frame, values of interest can be selected
#' using \link{dfName2Value}.
#' The function is intended for fields with complex content, such as node
#' field "\code{clinical_results}" from EUCTR, which \link{dbGetFieldsIntoDf}
#' returns as a multiply nested list and for which this function then
#' converts every observation of every (leaf) field into a row of its own.
#'
#' @param df Data frame (or tibble) with columns including
#'  the trial identifier (\code{_id}) and
#'  one or more variables as obtained from
#'  \link{dbGetFieldsIntoDf}
#'
#' @return A data frame  (or tibble, if \code{dplyr} is loaded)
#' with the four columns: `_id`, `identifier`, `name`, `value`
#'
#' @importFrom stringi stri_extract_all_charclass stri_extract_first stri_replace_first
#' @importFrom dplyr as_tibble
#' @importFrom xml2 xml_text read_html
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' dfwide <- dbGetFieldsIntoDf(
#'   fields = "clinical_results.participant_flow",
#'   con = dbc)
#'
#' dfTrials2Long(df = dfwide)
#'
dfTrials2Long <- function(df) {

  # get names
  dfn <- names(df)

  # check parameters
  if (!any("_id" == dfn) ||
      ncol(df) == 1L) stop(
        "Missing _id column or other variables in 'df'",
        call. = FALSE
      )
  if (any(c("identifier", "name", "value") %in% dfn)) stop(
    "Unexpected columns; 'df' should not come from dfTrials2Long",
    call. = FALSE
  )

  # make _id the first column
  if (dfn[1] != "_id") {
    dfn <- c("_id", dfn[dfn != "_id"])
    df <- df[, dfn, drop = FALSE]
  }

  # helper function
  flattenDf <- function(x) {
    while (any(vapply(x, is.list, logical(1L)))) {
      x <- lapply(x, function(x) if (is.list(x)) x else list(x))
      x <- unlist(x, recursive = FALSE, use.names = TRUE)
    }
    return(x)
  }

  # columns that are not compatible with the
  # later operations are converted to character
  conv <- sapply(df, class) == "Date"
  conv <- seq_len(ncol(df))[conv]
  for (c in conv) df[, c] <- as.character(df[, c, drop = TRUE])

  # iterative unnesting, by column
  out <- lapply(
    seq_len(ncol(df))[-1],
    function(cc) {
      # get item
      ci <- df[[cc]]
      # get item name
      tn <- dfn[cc]
      # inform user
      message(tn, rep(" ", 200L - nchar(tn)), "\r", appendLF = FALSE)
      # handle case when column is data frame, turn into list by row
      if (is.data.frame(ci)) ci <- split(ci, seq_len(nrow(ci)))
      # and by cell in column
      lapply(ci, function(c) {
        if (is.data.frame(c)) {
          # unlist is numbering repeat item names
          x <- unlist(flattenDf(c))
          if (!is.null(names(x))) tn <- paste0(tn, ".", names(x))
          if (is.null(x)) x <- NA
          # compose
          data.frame(
            "name" = tn,
            "value" = x,
            check.names = FALSE,
            stringsAsFactors = FALSE,
            row.names = NULL)
        } else {
          # initialise
          xx <- NULL
          tnn <- NULL
          # need to iterate since there may be repeats, e.g, 1, 2, 3.1, 3.2
          sapply(seq_len(length(c)), function(i) {
            # unlist is numbering repeat item names
            x <- unlist(flattenDf(c[i]))
            if (!is.null(names(x))) {
              # any first numeric identifier?
              tst <- stringi::stri_extract_first_regex(names(x), "[0-9]+")
              if (all(!is.na(tst))) {
                # if yes bring into middle
                tn <- paste0(
                  tn, ".", tst, ".",
                  stringi::stri_replace_first_regex(names(x), "[0-9]+", ""))
              } else {
                # if no add using i
                tn <- paste0(tn, ".", i, ".", names(x))
              }
            }
            if (is.null(x)) x <- NA
            xx <<- c(xx, x)
            tnn <<- c(tnn, tn)
          }, USE.NAMES = FALSE)
          # compose
          data.frame(
            "name" = tnn,
            "value" = xx,
            check.names = FALSE,
            stringsAsFactors = FALSE,
            row.names = NULL)
        } # if is.data.frame
      })})
  message(rep(" ", 200L), "\r", appendLF = FALSE)

  # add _id to list elements and
  # simplify into data frames
  out <- lapply(
    out, function(e) {
      message(". ", appendLF = FALSE)
      names(e) <- df[["_id"]]
      do.call(rbind, c(e, stringsAsFactors = FALSE))
    })
  out <- do.call(rbind, c(out, stringsAsFactors = FALSE))
  message(". ", appendLF = FALSE)

  # remove rows where value is NA
  out <- out[!is.na(out[["value"]]), , drop = FALSE]

  # convert html entities
  htmlEnt <- grepl("&[#a-zA-Z]+;", out[["value"]])
  if (any(htmlEnt)) out[["value"]][htmlEnt] <-
    sapply(out[["value"]][htmlEnt], function(i)
      xml2::xml_text(xml2::read_html(charToRaw(i))), USE.NAMES = FALSE)
  message(". ", appendLF = FALSE)

  # generate new data frame with target columns and order
  out <- data.frame(
    # process row.names to obtain trial id
    "_id" = stringi::stri_extract_first(
      str = row.names(out),
      regex = c(paste0(regCtgov, "|", regIsrctn, "|",
                       regEuctr, "-[3]?[A-Z]{2}|", regCtis))),
    "identifier" = NA,
    "name" = out[["name"]],
    "value" = out[["value"]],
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE)
  message(". ", appendLF = FALSE)

  # name can include from 0 to about 6 number groups, get all
  # and concatenate to oid-like string such as "1.2.3.4.5.6",
  # e.g. "9.8.2" which should be extracted from the this name
  # clinical...class9.analyzed...count8.@attributes.value2
  #
  # except where name is exactly one of dfn
  onlyHere <- vapply(out[["name"]], function(i) !any(i == dfn),
                     logical(1L), USE.NAMES = FALSE)
  #
  out[["identifier"]][onlyHere] <- vapply(
    stringi::stri_extract_all_regex(out[["name"]][onlyHere], "[0-9]+([.]|$)"),
    function(i) paste0(gsub("[.]", "", i), collapse = "."), character(1L))
  # defaults
  out[["identifier"]] [out[["identifier"]] == "NA"] <- "0"
  out[["identifier"]] [is.na(out[["identifier"]])]  <- "0"
  message(". ", appendLF = FALSE)
  #
  # remove numbers from variable name
  out[["name"]][onlyHere] <- gsub(
    "[0-9]+([.])|[0-9]+$|[.]?@attributes", "\\1",
    out[["name"]][onlyHere], perl = TRUE)
  #
  # remove any double separators
  out[["name"]] <- gsub("[.][.]+", ".", out[["name"]], perl = TRUE)

  # remove double rows from duplicating e above
  out <- unique(out)

  # inform
  message("\nTotal ", nrow(out), " rows, ",
          length(unique(out[["name"]])),
          " unique names of variables")

  # output
  if (any("dplyr" == .packages())) return(dplyr::as_tibble(out))
  return(out)

} # end dfTrials2Long


#' Extract named element(s) from list(s) into long-format
#' data frame
#'
#' (Deprecated, use \link{dfTrials2Long} and \link{dfName2Value}!)
#' The function uses a name (key) to extract an element
#' from a list in a data.frame such as obtained with
#' \link{dbGetFieldsIntoDf}. This helps to simplify
#' working with nested lists and with complex structures.
#'
#' @param df A data frame (or tibble)
#'
#' @param list.key A list of pairs of list names and
#'  key names, where the list name corresponds to the
#'  name of a column in \code{df} that holds a list and
#'  the name of the key identifies the element to be
#'  extracted. See example.
#'
#' @return A data frame (or tibble, if \code{dplyr} is loaded)
#'  in long format with columns
#'  name (identifying the full path in the data frame,
#'  "<list>.<key>"), _id (of the trial record), value
#'  (of name per _id), item (number of value of name
#'  per _id).
#'
#' @importFrom dplyr as_tibble
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' df <- dbGetFieldsIntoDf(
#'   fields = c(
#'     "endPoints.endPoint",
#'     "subjectDisposition.postAssignmentPeriods"),
#'   con = dbc)
#'
#' suppressWarnings(
#'   dfListExtractKey(
#'     df = df,
#'     list.key = list(
#'         c("endPoints.endPoint",
#'           "^title"),
#'         c("subjectDisposition.postAssignmentPeriods",
#'           "arms.arm.type.value"))
#' ))
#'
dfListExtractKey <- function(
  df,
  list.key =
    list(c("endPoints.endPoint", "^title")
    )) {

  # deprecate
  .Deprecated(new = "dfName2Value")

  # check
  if (!any("_id" == names(df))) {
    stop("Data frame 'df' lacks '_id' column.",
         call. = FALSE)
  }

  # helper function to extract from
  # a named vector elements by name
  extractKey <- function(flattenedList, key) {

    # find element by key
    selected <- grepl(key,
                      names(flattenedList),
                      ignore.case = TRUE)


    # extract value for key
    extracted <- flattenedList[selected]

    # if key is not found, return a value
    # e.g. missing value (NA) or empty string ("")
    # please change as wanted for later processing
    if (length(extracted) == 0) extracted <- NA

    # return
    return(extracted)
  }

  # dots needs to be defined because passing
  # it in .Internal(mapply()) is not enough
  out <- lapply(
    list.key,
    function(k)
      lapply(df[[k[1]]],
             # k[1] = "endPoints.endPoint" identifies
             # the column in data frame with the list
             function(l) extractKey(
               unlist(l, recursive = TRUE, use.names = TRUE), k[2])
             # k[2] = "^title" identifies the key in the sublist
      ))

  out <- sapply(seq_along(list.key), function(li) {

    tmp <- out[[li]]

    tmp <- sapply(

      seq_along(tmp),
      function(ii) {

        data.frame(
          name = gsub("[-0-9]*$", "", # trailing number
                      gsub("[^a-zA-Z0-9_.-]", "",
                           paste0(list.key[[li]], collapse = "."))),
          "_id" = df[["_id"]][[ii]],
          value = tmp[[ii]],
          item = seq_along(tmp[[ii]]),
          row.names = NULL,
          stringsAsFactors = FALSE,
          check.names = FALSE)
      }, simplify = FALSE)

    do.call(rbind, tmp)

  }, simplify = FALSE)

  # result
  out <- do.call(rbind, c(out, stringsAsFactors = FALSE, make.row.names = FALSE))

  # return
  if (any("dplyr" == .packages())) return(dplyr::as_tibble(out))
  return(out)

} # end dfListExtractKey


#' Merge two variables
#'
#' Merge two variables in a data frame such as returned by \link{dbGetFieldsIntoDf}
#' into a new variable, and optionally also map its values to new levels.
#'
#' @param df A \link{data.frame} in which there are two variables (columns)
#' to be merged into one.
#'
#' @param colnames A vector of length two with names of the two columns
#' that hold the variables to be merged. See \link{colnames} for how to
#' obtain the names of columns of a data frame.
#'
#' @param levelslist A list with one slice each for a new value to be
#' used for a vector of old values (optional).
#'
#' @param ... for deprecated \code{varnames} parameter (will be removed)
#'
#' @return A vector of strings
#'
#' @importFrom dplyr as_tibble
#'
#' @export
#'
#' @examples
#'
#' vars2merge <- c("overall_status", "x5_trial_status")
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials")
#'
#' df <- dbGetFieldsIntoDf(
#'   fields = vars2merge,
#'   con = dbc)
#'
#' statusvalues <- list(
#'   "ongoing" = c("Recruiting", "Active", "Ongoing",
#'                 "Active, not recruiting", "Enrolling by invitation"),
#'   "completed" = c("Completed", "Prematurely Ended", "Terminated"),
#'   "other" = c("Withdrawn", "Suspended",
#'               "No longer available", "Not yet recruiting"))
#'
#' dfMergeTwoVariablesRelevel(
#'   df = df,
#'   colnames = vars2merge,
#'   levelslist = statusvalues)
#'
dfMergeTwoVariablesRelevel <- function(
  df = NULL,
  colnames = "",
  levelslist = NULL,
  ...) {

  # check parameters

  # FIXME migrate from previously
  # used parameter "varnames"
  tmp <- match.call()
  tmp <- tmp["varnames"]
  tmp <- as.list(tmp)[[1]]
  if (length(tmp) == 3 && colnames == "") {
    colnames <- unlist(as.list(tmp[-1], use.names = FALSE))
    warning("Parameter varnames is deprecated, use colnames instead.",
            call. = FALSE)
  }

  # other checks
  if (!inherits(df, "data.frame")) {
    stop("Need a data frame as input.", call. = FALSE)
  }
  if (length(colnames) != 2) {
    stop("Please provide exactly two column names.", call. = FALSE)
  }
  # change to data frame because
  # variable classes are compared
  df <- as.data.frame(df)

  # find variables in data frame and merge
  tmp <- match(colnames, names(df))
  df <- df[, tmp, drop = FALSE]

  # bind as ...
  if (class(df[[1]]) == class(df[[2]]) &&
      !all(class(df[[1]]) %in% "character")) {
    # check
    if (nrow(na.omit(df[!vapply(df[[1]], is.null, logical(1L)) &
                        !vapply(df[[2]], is.null, logical(1L)), ,
                        drop = FALSE]))) {
      warning("Some rows had non-character values for both columns, used first",
              noBreaks. = TRUE, immediate. = TRUE)
    }
    # values, with first having
    # priority over the second
    tmp <- ifelse(is.na(tt <- df[[1]]), df[[2]], df[[1]])
  } else {
    catind <- which((!is.na(df[[1]]) & df[[1]] != "") &
                    (!is.na(df[[2]]) & df[[2]] != ""))
    # check
    if (length(catind)) {warning(
      "Some rows had character values for both columns, concatenated",
      noBreaks. = TRUE, immediate. = TRUE)
    }
    # strings, concatenated
    tmp <- rep_len("", length.out = nrow(df))
    tmp[catind] <- " / "
    tmp <- paste0(
      ifelse(is.na(tt <- as.character(df[[1]])), "", tt), tmp,
      ifelse(is.na(tt <- as.character(df[[2]])), "", tt))
  }

  # type where possible
  if (class(df[[1]]) == class(df[[2]]) &&
      !all(class(df[[1]]) %in% "character")) {
    mode(tmp) <- mode(df[[1]])
    class(tmp) <- class(df[[1]])
  }

  # relevel if specified
  if (!is.null(levelslist)) {

    # check
    if (!all(class(levelslist) %in% "list")) {
      stop("Need list for parameter 'levelslist'.", call. = FALSE)
    }

    # helper function to collapse factor levels into the first
    refactor <- function(x, collapselevels, levelgroupname) {
      levels(x) [match(collapselevels, levels(x))] <- levelgroupname
      return(x)
    }

    # convert result to factor as this is needed for helper function
    tmp <- as.factor(tmp)

    # apply helperfunction to elements of the list
    for (i in seq_len(length(levelslist))) {
      tmp <- refactor(tmp, unlist(levelslist[i], use.names = FALSE),
                      attr(levelslist[i], "names"))
    }

    # convert factor back into string vector
    tmp <- as.character(tmp)

  }

  # check and inform user
  if (length(tt <- unique(tmp)) > 3L) {
    message("Unique values returned (first three): ",
            paste(tt[1L:3L], collapse = ", "))
  } else {
    message("Unique values returned: ",
            paste(tt, collapse = ", "))
  }

  # return
  return(tmp)
}
# end dfMergeTwoVariablesRelevel


#' Select a single trial record from records of different EU Member States
#'
#' The EUCTR provides one record per trial per EU Member State in which the
#' trial is conducted. For all trials conducted in more than one Member State,
#' this function returns only one record per trial.
#'
#' Note: To deduplicate trials from different registers (EUCTR and CTGOV),
#' please first use function \link{dbFindIdsUniqueTrials}.
#'
#' @param df A data frame created from the database collection that includes
#'   the columns "_id" and "a2_eudract_number", for example created with
#'   function dbGetFieldsIntoDf(c("_id", "a2_eudract_number")).
#'
#' @param prefermemberstate Code of single EU Member State for which records
#' should returned. If not available, a record for DE or lacking this, any
#' random Member State's record for the trial will be returned.
#' For a list of codes of EU  Member States, please see vector
#' \code{countriesEUCTR}. Specifying "3RD" will return the Third Country
#' record of trials, where available.
#'
#' @param include3rdcountrytrials A logical value if trials should be retained
#' that are conducted exclusively in third countries, that is, outside
#' the European Union. Ignored if \code{prefermemberstate} is set to "3RD".
#'
#' @return A data frame as subset of \code{df} corresponding to the sought
#'   records.
#'
#' @keywords internal
#
dfFindUniqueEuctrRecord <- function(
  df = NULL,
  prefermemberstate = "DE",
  include3rdcountrytrials = TRUE) {

  # check parameters
  if (!any(class(df) %in% "data.frame")) {
    stop("Parameter df is not a data frame.", call. = FALSE)
  }
  #
  if (is.null(df[["_id"]]) ||
      is.null(df[["a2_eudract_number"]])) {
    stop('Data frame does not include "_id"',
         ' and "a2_eudract_number" columns.',
         call. = FALSE)
  }
  #
  if (nrow(df) == 0) {
    stop("Data frame does not contain records (0 rows).",
         call. = FALSE)
  }
  #
  if (!(prefermemberstate %in% countriesEUCTR)) {
    stop("Value specified for prefermemberstate does not match",
         " one of the recognised codes: ",
         paste(sort(countriesEUCTR), collapse = ", "),
         call. = FALSE)
  }

  # notify it mismatching parameters
  if (prefermemberstate == "3RD" & !include3rdcountrytrials) {
    warning("Preferred EUCTR version set to 3RD country trials, but ",
            "'include3rdcountrytrials' was FALSE, setting it to TRUE.",
            call. = FALSE,
            noBreaks. = FALSE,
            immediate. = FALSE)
    include3rdcountrytrials <- TRUE
  }

  # count total
  totalEuctr <- unique(df[["a2_eudract_number"]])
  totalEuctr <- na.omit(totalEuctr[totalEuctr != ""])
  totalEuctr <- length(totalEuctr)

  # as a first step, handle 3rd country trials e.g. 2010-022945-52-3RD
  # if retained, these trials would count as record for a trial
  if (!include3rdcountrytrials) {
    df <- df[!grepl("-3RD", df[["_id"]]), , drop = FALSE]
  }

  # count number of records by eudract number
  tbl <- table(df[["_id"]], df[["a2_eudract_number"]])
  tbl <- as.matrix(tbl)
  # nms has names of all records
  nms <- dimnames(tbl)[[1]]

  # nrs has eudract numbers for which is there more than 1 record
  nrs <- colSums(tbl)
  nrs <- nrs[nrs > 1]
  nrs <- names(nrs)

  # nst is a list of nrs trials of a logical vector along nms
  # that indicates if the indexed record belongs to the trial
  nms2 <- substr(nms, 1, 14)
  nst <- lapply(nrs, function(x) nms2 %in% x)

  # helper function to find the Member State version
  removeMSversions <- function(indexofrecords) {
    # given a vector of records (nnnn-nnnnnnn-nn-MS) of a single trial, this
    # returns all those _ids of records that do not correspond to the preferred
    # Member State record, based on the user's choices and defaults.
    # Function uses prefermemberstate, nms from the caller environment
    recordnames <- nms[indexofrecords]
    #
    # fnd should be only a single string, may need to be checked
    if (sum(fnd <- grepl(prefermemberstate, recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    #
    if (sum(fnd <- grepl("DE", recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    #
    # default is to list all but first record
    # the listed records are the duplicates
    # 3RD country trials would be listed first
    # hence selected, which is not desirable
    # unless chosen as prefermemberstate
    return(rev(sort(recordnames))[-1])
  }

  # finds per trial the desired record;
  # uses prefermemberstate and nms
  result <- lapply(nst,
                   function(x) removeMSversions(x))
  result <- unlist(result, use.names = FALSE)

  # eleminate the unwanted EUCTR records
  df <- df[!(df[["_id"]] %in% result), , drop = FALSE]

  # also eliminate the meta-info record
  df <- df[!(df[["_id"]] == "meta-info"), , drop = FALSE]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result))) {
    message(
      " - ", tmp,
      " EUCTR _id were not preferred EU Member State record for ",
      totalEuctr, " trials")
  }

  # return
  return(df)

}
# end dfFindUniqueEuctrRecord


#' Change type of field based on name of field
#'
#' @param dv a vector of character strings
#'
#' @param fn a field name
#'
#' @return a typed vector, same length as dv
#'
#' @importFrom xml2 xml_text read_html
#' @importFrom lubridate duration
#'
#' @keywords internal
#' @noRd
#'
typeField <- function(dv, fn) {

  # early exit if dv is not character
  if (!is.atomic(dv)) return(dv)

  # early exit if dv is not character
  if (!all(class(dv) %in% "character")) return(dv)

  # clean up for all character vectors
  # - if NA as string, change to NA
  dv[grepl("^N/?A$|^ND$", dv)] <- NA
  # - remove explanatory text before date
  dv <- sub("^ClinicalTrials.gov processed this data on ", "", dv)
  # - give Month Year also a Day to work with as.Date
  dv <- sub("^([a-zA-Z]+) ([0-9]{4})$", "\\1 15, \\2", dv)
  # - convert html entities because these had to
  #   be left intact when converting to ndjson
  htmlEnt <- grepl("&[#a-zA-Z]+;", dv)
  if (any(htmlEnt)) dv[htmlEnt] <-
    sapply(dv[htmlEnt], function(i)
      xml2::xml_text(xml2::read_html(charToRaw(i))), USE.NAMES = FALSE)
  # - convert newline
  dv <- gsub("\r", "\n", dv)

  # early exit if fn is not in typeVars
  if (is.null(typeVars[[fn]])) return(dv)

  # for date time conversion
  lct <- Sys.getlocale("LC_TIME")

  # main typing functions
  ctrDate <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%Y-%m-%d")
  }
  #
  ctrDateUs <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%b %e, %Y")
  }
  #
  ctrDateCtr <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%Y-%m-%d %H:%M:%S")
  }
  #
  ctrDateTime <- function() {
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)
    as.Date(dv, format = "%Y-%m-%dT%H:%M:%S")
  }
  #
  ctrYesNo <- function() {
    vapply(dv, FUN = function(x)
      switch(x, "Yes" = TRUE, "No" = FALSE, NA),
      logical(1L), USE.NAMES = FALSE)
  }
  #
  ctrFalseTrue <- function() {
    vapply(dv, FUN = function(x)
      switch(x, "true" = TRUE, "false" = FALSE, NA),
      logical(1L), USE.NAMES = FALSE)
  }
  #
  ctrInt       <- function() {
    vapply(dv, FUN = function(x)
      as.integer(x = x), integer(1L),
      USE.NAMES = FALSE)
  }
  #
  ctrDifftime   <- function() {
    out <- sapply(dv, FUN = function(x) {
      if (is.na(x)) {NA} else {
        as.numeric(
          lubridate::duration(
            tolower(x)
          ), units = "days")
      }
    }, USE.NAMES = FALSE)
    as.difftime(out, units = "days")
  }
  #
  ctrDifftimeDays   <- function() {
    lubridate::ddays(x = as.numeric(dv))
  }
  #
  ctrDifftimeMonths   <- function() {
    lubridate::dmonths(x = as.numeric(dv))
  }
  #
  ctrDifftimeYears   <- function() {
    lubridate::dyears(x = as.numeric(dv))
  }

  ## apply typing
  ldv <- length(dv)
  if (any(grepl(" / ", dv))) {

    # if any concatenations, apply typing per concatenated
    # item and return list per item. note that dv has to be
    # overwritten in outer environment for typeVars to work
    out <- lapply(dv, function(r)  {
      dv <<- strsplit(r, " / ", fixed = TRUE)[[1]]
      try(do.call(typeVars[[fn]], list()), silent = TRUE)
    })

  } else {

    # apply typing function with its specified type
    out <- try(do.call(typeVars[[fn]], list()), silent = TRUE)

  }

  # error output
  if (any(sapply(out, function(r) inherits(r, "try-error"))) ||
      length(out) != ldv) {
    out <- rep.int(x = NA, times = ldv)
  }

  # return
  return(out)

} # end typeField


#' Annotate ctrdata function return values
#'
#' @param x object to be annotated
#'
#' @inheritParams ctrDb
#'
#' @keywords internal
#' @noRd
#'
addMetaData <- function(x, con) {

  # add metadata
  attr(x, "ctrdata-dbname")         <- con$db
  attr(x, "ctrdata-table")          <- con$collection
  attr(x, "ctrdata-dbqueryhistory") <- dbQueryHistory(
    con = con,
    verbose = FALSE)

  # return annotated object
  return(x)

} # end addMetaData


#' Install necessary helper apps (Windows only)
#'
#' Convenience function to install a minimal Cygwin environment under MS
#' Windows, including perl, sed and php.
#' Alternatively and in case of difficulties, download and run the cygwin
#' setup yourself as follows: \code{cygwinsetup.exe --no-admin --quiet-mode
#' --verbose --upgrade-also --root c:/cygwin --site
#' https://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/ --packages
#' perl,php-jsonc,php-simplexml}.
#' These binaries are required only for function \link{ctrLoadQueryIntoDb}
#' but not for any other function in this package.
#'
#' @export
#'
#' @param force Set to \code{TRUE} to update a Cygwin environment that
#'   was previously installed with the function, or to overwrite any existing
#'   installation in \code{c:\\cygwin}
#'
#' @param proxy Specify any proxy to be used for downloading via http, e.g.
#'   `host_or_ip:port`; defaults to the environment variable `https_proxy`.
#'   Set to `""` to not specify or to unset a proxy.
#'
#' @examples
#' \dontrun{
#'
#'
#' try(installCygwinWindowsDoInstall(), silent = TRUE)
#'
#' }
installCygwinWindowsDoInstall <- function(
  force = FALSE, proxy = Sys.getenv("https_proxy")) {

  # checks
  if (.Platform$OS.type != "windows") {
    stop("This function is only for MS Windows operating systems.",
         call. = FALSE)
  }
  #
  if (!force & dir.exists("c:\\cygwin")) {
    message("cygwin is already installed in c:\\cygwin. ",
            "To update or re-install, use force = TRUE.")
    # exit function after testing
    return(installCygwinWindowsTest(verbose = TRUE))
  }

  # create R session temporary directory
  tmpfile <- paste0(tempdir(), "/cygwin_inst")
  dir.create(tmpfile)
  dstfile <- paste0(tmpfile, "/cygwinsetup.exe")

  # generate download url
  tmpurl <- ifelse(
    grepl("x64", utils::win.version()),
    "setup-x86_64.exe",
    "setup-x86.exe")
  tmpurl <- paste0("https://cygwin.org/", tmpurl)

  # inform user
  message("Attempting download of ", tmpurl, " ...")

  # download.file uses the proxy configured in the system
  tmpdl <- try({
    utils::download.file(
      url = tmpurl,
      destfile = dstfile,
      quiet = FALSE,
      mode = "wb")
  }, silent = TRUE)

  # compose setup command
  setupcmd <- paste0(
    dstfile,
    " --no-admin --quiet-mode --upgrade-also --no-shortcuts --root c:/cygwin",
    " --site https://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/",
    " --packages perl,php-simplexml,php-json ",
    " --local-package-dir ", tmpfile,
    ifelse(nchar(proxy), " --proxy ", ""), proxy
  )

  # check
  if (!file.exists(dstfile) ||
      file.size(dstfile) < (5 * 10 ^ 5) ||
      (inherits(tmpdl, "try-error"))) {
    stop("Failed, please download manually and install with:\n",
         tmpurl, " ", setupcmd, call. = FALSE)
  }

  # execute cygwin setup command
  message("Executing: ", setupcmd)
  system(setupcmd)

  # return cygwin installation test
  return(installCygwinWindowsTest(verbose = TRUE))

}
# end installCygwinWindowsDoInstall


#' Convenience function to test for working cygwin installation
#'
#' @param verbose If \code{TRUE}, prints confirmatory
#'  message (default \code{FALSE})
#'
#' @return Information if Cygwin can be used, \code{TRUE}
#'  or \code{FALSE}, or NULL if not under MS Windows
#'
#' @keywords internal
#' @noRd
#
installCygwinWindowsTest <- function(verbose = FALSE) {
  #
  if (.Platform$OS.type != "windows") {
    message("Function installCygwinWindowsTest() is ",
            "only for MS Windows operating systems.")
    return(invisible(NULL))
  }
  #
  if (checkBinary()) {
    if (verbose) message("cygwin seems to work correctly")
    return(invisible(TRUE))
  } else {
    stop(
      "cygwin is not available, ctrLoadQueryIntoDb() will not work. ",
      "Consider calling ctrdata::installCygwinWindowsDoInstall()",
      call. = FALSE)
  }
}
# end installCygwinWindowsTest


#' Check availability of binaries installed locally
#'
#' @param commandtest Command to be used for testing
#' the availability of the binary, e.g. "php -v".
#' Note internal quotes need to be escaped, e.g.
#' \code{installFindBinary('php -r
#' \"simplexml_load_string(\'\');\"')}.
#' See R/onload.R for tested binaries.
#'
#' @param verbose Set to \code{TRUE} to see printed
#' return value of \code{commandtest}
#'
#' @return Logical indicating if executing `commandtest`
#' returned an error or not
#'
#' @keywords internal
#' @noRd
#
checkCommand <- function(commandtest = NULL, verbose = FALSE) {

  # check
  if (is.null(commandtest)) {
    stop("Empty argument: commandtest",
         call. = FALSE)
  }

  # only for windows, add cygwin shell
  if (.Platform$OS.type == "windows") {
    commandtest <- paste0(
      rev(Sys.glob("c:\\cygw*\\bin\\bash.exe"))[1],
      ' --noprofile --norc --noediting -c ',
      shQuote(paste0(
        "PATH=/usr/local/bin:/usr/bin; ",
        commandtest)))
  }
  if (verbose) message(commandtest)

  # test command
  commandresult <- try(
    suppressWarnings(
      system(commandtest,
             intern = TRUE,
             ignore.stderr =
               ifelse(.Platform$OS.type == "windows",
                      FALSE, TRUE))),
    silent = TRUE
  )

  # evaluate command
  commandreturn <- ifelse(
    inherits(commandresult, "try-error") ||
      grepl("error|not found", tolower(paste(commandresult, collapse = " "))) ||
      (!is.null(attr(commandresult, "status")) &&
         (attr(commandresult, "status") != 0)),
    FALSE, TRUE)

  # user info
  if (commandreturn && interactive()) message(". ", appendLF = FALSE)
  if (verbose) print(commandresult)

  # return
  return(commandreturn)
}
# end checkCommand


#' checkBinary
#'
#' @param b Vector of pre-defined binaries to be tested
#'
#' @param verbose Set to \code{TRUE} for more information
#'
#' @keywords internal
#' @noRd
#'
#' @return Logical, \code{TRUE} if all binaries ok
#'
checkBinary <- function(b = NULL, verbose = FALSE) {

  # check actions and user infos
  actionsInfos <- list(
    "notworking" = c("nonexistingbinarytested",
                     "nonexistingbinarytested not found"),
    "php" = c("php --version",
              "php not found, ctrLoadQueryIntoDb() will not work "),
    "phpxml" = c("php -r 'simplexml_load_string(\"<br />\");'",
                 "php xml not found, ctrLoadQueryIntoDb() will not work "),
    "phpjson" = c("php -r 'json_encode(\"<foo>\");'",
                  "php json not found, ctrLoadQueryIntoDb() will not work "),
    "sed" = c("echo x | sed s/x/y/",
              "sed not found, ctrLoadQueryIntoDb() will not work "),
    "perl" = c("perl -V:osname",
               "perl not found, ctrLoadQueryIntoDb() will not work ")
  )

  # if input empty, just check all except test
  if (is.null(b)) b <- names(actionsInfos)[-1]

  # do check
  out <- sapply(X = b, function(bi) {

    # check input
    actionsInfo <- actionsInfos[[bi]]
    if (is.null(actionsInfo)) stop("Unknown binary to check: ", bi, call. = FALSE)

    # previously checked and successful?
    checked <- ctrCache(xname = paste0("bin_check_", bi))
    if (!is.null(checked) && checked) return(TRUE)

    # continue to check binary
    ok <- checkCommand(commandtest = actionsInfo[1], verbose = verbose)
    if (!ok) message("\n", actionsInfo[2], appendLF = FALSE)

    # store check to private environment only if successful
    if (ok) ctrCache(xname = paste0("bin_check_", bi), xvalue = ok)

    # return
    return(ok)

  })

  # inform user
  if (!all(out)) message(
    "\nTo install command line binaries needed for the function ",
    "ctrLoadQueryIntoDb() of package ctrdata, see recommendations at ",
    "https://github.com/rfhb/ctrdata#",
    "2-command-line-tools-perl-sed-and-php-52-or-higher",
    "\nAfter installation, detach and load package ctrdata again, ",
    "or restart the R session.\n")

  # return single value since
  # all tests need to be ok
  invisible(all(out))

}


#' ctrMultiDownload
#'
#' @param urls Vector of urls to be downloaded
#'
#' @param progress Set to \code{FALSE} to not print progress bar
#'
#' @keywords internal
#' @noRd
#'
#' @return Data frame with columns such as status_code etc
#'
#' @importFrom curl multi_download
#' @importFrom utils URLencode
#'
ctrMultiDownload <- function(urls, destfiles, progress = TRUE) {

  downloadValue <- do.call(
    curl::multi_download,
    c(urls = list(utils::URLencode(urls)),
      destfiles = list(destfiles),
      progress = progress,
      timeout = Inf,
      getOption("httr_config")[["options"]],
      accept_encoding = "gzip,deflate,zstd,br"
    )
  )
  if (inherits(downloadValue, "try-error")) {
    stop("Download failed; last error: ", class(downloadValue), call. = FALSE)
  }

  return(downloadValue)
