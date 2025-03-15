### ctrdata package

#' Get register name and query parameters from search URL
#'
#' Extracts query parameters and register name from parameter `url` or
#' from the clipboard, into which the URL of a register search was copied.
#'
#' @param url URL such as from the browser address bar.
#' If not specified, clipboard contents will be checked for
#' a suitable URL.
#' For automatically copying the user's query of a register
#' in a web browser to the clipboard, see
#' \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}.
#' Can also contain a query term such as from
#' \link{dbQueryHistory}()["query-term"].
#' Can also be an identifier of a trial, which based on its
#' format will indicate to which register it relates.
#'
#' @param register Optional name of register (one of "EUCTR", "CTGOV2"
#' "ISRCTN" or "CTIS") in case `url` is a query term but not a full URL
#'
#' @export
#'
#' @return A data frame (or tibble, if \code{tibble} is loaded)
#' with column names `query-term` and `query-register`.
#' The data frame (or tibble) can be passed as such as parameter
#' `queryterm` to \link{ctrLoadQueryIntoDb} and as parameter
#' `url` to \link{ctrOpenSearchPagesInBrowser}.
#'
#' @importFrom clipr read_clip
#'
#' @examples
#'
#' # user copied into the clipboard the URL from
#' # the address bar of the browser that shows results
#' # from a query in one of the trial registers
#' if (interactive()) try(ctrGetQueryUrl(), silent = TRUE)
#'
#' # extract query parameters from search result URL
#' # (URL was cut for the purpose of formatting only)
#' ctrGetQueryUrl(
#'     url = paste0(
#'         "https://classic.clinicaltrials.gov/ct2/results?",
#'         "cond=&term=AREA%5BMaximumAge%5D+RANGE%5B0+days%2C+28+days%5D",
#'         "&type=Intr&rslt=&age_v=&gndr=&intr=Drugs%2C+Investigational",
#'         "&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=",
#'         "&locn=&phase=2&rsub=&strd_s=01%2F01%2F2015&strd_e=01%2F01%2F2016",
#'         "&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&rfpd_s=&rfpd_e=&lupd_s=&lupd_e=&sort="
#'     )
#' )
#'
#' # other examples
#' ctrGetQueryUrl("https://www.clinicaltrialsregister.eu/ctr-search/trial/2007-000371-42/results")
#' ctrGetQueryUrl("https://euclinicaltrials.eu/ctis-public/view/2022-500041-24-00")
#' ctrGetQueryUrl("https://classic.clinicaltrials.gov/ct2/show/NCT01492673?cond=neuroblastoma")
#' ctrGetQueryUrl("https://clinicaltrials.gov/ct2/show/NCT01492673?cond=neuroblastoma")
#' ctrGetQueryUrl("https://clinicaltrials.gov/study/NCT01467986?aggFilters=ages:child")
#' ctrGetQueryUrl("https://www.isrctn.com/ISRCTN70039829")
#'
#' # using identifiers of single trials
#' ctrGetQueryUrl("70039829")
#' ctrGetQueryUrl("ISRCTN70039829")
#' ctrGetQueryUrl("NCT00617929")
#' ctrGetQueryUrl("2022-501142-30-00")
#' ctrGetQueryUrl("2012-003632-23")
#'
ctrGetQueryUrl <- function(
    url = "",
    register = "") {

  # check parameters expectations
  if (!is.atomic(url) || !is.atomic(register) ||
      is.null(url) || is.null(register) ||
      !inherits(url, "character") || !inherits(register, "character") ||
      length(url) != 1L || length(register) != 1L ||
      is.na(url) || is.na(register)) {
    stop("ctrGetQueryUrl(): 'url' and / or 'register' ",
         "is not a single character string, url: '",
         url, "', register: '", register, "'",
         call. = FALSE
    )
  }

  # if no parameter specified,
  # check clipboard contents
  if (nchar(url) == 0L && register != "CTIS") {
    url <- try(
      suppressWarnings(
        clipr::read_clip(
          allow_non_interactive = TRUE
        )
      ),
      silent = TRUE
    )
    if (inherits(url, "try-error")) url <- ""
    if (is.null(url) || (length(url) != 1L) || (nchar(url) == 0L) ||
        !startsWith(url, "https://")) {
      stop("ctrGetQueryUrl(): no clinical trial register ",
           "search URL found in parameter 'url' or in clipboard.",
           call. = FALSE
      )
    }
    message("* Using clipboard content as register query URL: ", url)
  }

  # check parameter combination
  if (register != "" && startsWith(url, "http")) {
    warning("Full URL but also 'register' specified; ",
            "continuing with register = ''",
            immediate. = TRUE
    )
    register <- ""
  }

  ### identify register ####

  # identify domain and register short name
  registerFromUrl <- switch(
    sub("^https://([a-zA-Z.]+?)/.*", "\\1", url),
    "classic.clinicaltrials.gov" = "CTGOV",
    "www.clinicaltrials.gov" = "CTGOV2",
    "clinicaltrials.gov" = "CTGOV2",
    "euclinicaltrials.eu" = "CTIS",
    "www.clinicaltrialsregister.eu" = "EUCTR",
    "www.isrctn.com" = "ISRCTN",
    "isrctn.com" = "ISRCTN",
    "NONE"
  )

  # identify register from trial identifier
  # added after CTGOV was decomissioned
  if (registerFromUrl == "NONE" &&
      register == "") {
    registerFromUrl <- switch(
      c(as.character(1:6)[sapply(
        c(regCtgov2, regCtis, regEuctr,
          regIsrctn, paste0("ISRCTN", regIsrctn),
          paste0(regEuctr, "-[3A-Z]{2,3}")),
        function(r) grepl(paste0("^", r, "$"), url))], "")[1],
      "1" = "CTGOV2",
      "2" = "CTIS",
      "3" = "EUCTR",
      "4" = "ISRCTN",
      "5" = "ISRCTN",
      "6" = "EUCTR",
      "NONE"
    )
  }

  # check parameters expectations
  if (register != "" &&
      registerFromUrl != "NONE" &&
      register != registerFromUrl) {
    stop("ctrGetQueryUrl(): 'url' and / or 'register' mismatch, url: '",
         deparse(url), "', register: '", deparse(register), "'",
         call. = FALSE
    )
  } else {
    if (registerFromUrl != "NONE") register <- registerFromUrl
  }

  # handle any mismatch of ctgov label with expected parameters
  if (grepl("^CTGOV[2]?$", register)) register <- ctgovVersion(url, register)

  # output value for return
  outdf <- function(qt, reg) {
    qt <- utils::URLdecode(qt)
    message("* Found search query from ", reg, ": ", qt)
    out <- data.frame(
      `query-term` = qt,
      `query-register` = reg,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    return(dfOrTibble(out))
  }

  ## identify query term per register

  #### EUCTR ####
  if (register == "EUCTR") {

    # search result page
    queryterm <- sub(".*/ctr-search/search[?](.*)", "\\1", url)

    # handle trial id, can only search without country
    queryterm <- gsub(paste0(
      "(.*", regEuctr, ")-[3A-Z]{2,3}(.*)"), "\\1\\2", queryterm)

    # single trial page
    queryterm <- sub(
      paste0(".*/ctr-search/trial/(", regEuctr, ")/.*"),
      "\\1", queryterm
    )

    # remove any intrapage anchor, e.g. #tableTop
    queryterm <- sub("#.+$", "", queryterm)

    # sanity correction for naked terms
    queryterm <- sub(
      paste0(
        "(^|&|[&]?\\w+=\\w+&)(",
        gsub("[=&^]", "", regQueryterm),
        "+)($|&\\w+=\\w+)"),
      "\\1query=\\2\\3",
      queryterm
    )

    # check if url was for results of single trial
    if (endsWith(url, ".*/results")) {
      queryterm <- paste0(queryterm)
    }
    queryterm <- sub(
      paste0("^(", regEuctr, ")$"),
      'query=\\1',
      queryterm
    )

    # inform user
    if (grepl("^http", queryterm) && queryterm == url) stop(
      "Not a search query: ", url
    )

    # return
    return(outdf(queryterm, register))
  }

  #### CTGOV classic ####
  if (register == "CTGOV") {

    # mangle query term
    queryterm <- sub(
      paste0(".*/ct2/show/[recodsult/]*(", regCtgov, ")([?][a-z]+.*|$)"),
      "\\1", url
    )
    # single trial page
    if (grepl("[?][a-z]+=\\w+", url, perl = TRUE) &&
        grepl(paste0("^", regCtgov, "$"), queryterm)) {
      message(
        "* Note: 'url' shows a single trial (and is returned by the ",
        "function) but also had search parameters: If interested in ",
        "search results, click 'Return to List' in browser and use ",
        "this as 'url'."
      )
    }

    # expert search page
    queryterm <- sub(".*/ct2/results/refine[?](.*)", "\\1", queryterm)
    # search results page
    queryterm <- sub(".*/ct2/results[?](.*)", "\\1", queryterm)
    # other results page
    queryterm <- sub(
      "(.*)&Search[a-zA-Z]*=(Search|Find)[a-zA-Z+]*",
      "\\1", queryterm
    )

    # remove empty parameters
    queryterm <- gsub("[a-z_0-9]+=&", "", queryterm)
    queryterm <- sub("&[a-z_0-9]+=$", "", queryterm)
    queryterm <- sub("&Search=Apply", "", queryterm)

    # correct naked terms
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)(\\w+|[a-zA-z0-9+-.:]+)($|&\\w+=\\w+)",
      "\\1term=\\2\\3", queryterm
    )

    # inform user
    if (grepl("^http", queryterm) && queryterm == url) stop(
      "Not a search query: ", url
    )

    # translate classic to current
    classicRoot <- "https://classic.clinicaltrials.gov/ct2/results?"
    url <- ctgovClassicToCurrent(url = paste0(classicRoot, queryterm))
    queryterm <- sub("https://clinicaltrials.gov/search[?][&]?", "", url)
    register <- "CTGOV2"

    # return
    return(outdf(queryterm, register))
  }

  #### ISRCTN ####
  if (register == "ISRCTN") {
    # single trial page
    queryterm <- sub(
      paste0("^.*/ISRCTN(", regIsrctn, ")([&?].+|$)"),
      "ISRCTN\\1", url
    )
    # search results page
    queryterm <- sub(".*/search[?](.*)", "\\1", queryterm)
    # remove unnecessary parameter
    queryterm <- sub("&searchType=[a-z]+-search", "", queryterm)
    # correct naked terms
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)(\\w+|[ a-zA-Z0-9+-]+)($|&\\w+=\\w+)",
      "\\1q=\\2\\3", queryterm
    )

    # inform user
    if (grepl("^http", queryterm) && queryterm == url) stop(
      "Not a search query: ", url
    )

    # single trial
    if (nchar(queryterm) && grepl(regIsrctn, queryterm) &&
        grepl("[?&].+=[^&]+", url)) {
      message(
        "* Note: 'url' shows a single trial (and is returned by the ",
        "function) but also had search parameters: If interested in ",
        "search results, click 'Back to results' in browser and use ",
        "this as 'url'."
      )
    }
    queryterm <- sub(
      paste0("(^", regIsrctn, "$)|^ISRCTN(", regIsrctn, "$)"),
      'q=\\1',
      queryterm
    )

    # return
    return(outdf(queryterm, register))
  }

  #### CTGOV2 ####
  if (register == "CTGOV2") {

    # extract search query
    queryterm <- sub(
      paste0(
        "(.*/study/", regCtgov,
        "/?[?]|.*/(expert-search|search)/?[?][&]?)([a-z]+.*$)"),
      "\\3", url
    )

    # single trial page
    if (nchar(queryterm) && queryterm != url && grepl(regCtgov2, url)) {
      message(
        "* Note: 'url' shows a single trial (and is returned by the ",
        "function) but also had search parameters: If interested in ",
        "search results, click on 'Search Results' in browser and use ",
        "this as 'url'."
      )
    }
    if (grepl(paste0("study/", regCtgov2), url)) {
      queryterm <-
        paste0(sub(paste0(".*study/(", regCtgov2, ").*"), "id=\\1", url))
    }
    queryterm <- sub(
      paste0("^(", regCtgov2, ")$"),
      'term=\\1',
      queryterm
    )

    # inform user
    if (grepl("^http", queryterm) && queryterm == url) stop(
      "Not a search query: ", url
    )

    # remove empty parameters, rank, sort
    queryterm <- gsub("[a-z_0-9]+=&", "", queryterm)
    queryterm <- sub("&[a-z_0-9]+=$", "", queryterm)
    queryterm <- sub("&rank=[0-9]+", "", queryterm)
    queryterm <- sub("&sort=[a-zA-Z%23,:]+(&|$)", "\\1", queryterm)
    queryterm <- sub("&viewType=[a-zA-Z]+(&|$)", "\\1", queryterm)

    # correct naked terms
    queryterm <- sub(
      "(^|&|[&]?\\w+=\\w+&)(\\w+|[a-zA-z0-9+-.:]+)($|&\\w+=\\w+)",
      "\\1term=\\2\\3", queryterm
    )

    # return
    return(outdf(queryterm, register))
  }

  #### CTIS ####
  if (register == "CTIS") {

    # 2024-09-21 defined by ctrdata Tampermonkey script:
    # https://euclinicaltrials.eu/ctis-public/search#searchCriteria=
    # {"containAll":"infection","containAny":"neonates","status":[4]}

    # search for trials
    queryterm <- sub("https://euclinicaltrials.eu/ctis-public/search#?", "", url)

    # if viewing a single trial
    queryterm <- sub(
      "https://euclinicaltrials.eu/ctis-public/view/([-0-9]+)",
      'searchCriteria={"number":"\\1"}',
      queryterm
    )
    queryterm <- sub(
      paste0("^(", regCtis, ")$"),
      'searchCriteria={"number":"\\1"}',
      queryterm
    )

    # inform user
    if (grepl("^http", queryterm) && queryterm == url) stop(
      "Not a search query: ", url
    )

    # return
    return(outdf(queryterm, register))
  }

  # default / NONE
  warning("ctrGetQueryUrl(): no clinical trial register ",
          "search URL found in parameter 'url' or in clipboard.",
          call. = FALSE, immediate. = TRUE
  )
  return(invisible(NULL))
}
# end ctrGetQueryUrl
