### ctrdata package

#' Open register to show query results or search page
#'
#' Open advanced search pages of register(s), or execute search in browser
#'
#' @param url of search results page to show in the browser. To open the
#'   browser with a previous search, the output of \link{ctrGetQueryUrl}
#'   or \link{dbQueryHistory} can be used. Can be left as empty string
#'   (default) to open the advanced search page of \code{register}.
#'
#' @param register Register(s) to open, "EUCTR", "CTGOV2",
#'   "ISRCTN" or "CTIS". Default is empty string, and this opens the
#'   advanced search page of the register(s).
#'
#' @param copyright (Optional) If set to \code{TRUE}, opens only the
#'   copyright pages of all registers.
#'
#' @export
#'
#' @returns (String) Full URL corresponding to the shortened \code{url}
#'   in conjunction with \code{register} if any, or invisibly
#'   \code{TRUE} if no \code{url} is specified.
#'
#' @examples
#'
#' # Open all and check copyrights before using registers
#' ctrOpenSearchPagesInBrowser(copyright = TRUE)
#'
#' # Open specific register advanced search page
#' ctrOpenSearchPagesInBrowser(register = "CTGOV2")
#' ctrOpenSearchPagesInBrowser(register = "CTIS")
#' ctrOpenSearchPagesInBrowser(register = "EUCTR")
#' ctrOpenSearchPagesInBrowser(register = "ISRCTN")
#'
#' # Open all queries that were loaded into demo collection
#' dbc <- nodbi::src_sqlite(
#'     dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'     collection = "my_trials"
#' )
#'
#' dbh <- dbQueryHistory(
#'     con = dbc
#' )
#'
#' for (r in seq_len(nrow(dbh))) {
#'     ctrOpenSearchPagesInBrowser(dbh[r, ])
#' }
#'
ctrOpenSearchPagesInBrowser <- function(
    url = "",
    register = "",
    copyright = FALSE) {
  
  ## in case a browser is not available
  ctrOpenUrl <- function(u) {
    try(utils::browseURL(u), silent = TRUE)
  }
  
  ## check combination of arguments to select action
  
  # - open copyright or similar pages
  if (copyright) {
    sapply(
      c(
        "https://www.clinicaltrialsregister.eu/disclaimer.html",
        "https://clinicaltrials.gov/about-site/terms-conditions#usage",
        "https://www.isrctn.com/page/faqs#usingISRCTN",
        "https://euclinicaltrials.eu/about-this-website/"
      ),
      ctrOpenUrl
    )
    return(invisible(TRUE))
  }
  
  # - open register search page(s)
  if (is.atomic(url) && url == "") {
    url <- c(
      "CTGOV2" = "https://clinicaltrials.gov/#main-content",
      "CTIS" = "https://euclinicaltrials.eu/ctis-public/search",
      "EUCTR" = "https://www.clinicaltrialsregister.eu/ctr-search/search",
      "ISRCTN" = "https://www.isrctn.com/editAdvancedSearch"
    )
    
    if (is.atomic(register) &&
        sum(register %in% registerList, na.rm = TRUE) == 1L) {
      ctrOpenUrl(url[register])
    } else {
      sapply(url, ctrOpenUrl)
    }
    
    return(invisible(TRUE))
  }
  
  # - get shortened url and register
  if (is.atomic(url) && url != "") {
    url <- ctrGetQueryUrl(url = url, register = register)
  }
  
  # - get from a data frame, such as from
  #   ctrQueryHistoryInDb() or ctrGetQueryUrl()
  if (is.data.frame(url) &&
      all(substr(names(url), 1, 6) == "query-")) {
    nr <- nrow(url)
    if (nr > 1L) {
      warning("Using last query",
              call. = FALSE, immediate. = TRUE
      )
    }
    register <- url[nr, "query-register", drop = TRUE]
    url <- url[nr, "query-term", drop = TRUE]
  }
  
  # - open search or view from url and register
  if (is.atomic(url) && url != "" && register != "") {
    pre <- "(^|[?&]*)"
    post <- "(&|$)|"
    
    # - open parametrised search
    if (grepl(paste0(
      # pre, "term=", regCtgov, post,
      pre, "id=", regCtgov2, post,
      pre, "number=", regCtis, post,
      pre, "query=", regEuctr, post,
      pre, "q=ISRCTN", regIsrctn, post, "^$"
    ), url)
    ) {
      
      # - open single study from url and register
      url <- sub(paste0(
        ".*?(", paste0(
          c(regCtgov, regCtgov2, regCtis, regEuctr, regIsrctn),
          collapse = "|"
        ), ").*"
      ), "\\1", url)
      
      url <- switch(
        register,
        "CTGOV" = ctgovClassicToCurrent(url),
        "CTGOV2" = paste0("https://clinicaltrials.gov/study/", url, "#main-content"),
        "CTIS" = paste0("https://euclinicaltrials.eu/ctis-public/view", url),
        "EUCTR" = paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=", url, "#tabs"),
        "ISRCTN" = paste0("https://www.isrctn.com/ISRCTN", url)
      )
    } else {
      
      # - open search
      #   adjust to use expert search for CTGOV2
      if (grepl("(|\\[|\\]|AREA)", url, ignore.case = FALSE) &&
          (register == "CTGOV2")) register <- "CTGOV2expert"
      #
      url <- switch(
        register,
        "CTGOV" = ctgovClassicToCurrent(url),
        "CTGOV2" = paste0("https://clinicaltrials.gov/search?", url),
        "CTGOV2expert" = paste0("https://clinicaltrials.gov/expert-search?", url),
        "CTIS" = paste0("https://euclinicaltrials.eu/ctis-public/search?", url),
        "EUCTR" = paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?", url, "#tabs"),
        "ISRCTN" = paste0("https://www.isrctn.com/search?", url)
      )
    }
    
    ctrOpenUrl(url)
    return(url)
  }
  
  # if not returned before
  invisible(NULL)
}
# end ctrOpenSearchPagesInBrowser
