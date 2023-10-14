### ctrdata package

#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name (INN), a trade or product name, or a company code(s).
#' Retrieves the names of substance which are searched for by "CTGOV"
#' when querying for a given active substance.
#'
#' @param activesubstance An active substance, in an atomic character vector
#'
#' @return A character vector of the active substance (input parameter) and
#'  synonyms, or NULL if active substance was not found and may be invalid
#'
#' @importFrom httr GET set_config user_agent
#' @importFrom utils packageDescription
#' @importFrom xml2 read_html xml_find_all xml_text
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#' # [1] "imatinib" "gleevec" "sti 571" "glivec" "CGP 57148" "st1571"
#' }
ctrFindActiveSubstanceSynonyms <- function(activesubstance = "") {
  # check parameters
  if ((length(activesubstance) != 1L) ||
      !is.character(activesubstance) ||
      (nchar(activesubstance) == 0L)) {
    stop("ctrFindActiveSubstanceSynonyms(): ",
         "activesubstance should be a single string.",
         call. = FALSE
    )
  }

  # getting synonyms using httr since rvest::read_html
  # does not close network connection in case of 404
  ctgovfirstpageurl <-
    utils::URLencode(
      paste0(
        "https://classic.clinicaltrials.gov/ct2/results/details?term=",
        activesubstance
      )
    )

  # set user agent for httr and curl to inform registers
  httr::set_config(httr::user_agent(
    paste0(
      "ctrdata/", utils::packageDescription("ctrdata")$Version,
      " (https://cran.r-project.org/package=ctrdata)"
    )
  ))

  # get webpage
  tmp <- try(
    {
      httr::GET(url = ctgovfirstpageurl)
    },
    silent = TRUE
  )

  # check result
  if (inherits(tmp, "try-error") || tmp[["status_code"]] == 404L) {
    # 404 means active substance not found, thus early exit
    message("Check active substance '", activesubstance, "', may not exist.")
    return(NULL)
  }

  # extract from table "Terms and Synonyms Searched:"
  asx <- xml2::read_html(tmp)
  asx <- xml2::xml_find_all(asx, '//*[@id="searchdetail"]/div[1]/div/table[1]/tbody/tr/td[1]')
  asx <- trimws(xml2::xml_text(asx))

  # prepare and return output
  asx <- c(activesubstance, asx)
  return(unique(asx))
}
# end ctrFindActiveSubstanceSynonyms
