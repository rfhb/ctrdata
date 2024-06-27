### ctrdata package

#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name (INN), a trade or product name, or a company code(s).
#' To find likely synonyms, the function retrieves from CTGOV2 the field
#' protocolSection.armsInterventionsModule.interventions.otherNames.
#' Note this is not free of error and should be checked manually.
#'
#' @param activesubstance An active substance, in an atomic character vector
#'
#' @param verbose Print number of studies found in CTGOV2 for `activesubstance`
#'
#' @return A character vector of the active substance (input parameter) and
#'  synonyms, or NULL if active substance was not found and may be invalid
#'
#' @importFrom httr GET set_config user_agent
#' @importFrom utils packageDescription str
#' @importFrom jqr jq
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#' #  [1] "imatinib"                    "Carcemia"                    "Cemivil"
#' #  [4] "CGP 57148"                   "CGP-57148B"                  "CGP57148B"
#' #  [7] "Gleevac"                     "gleevec"                     "Gleevec (Imatinib Mesylate)"
#' # [10] "Glevec"                      "glivec"                      "Imatinib"
#' # [13] "imatinib mesylate"           "Imatinib-AFT"                "IND # 55666"
#' # [16] "NSC #716051"                 "NSC-716051"                  "QTI571"
#' # [19] "ST1571"                      "STI 571"                     "STI-571"
#' # [22] "STI571"                      "tyrosine kinase inhibitors"
#'
#' }
ctrFindActiveSubstanceSynonyms <- function(activesubstance = "", verbose = FALSE) {

  # check parameters
  if ((length(activesubstance) != 1L) ||
      !is.character(activesubstance) ||
      (nchar(activesubstance) == 0L)) {
    stop("ctrFindActiveSubstanceSynonyms(): ",
         "activesubstance should be a single string.",
         call. = FALSE
    )
  }

  # using CTGOV2 API as per
  # https://clinicaltrials.gov/data-api/about-api/api-migration#query-endpoints

  # parametrise endpoint
  apiEndpoint <- sprintf(paste0(
    "https://clinicaltrials.gov/api/v2/studies?",
    "query.intr=%s&",
    # alternative names are in these fields
    "fields=protocolSection.armsInterventionsModule.interventions.otherNames|",
    "protocolSection.armsInterventionsModule.interventions.name&",
    "pageSize=%i"
  ), activesubstance, 1000L)

  # set user agent for httr and curl to inform registers
  httr::set_config(httr::user_agent(
    paste0(
      "ctrdata/", utils::packageDescription("ctrdata")$Version,
      " (https://cran.r-project.org/package=ctrdata)"
    )
  ))

  # call endpoint
  tmp <- try(httr::GET(url = apiEndpoint), silent = TRUE)

  # check result
  if (inherits(tmp, "try-error") || tmp[["status_code"]] == 404L) {
    message("Cound not search for active substance, error ",
            utils::str(tmp[min(length(tmp), 2L)]))
    return(NULL)
  }

  # digest results
  nrec <- jqr::jq(
    textConnection(rawToChar(tmp[["content"]])),
    ' .studies | length ')

  # inform user
  if (verbose || nrec == 0L) message(
    nrec, " studies found in CTGOV2 for active substance ", activesubstance)

  # extract otherNames for name
  asx <- jqr::jq(
    textConnection(rawToChar(tmp[["content"]])), paste0(
      ' .studies[] | .protocolSection.armsInterventionsModule.interventions
      | select ( length > 0 ) | .[]
      | select ( .name | test("^', activesubstance, '"; "i") and
                       ( test( ", | or | and | & | [+] " ) | not ) )
      | .otherNames | select( length > 0 ) | .[] ')
  )

  # prepare and return output
  asx <- gsub('"', "", asx)
  asx <- sort(asx)
  # remove some decorations
  asx <- gsub("@|\U000AE|Trade name: ?| ?[(]?INN[)]?|[(]R[)]", "", asx)
  # some otherNames are multiple active substances
  asx <- asx[!grepl("(,|/| and | or )", asx)]
  # remove descriptive elements
  asx <- asx[!grepl("(intervent|treat|therapy|combin|none)", asx, ignore.case = TRUE)]
  # deduplicate irrespective of case
  asx <- asx[!duplicated(tolower(asx))]
  asx <- unique(asx)
  asx <- c(activesubstance, asx)

  # return
  return(asx)
}
# end ctrFindActiveSubstanceSynonyms
