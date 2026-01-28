### ctrdata package

#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name (INN), a trade or product name, or a company code(s).
#' To find likely synonyms, the function retrieves from CTGOV2 the field
#' protocolSection.armsInterventionsModule.interventions.otherNames.
#' Note this does not seem to be based on choices from a dictionary but
#' may be manually filled, thus is not free of error and needs to be checked.
#'
#' @param activesubstance An active substance, in an atomic character vector
#'
#' @param verbose Print number of studies found in CTGOV2 for `activesubstance`
#'
#' @returns A character vector of the active substance (input parameter) and
#'  synonyms, or NULL if active substance was not found and may be invalid
#'
#' @importFrom utils str
#' @importFrom jqr jq
#' @importFrom stats quantile
#' @importFrom httr2 req_perform req_user_agent request
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#' #  [1] "imatinib"          "CGP 57148"         "CGP 57148B"
#' #  [4] "CGP57148B"         "Gleevec"           "GLIVEC"
#' #  [7] "Imatinib"          "Imatinib Mesylate" "NSC 716051"
#' # [10] "ST1571"            "STI 571"           "STI571"
#' }
#'
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

  # call endpoint
  res <- try(httr2::req_perform(
    httr2::req_user_agent(
      httr2::request(
        base_url = apiEndpoint),
      ctrdataUseragent)), silent = TRUE)

  # check result
  if (inherits(res, "try-error") || res[["status_code"]] == 404L) {
    message(
      "Cound not search for active substance, error ",
      utils::str(res[min(length(res), 2L)])
    )
    return(NULL)
  }

  # get content
  jsn <- rawToChar(res[["body"]])

  # digest results
  nrec <- jqr::jq(textConnection(jsn), " .studies | length ")

  # inform user
  if (verbose || nrec == 0L) message(
    nrec, " studies found in CTGOV2 for active substance ", activesubstance)

  # extract otherNames for name
  asx <- jqr::jq(textConnection(jsn), paste0(
    ' .studies[] | .protocolSection.armsInterventionsModule.interventions
    | select ( length > 0 ) | .[]
    | select ( .name | test("^', activesubstance, '( |$)"; "i") )
    | .otherNames | select( length > 0 ) | .[]
  '))

  # prepare output
  asx <- gsub('"', "", asx)
  # remove some decorations
  asx <- gsub("@|\U000AE|Trade name: ?| ?[(]?INN[)]?|[(]R[)]", "", asx)
  # some otherNames are multiple active substances
  asx <- asx[!grepl("(,|/| and | or )", asx)]
  # remove descriptive elements
  asx <- asx[!grepl("(intervent|treat|therapy|combin|none)", asx, ignore.case = TRUE)]
  # normalise a bit
  asx <- gsub("[-]", " ", asx)

  # deduplicate irrespective of case
  asx <- asx[!duplicated(tolower(asx))]
  asx <- unique(asx)
  asx <- sort(asx)
  asx <- c(activesubstance, asx[asx != activesubstance])

  # return
  return(asx)
}
# end ctrFindActiveSubstanceSynonyms
