### ctrdata package

#' Find synonyms of an active substance
#'
#' An active substance can be identified by a recommended international
#' nonproprietary name (INN), a trade or product name, or a company code(s).
#' To find likely synonyms, the function retrieves from CTGOV2 the field
#' protocolSection.armsInterventionsModule.interventions.
#' Note this is mostly manually filled, thus may not be free of errors.
#'
#' @param activesubstance An active substance, in an atomic character vector
#'
#' @param verbose Print number of studies found in CTGOV2 for `activesubstance`
#'
#' @returns A named character vector of the active substance (input parameter),
#'  the MeSH code(s) and various names used in registered studies, or NULL if
#'  active substance was not found and may be invalid
#'
#' @importFrom utils str
#' @importFrom jqr jq
#' @importFrom stats quantile
#' @importFrom httr2 req_perform req_user_agent request
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#' activesubstance                mesh
#'      "imatinib" "imatinib mesylate"  "imatinib"  "gleevec"  "imatinib mesylate"
#'        "glivec"            "STI571"    "111201" "CGP57148"          "CGP57148B"
#'       "gleevac" "mesylate imatinib"    "ST1571"
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

  # TODO explore
  # https://rxnav.nlm.nih.gov/REST/rxcui.json?name=%s&search=1
  # https://rxnav.nlm.nih.gov/REST/approximateTerm.json?term=%s&maxEntries=1
  # https://rxnav.nlm.nih.gov/REST/rxcui/1656328/allallonym.json
  # https://rxnav.nlm.nih.gov/REST/rxcui/1656328/allproperties.json?prop=ALL

  # using CTGOV2 API as per
  # https://clinicaltrials.gov/data-api/about-api/api-migration#query-endpoints

  # parametrise endpoint
  apiEndpoint <- sprintf(paste0(
    "https://clinicaltrials.gov/api/v2/studies?",
    "query.intr=%s&fields=",
    # alternative names are in these fields
    # https://clinicaltrials.gov/data-api/about-api/study-data-structure#protocolSection
    # https://clinicaltrials.gov/policy/protocol-definitions#InterventionName
    "protocolSection.armsInterventionsModule.interventions.otherNames|",
    "protocolSection.armsInterventionsModule.interventions.name|",
    "derivedSection.interventionBrowseModule.meshes.term",
    "&pageSize=%i"
  ), activesubstance, 50L)

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

  # strategy
  # - find activesubstance in name and possibly otherNames
  # - obtain the MeSH term for these, deduplicate etc.
  # - get name and otherNames for the MeSH term(s)
  # - clean up, get most frequent names

  # get mesh from intervention names
  mesh1 <- jqr::jq(textConnection(jsn), paste0(
    '.studies[]

    | ( .protocolSection.armsInterventionsModule.interventions
    | if length == 0 then [false] else map(.name | test("^', activesubstance, '( |$)"; "i")) end
    ) as $indN

    | [ .derivedSection.interventionBrowseModule.meshes, $indN]
    | if (.[0] | length == 0) then {} else
    (transpose | map(select(.[1]) | .[0]) | .[])
    end
    ')) |>
    sapply(function(i) jsonlite::fromJSON(i)) |>
    unlist(use.names = FALSE) |>
    table() |>
    which.max() |>
    names()

  # get mesh from other names
  mesh2 <- jqr::jq(textConnection(jsn), paste0(
    '.studies[]

    | (.protocolSection.armsInterventionsModule.interventions
    | if length == 0 then null else
    map(.otherNames) | map (
    if length == 0 then false else
    (map(test("', activesubstance, '"; "i")) | any) end
    ) end
    ) as $indN

    | [ .derivedSection.interventionBrowseModule.meshes, $indN]
    | if (.[0] | length == 0) then {} else
    (transpose | map(select(.[1]) | .[0]) | .[])
    end
   '))|>
    sapply(function(i) jsonlite::fromJSON(i)) |>
    unlist(use.names = FALSE) |>
    table() |>
    which.max() |>
    names()

  # consolidate meshes
  meshes <- tolower(unique(c(mesh1, mesh2)))

  # process meshes
  if (length(meshes)) {

    # use mesh to find names and othernames
    names <- jqr::jq(textConnection(jsn), paste0(
      '.studies[]

    | ( .derivedSection.interventionBrowseModule.meshes
    | map(.term | test("', paste0(meshes, collapse = "|"), '"; "i"))
    ) as $outM

    | ( [ [.protocolSection.armsInterventionsModule.interventions[].name], $outM ]
    | transpose | map(select(.[1]) | .[0]) | .[]
    ) as $outN

    | ( [ [.protocolSection.armsInterventionsModule.interventions[].otherNames], $outM ]
    | transpose | map(select(.[1]) | .[0]) | .[]
    ) as $outO

    | {name: $outN, otherNames: $outO}

  '))

  } else {

    # get logical index in array of interventions
    names <- jqr::jq(textConnection(jsn), paste0(
      '.studies[]

    | ( .protocolSection.armsInterventionsModule.interventions
    | map(.name | test("^', activesubstance, '( |$)"; "i"))
    ) as $indN

    | ( [ [.protocolSection.armsInterventionsModule.interventions[].name], $indN ]
    | transpose | map(select(.[1]) | .[0]) | .[]
    ) as $outN

    | ( [ [.protocolSection.armsInterventionsModule.interventions[].otherNames], $indN ]
    | transpose | map(select(.[1]) | .[0]) | .[]
    ) as $outO

    | {name: $outN, otherNames: $outO}

  '))

  }

  # further process meshes and names
  names <- names |>
    sapply(function(i) jsonlite::fromJSON(i)) |>
    unlist(use.names = FALSE) |>
    sub("^([a-zA-Z ]+)$", "\\L\\1", x = _, perl = TRUE)

  # remove some decorations and terms in brackets
  names <- gsub("@|\U000AE|Trade name: ?| ?[(]?INN[)]?|[(]R[)]|\\(.+\\)", "", names)

  # remove other components
  names <- gsub("oral|tablet|capsule|withdrawal", "", names)

  # some otherNames are multiple active substances
  names <- names[!grepl("(,|/| and | or )", names)]

  # remove descriptive elements
  names <- names[!grepl("(intervent|treat|therapy|combin|none|arm )", names, ignore.case = TRUE)]

  # normalise
  names <- names |>
    sub("([0-9]+)[- ]([a-zA-Z]+)", "\\1\\2", x = _) |>
    sub("([a-zA-Z]+)[- ]([0-9]+)", "\\1\\2", x = _) |>
    trimws() |>
    sub("^([a-zA-Z ]+)$", "\\L\\1", x = _, perl = TRUE) |>
    table() |>
    sort(decreasing = TRUE)

  # TODO
  # sum(names)
  # length(names)
  # sum(names) / length(names)
  #
  # sapply(
  #   seq_along(names),
  #   function(i) if (i == 1L) names[i] else
  #   names[i] + as.integer(names[i - 1L])
  # )

  # prepare output
  names <- c(
    "activesubstance" = activesubstance,
    "mesh" = meshes,
    names(names))

  # return
  return(names)
}
# end ctrFindActiveSubstanceSynonyms
