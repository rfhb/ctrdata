### ctrdata package

#' Get value for variable of interest
#'
#' Get information for variable of interest (e.g., clinical endpoints)
#' from a long data frame of protocol- or result-related trial information
#' as returned by \link{dfTrials2Long}.
#' Parameters `valuename`, `wherename` and `wherevalue` are
#' matched using Perl regular expressions and ignoring case.
#' See also \link{ctrdata-trial-concepts} for how to extract and calculate
#' concepts of interest across registers.
#'
#' @param df A data frame (or tibble) with four columns (`_id`,
#'  `identifier`, `name`, `value`) as returned by \link{dfTrials2Long}
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
#' @returns A data frame (or tibble, if \code{tibble} is loaded)
#' that includes the values of interest, with columns
#' `_id`, `identifier`, `name`, `value` and `where` (with the
#' contents of `wherevalue` found at `wherename`).
#' Contents of `value` are strings unless all its elements
#' are numbers. The `identifier` is generated by
#' function \link{dfTrials2Long} to identify matching elements,
#' e.g endpoint descriptions and measurements.
#'
#' @importFrom stringi stri_detect_regex
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials",
#'   flags = RSQLite::SQLITE_RO)
#'
#' dfwide <- dbGetFieldsIntoDf(
#'     fields = c(
#'         ## ctgov - typical results fields
#'         # "clinical_results.baseline.analyzed_list.analyzed.count_list.count",
#'         # "clinical_results.baseline.group_list.group",
#'         # "clinical_results.baseline.analyzed_list.analyzed.units",
#'         "clinical_results.outcome_list.outcome",
#'         "study_design_info.allocation",
#'         ## euctr - typical results fields
#'         # "trialInformation.fullTitle",
#'         # "baselineCharacteristics.baselineReportingGroups.baselineReportingGroup",
#'         # "trialChanges.hasGlobalInterruptions",
#'         # "subjectAnalysisSets",
#'         # "adverseEvents.seriousAdverseEvents.seriousAdverseEvent",
#'         "endPoints.endPoint",
#'         "subjectDisposition.recruitmentDetails"
#'     ), con = dbc
#' )
#'
#' dflong <- dfTrials2Long(df = dfwide)
#'
#' ## get values for the endpoint 'response'
#' dfName2Value(
#'     df = dflong,
#'     valuename = paste0(
#'         "clinical_results.*measurement.value|",
#'         "clinical_results.*outcome.measure.units|",
#'         "endPoints.endPoint.*tendencyValue.value|",
#'         "endPoints.endPoint.unit"
#'     ),
#'     wherename = paste0(
#'         "clinical_results.*outcome.measure.title|",
#'         "endPoints.endPoint.title"
#'     ),
#'     wherevalue = "response"
#' )
#'
dfName2Value <- function(df, valuename = "",
                         wherename = "", wherevalue = "") {
  # check parameters
  if (valuename == "") {
    stop("'valuename' must be specified.",
         call. = FALSE
    )
  }
  if (!identical(
    names(df),
    c("_id", "identifier", "name", "value")
  )) {
    stop("'df' does not seem to come from dfTrials2Long()",
         call. = FALSE
    )
  }

  # indices of valuename
  indexVnames <- which(grepl(valuename, df[["name"]],
                             perl = TRUE, ignore.case = TRUE
  ))
  if (!length(indexVnames)) stop("No rows found for 'valuename' = ", valuename)

  # if no where... are specified, just
  # return rows where name corresponds
  # to valuename
  if (wherename == "" && wherevalue == "") {
    # get relevant rows
    out <- df[indexVnames, , drop = FALSE]
  } else { # if where... are specified, continue

    # get where... indices per trial
    indexRows <- which(
      grepl(wherename, df[["name"]], perl = TRUE, ignore.case = TRUE) &
        grepl(wherevalue, df[["value"]], perl = TRUE, ignore.case = TRUE)
    )
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
            )
          ]
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
              c("_id", "where"),
              drop = FALSE
            ],
            by = "_id"
          )
        }
      }
    )

    # bind into data frame
    out <- do.call(
      rbind,
      c(out, stringsAsFactors = FALSE, make.row.names = FALSE)
    )
  } # if where...

  # value column is character
  # try to convert it to numeric
  val <- suppressWarnings(
    as.numeric(gsub(",", "", out[["value"]]))
  )
  # use if converted ok
  if (all(is.na(val) == is.na(out[["value"]]))) {
    out["value"] <- val
  }
  # remove any duplicates such as
  # from duplicate where... criteria
  out <- unique(out)
  row.names(out) <- NULL

  # inform user
  message(
    "Returning values for ", length(unique(out[["_id"]])),
    " out of ", length(unique(df[["_id"]])), " trials"
  )

  # return
  return(dfOrTibble(out))

} # end dfName2Value
