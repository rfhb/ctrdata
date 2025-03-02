### ctrdata package

#' Calculate trial concept from various fields and registers
#'
#' Across registers, calculate a trial concept (e.g., status of recruitment).
#' The concept is defined within ctrdata based on common understanding and
#' any relevant publications. Details of the implementation are printed,
#' if no data frame is specified, to show the peculiarities of the registers
#' for calculating the concept.
#'
#' The names of the functions can also be used for the argument `calculate`
#' in \link{dbGetFieldsIntoDf} to achieve the same purpose already at the
#' time that a data frame with data is generated from the trial collection.
#' This approach will in most cases be more efficient and preferred over
#' using \link{dfCalculateConcept}.
#'
#' Note that the functions do not operate on historic versions (CTGOV2) but
#' only on information from the latest record.
#'
#' @param name String with name of a function to be applied to `df`, or regular
#' expression to list available functions.
#'
#' @param df Optional. Data frame with fields needed to apply function `name`.
#'
#' @returns Data frame with additional column of name `name` as calculated
#' applying the function `name`. If `df` is `NULL` (default), either a list of
#' functions corresponding to `name` or, if exactly one function is identified,
#' prints details of the function `name` to explain the implementation of the
#' concept and to return invisibly the names of fields needed for its calculation.
#'
#' @export
#' @importFrom utils ls.str
#' @importFrom dplyr left_join
#'
#' @examples
#'
#' # list names of all available functions in ctrdata
#' dfCalculateConcept()
#'
#' # list names of functions for regular expression of name
#' dfCalculateConcept(name = "status")
#'
#' # describe a specific function
#' dfCalculateConcept(name = ".statusRecruitment")
#'
#' # print descriptions of all functions
#' invisible(sapply(dfCalculateConcept(), dfCalculateConcept))
#'
#' # apply dfCalculateConcept to data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials",
#'   flags = RSQLite::SQLITE_RO)
#'
#' # use with existing data frame,
#' # first get fields needed
#' trialsDf <- dbGetFieldsIntoDf(
#'   fields = unlist(dfCalculateConcept(name = ".statusRecruitment")),
#'   con = dbc)
#'
#' # then calculate
#' dfCalculateConcept(
#'   name = ".statusRecruitment",
#'   df = trialsDf)
#'
#' # or use already when creating a trial data frame
#' # to subset to the unique records of clinical trials
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = ".isUniqueTrial",
#'   con = dbc)
#' trialsDf[trialsDf[[".isUniqueTrial"]], ]
#'
dfCalculateConcept <- function(name = ".*", df = NULL) {

  # function information
  if (is.null(df)) {

    # describe
    if (length(ls(
      getNamespace("ctrdata"),
      all.names = TRUE,
      pattern = paste0("^", name, "$"))) == 1L) {

      # return
      return(do.call(name, list()))
    }

    # get all functions
    fcts <- capture.output(utils::ls.str(
      getNamespace("ctrdata"),
      all.names = TRUE,
      pattern = "^[.]")
    )
    fcts <- sub(
      "^(.+?) :.+", "\\1",
      fcts[grepl("function \\(df = NULL\\)", fcts)]
    )
    fcts <- fcts[grepl(name, fcts, ignore.case = TRUE)]

    # return
    return(fcts)

  }

  # check
  stopifnot(is.data.frame(df))

  # inform user
  message(
    "Calculating ", name,
    "...                            \r",
    appendLF = FALSE)

  # apply function
  fldsNeeded <- suppressMessages(unique(
    c("_id", unlist(dfCalculateConcept(name), use.names = FALSE))))
  result <- do.call(name, list(df[, fldsNeeded, drop = FALSE]))

  # check
  stopifnot(is.data.frame(result) && ncol(result) >= 2L)

  # merge
  df <- dplyr::left_join(
    x = df, y = result, by = "_id")

  # return
  return(dfOrTibble(df))

} # end dfCalculateConcept
