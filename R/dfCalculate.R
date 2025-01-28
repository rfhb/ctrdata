### ctrdata package

#' Calculate fields from data in other fields
#'
#' Across registers, calculate from fields in a data frame a common trial
#' concept (e.g., status of recruitment) that is defined within ctrdata
#' based on general understanding and any publications concerning the concept,
#' which is printed to explain if no data frame is specified.
#'
#' The names of the functions can also be used for the argument `calculate`
#' in \link{dbGetFieldsIntoDf} to achieve the same purpose already at the
#' time that a data frame with data is generated from the trial collection.
#'
#' @param name String with name of function to be applied to `df`, or regular
#' expression to list available functions.
#'
#' @param df Optional. Data frame with fields needed to apply function `name`.
#'
#' @returns Data frame with additional column of name `name` as calculated
#' applying the function `name`. If `df` is not specified, either a list of
#' functions corresponding to `name` or, if exactly one function is identified,
#' prints details of the function `name` to explain the implementation of the
#' concept and returns invisibly the names of fields needed for the calculation.
#'
#' @export
#' @importFrom utils ls.str
#'
#' @examples
#'
#' # list names of all available functions in ctrdata
#' dfCalculate()
#'
#' # list names of functions for regular expression of name
#' dfCalculate(name = "status")
#'
#' # describe a specific function
#' dfCalculate(name = ".statusRecruitment")
#'
#' # print descriptions of all functions
#' invisible(sapply(dfCalculate(), dfCalculate))
#'
#' # apply dfCalculate to data frame
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials",
#'    RSQLite::SQLITE_RO)
#'
#' # use with existing data frame,
#' # first get fields needed
#' trialsDf <- dbGetFieldsIntoDf(
#'   fields = unlist(dfCalculate(name = ".statusRecruitment")),
#'   con = dbc)
#'
#' # then calculate
#' dfCalculate(
#'   name = ".statusRecruitment",
#'   df = trialsDf)
#'
#' # or use already when creating a trial data frame
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = ".startDate",
#'   con = dbc)
#'
dfCalculate <- function(name = ".*", df = NULL) {

  # function information
  if (is.null(df)) {

    # describe

    # if (exists(name)) {
    if (length(ls(
      getNamespace("ctrdata"),
      all.names = TRUE,
      pattern = paste0("^", name, "$"))) == 1L) {

      # TODO
      if (exists(name)) {
        return(do.call(name, list()))
      } else {
        return(eval(parse(text = paste0("ctrdata::", name, "()"))))
      }

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

  # apply function. not using do.call
  # when the package is not attached
  # TODO
  if (exists(name)) {
    df[[name]] <- do.call(name, list(df))
  } else {
    df[[name]] <- eval(parse(text = paste0("ctrdata::", name, "(df = df)")))
  }

  # return
  return(dfOrTibble(df))

} # end dfCalculate


# TODO delete
if (FALSE) {

  dfCalculate()

  dfCalculate(".statusRecruitment")

  # describe all
  invisible(sapply(dfCalculate(), dfCalculate))

  dfCalculate(name = ".statusRecruitment", df = df)

  dfCalculate(name = "a")

  ctrdata::.statusRecruitment()

  .statusRecruitment()

  .statusRecruitment

}
