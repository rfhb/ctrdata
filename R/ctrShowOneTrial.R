#' Show full structure and all data of a trial
#'
#' If used interactively, the function shows a widget of all data in the trial
#' as a tree of field names and values. The widget opens in the default browser.
#' Fields names and values can be search and selected. Selected fields can be
#' copied to the clipboard for use with function \link{dbGetFieldsIntoDf}.
#' The trial is retrieved with \link{ctrLoadQueryIntoDb} if no database
#' \code{con} is provided or if the trial is not in database \code{con}.
#' For use in a Shiny app, see output and render functions in source code
#' \ifelse{latex}{\out{
#' \href{https://github.com/rfhb/ctrdata/blob/master/R/ctrShowOneTrial.R\#L196}{here}}}{
#' \href{https://github.com/rfhb/ctrdata/blob/master/R/ctrShowOneTrial.R#L196}{here}}.
#'
#' This is the widget for CTIS trial 2022-501142-30-00:
#'
#' \if{html}{
#'   \figure{ctrdata_ctrShowOneTrial.jpg}
#' }
#' \if{latex}{
#'   \out{\begin{center}}\figure{ctrdata_ctrShowOneTrial.jpg}\out{\end{center}}
#' }
#'
#' @param identifier A trial identifier (`_id`) string, see examples
#'
#' @inheritParams ctrDb
#'
#' @returns Invisibly, the trial data for constructing an HTML widget.
#'
#' @export
#'
#' @importFrom nodbi docdb_query src_sqlite
#' @importFrom jsonlite toJSON
#' @importFrom jqr jq
#' @importFrom V8 JS
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials",
#'   flags = RSQLite::SQLITE_RO)
#'
#' # get sample of identifiers of trials in database
#' sample(dbFindIdsUniqueTrials(con = dbc), 5L)
#'
#' # all such identifiers work
#' id <- "2014-003556-31"
#' id <- "2014-003556-31-SE"
#' id <- "76463425"
#' id <- "ISRCTN76463425"
#' id <- "NCT03431558"
#' id <- "2022-501142-30-00"
#'
#' # note these ids also work with
#' # ctrGetQueryUrl(url = id) and
#' # ctrLoadQueryIntoDb(queryterm = id, ...)
#'
#' # show widget for user to explore and search content as well as to
#' # select fields of interest and to click on "Copy names of selected
#' # fields to clipboard..." to use them with dbGetFieldsIntoDf()
#' ctrShowOneTrial(identifier = id, con = dbc)
#'
ctrShowOneTrial <- function(
    identifier = NULL,
    con = NULL) {

  ## check trial identifier

  ## helper
  getTrial <- function(id, con) {

    # get data
    trialData <- nodbi::docdb_query(
      src = con,
      key = con$collection,
      # need regex as EUCTR suffixes _id with country
      query = paste0('{"_id": {"$regex": "^', id, '"}}'),
      limit = 1L
    )

    # return
    return(trialData)

  }

  ## if con, search locally
  if (!is.null(con)) {

    # get data
    trialData <- getTrial(id = identifier, con = con)

  }

  ## if not con or no result, search remotely
  if (is.null(con) ||
      is.null(trialData) ||
      !nrow(trialData)) {

    # get search url
    queryTerm <- ctrGetQueryUrl(url = identifier)

    # temporary database
    conTemp <- suppressMessages(
      nodbi::src_sqlite(collection = "oneTrial")
    )

    # remove temporary database
    on.exit(try(rm(conTemp), silent = TRUE), add = TRUE)

    # get trial data
    loadResult <- suppressMessages(suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = queryTerm,
        euctrresults = TRUE,
        con = conTemp
      )
    ))

    # checks
    if (loadResult$n == 0L) {
      stop("Unexpected records found for trial ", identifier)
    }

    # get data
    trialData <- getTrial(id = identifier, con = conTemp)

  }

  # checks
  if (is.null(trialData) ||
      !nrow(trialData)) {
    stop("No data found for trial ", identifier)
  }

  # mangle back into json
  trialData <- jsonlite::toJSON(trialData[1, -1])

  # simplify
  trialData <- jqr::jq(trialData, " .[] ")

  # present widget
  msg <- list("data" = V8::JS(trialData))
  if (interactive()) ctrShowOneTrialWidget(message = msg)

  ## return
  return(invisible(msg))

}


#' ctrViewOneTrial
#'
#' generate jstree widget
#'
#' @importFrom htmlwidgets createWidget saveWidget
#' @importFrom utils browseURL
#'
#' @noRd
#'
ctrShowOneTrialWidget <- function(
    message,
    width = NULL,
    height = NULL,
    elementId = NULL) {

  # create widget
  widget <- htmlwidgets::createWidget(
    name = "ctrShowOneTrialWidget",
    x = message,
    width = "95%",
    height = height,
    package = "ctrdata",
    elementId = elementId
  )

  if (interactive()) {

    # save and serve widget in browser
    # so that copy to clipboard works
    tf <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(
      widget = widget,
      file = tf
    )
    utils::browseURL(tf)

  }

  # return
  return(widget)

}

#' Shiny bindings for ctrShowOneTrialWidget
#'
#' Output and render functions for using ctrShowOneTrialWidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a ctrShowOneTrialWidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @noRd
#' @keywords internal
#'
ctrShowOneTrialOutput <- function(outputId, width = "100%", height = "400px") {

  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Install package 'htmlwidgets' to use this function.", call. = FALSE)
  }

  htmlwidgets::shinyWidgetOutput(outputId, "ctrShowOneTrialWidget", width, height, package = "ctrdata")
}
#'
#' @noRd
#' @keywords internal
#'
renderCtrShowOneTrial <- function(expr, env = parent.frame(), quoted = FALSE) {

  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Install package 'htmlwidgets' to use this function.", call. = FALSE)
  }

  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted

  htmlwidgets::shinyRenderWidget(expr, ctrShowOneTrialOutput, env, quoted = TRUE)

}
