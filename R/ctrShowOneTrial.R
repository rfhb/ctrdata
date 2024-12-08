#' Show structure and all data of a single trial
#'
#' If used interactively, the function shows a widget of all data in the trial
#' as a tree of field names and values. The widget opens in the default browser.
#' Fields names and values can be search and selected. Selected fields can be
#' copied to the clipboard for use with function \link{dbGetFieldsIntoDf}.
#' If used non-interactively, the data frame is shown that results from
#' \link{dfTrials2Long} for the trial with the identifier.
#' The trial is searched online if no database \code{con} is provided or if
#' the trial is not found in the database.
#'
#' @param identifier A trial identifier string
#'
#' @inheritParams ctrDb
#'
#' @return Invisibly, the data frame resulting from \link{dfTrials2Long} for
#' the trial with the identifier.
#'
#' @export
#'
#' @importFrom nodbi docdb_query src_sqlite
#' @importFrom DBI dbDisconnect
#' @importFrom jsonlite toJSON
#' @importFrom jqr jq
#' @importFrom V8 JS
#'
#' @examples
#' \dontrun{
#'
#' dbc <- nodbi::src_sqlite(
#'    dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'    collection = "my_trials",
#'    RSQLite::SQLITE_RO)
#'
#' id <- "NCT00617929"
#' id <- "2012-003632-23"
#' id <- "80181452"
#' id <- "2022-501142-30-00"
#'
#' ctrGetQueryUrl(url = id)
#'
#' ctrShowOneTrial(identifier = id)
#' ctrShowOneTrial(identifier = id, con = dbc)
#'
#' }
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
      nodbi::src_sqlite(collection = "oneTrial"))

    # remove temporary database
    on.exit(try(suppressWarnings(
      DBI::dbDisconnect(conTemp$con, shutdown = TRUE)),
      silent = TRUE), add = TRUE)

    # get trial data
    loadResult <- suppressMessages(suppressWarnings(
      ctrLoadQueryIntoDb(
        queryterm = queryTerm,
        con = conTemp
      )))

    # get data
    trialData <- getTrial(id = identifier, con = conTemp)

  }

  # checks
  if (is.null(trialData) ||
      !nrow(trialData)) {
    stop("No data found for trial ", identifier)
  }

  ## user information

  # transform
  itemsDf <- suppressMessages(
    dfTrials2Long(df = trialData))

  # early exit if not interactive
  if (!interactive()) return(itemsDf)

  # present
  View(itemsDf)

  # mangle back into json
  trialData <- jsonlite::toJSON(trialData[1, -1])

  # simplify
  trialData <- jqr::jq(trialData, " .[] ")

  # present widget
  ctrViewOneTrial(message = list(
    "data" = V8::JS(trialData)
  ))

  ## return
  invisible(itemsDf)

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
ctrViewOneTrial <- function(
    message,
    width = NULL,
    height = NULL,
    elementId = NULL) {

  # create widget
  widget <- htmlwidgets::createWidget(
    name = 'ctrViewOneTrial',
    x = message,
    width = "95%",
    height = height,
    package = 'ctrdata',
    elementId = elementId
  )

  # save and serve widget in browser
  # so that copy to clipboard works
  tf <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(
    widget = widget,
    file = tf
  )
  utils::browseURL(tf)

  # return
  invisible()

}

#' Shiny bindings for ctrViewOneTrial
#'
#' Output and render functions for using ctrViewOneTrial within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a ctrViewOneTrial
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @noRd
#' @keywords internal
#'
ctrViewOneTrialOutput <- function(outputId, width = '100%', height = '400px'){

  htmlwidgets::shinyWidgetOutput(outputId, 'ctrViewOneTrial', width, height, package = 'ctrdata')

}
#'
#' @noRd
#' @keywords internal
#'
renderCtrViewOneTrial <- function(expr, env = parent.frame(), quoted = FALSE) {

  if (!quoted) { expr <- substitute(expr) } # force quoted

  htmlwidgets::shinyRenderWidget(expr, ctrViewOneTrialOutput, env, quoted = TRUE)

}
