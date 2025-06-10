### ctrdata package
### utility functions

#### variable definitions ####

# prototype return structure
emptyReturn <- list(n = 0L, success = NULL, failed = NULL)
#
# EUCTR definitions
countriesEUCTR <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SE", "SI", "ES", "GB", "IS", "LI",
  "NO", "3RD")
countriesActive <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SE", "SI", "ES",       "IS", "LI",
  "NO", "3RD")
#
# regexpr
# - queryterm and urls
regQueryterm <- "[^-.a-zA-Z0-9=?+&#%_:\"/, {}\\(\\)]"
# - EudraCT e.g. 2010-022945-52
regEuctr <- "[0-9]{4}-[0-9]{6}-[0-9]{2}"
# - CTGOV
regCtgov <- "NCT[0-9]{8}"
# - CTGOV2
regCtgov2 <- regCtgov
# - regIsrctn
regIsrctn <- "[0-9][0-9]{7}"
# - CTIS e.g. 2022-501549-57-00
regCtis <- "[0-9]{4}-[0-9]{6}-[0-9]{2}-[0-9]{2}"
#
# register list, order important
registerList <- c("EUCTR", "CTGOV", "CTGOV2", "ISRCTN", "CTIS")


#### functions ####

#' ctgovVersion
#'
#' Checks for mismatch between label CTGOV and CTGOV2
#' and tries to guess the correct label
#'
#' @param url url or data frame with query term
#' @param register any of the register names
#'
#' @keywords internal
#' @noRd
#'
#' @returns string
#'
#' @examples
#'
#' ctgovVersion("https://www.clinicaltrials.gov/ct2/show/NCT02703272", "")
#' ctgovVersion("https://classic.clinicaltrials.gov/ct2/results?cond=&term=NCT02703272&cntry=", "")
#' ctgovVersion("https://clinicaltrials.gov/ct2/results?cond=&term=NCT02703272&cntry=", "")
#' ctgovVersion("https://classic.clinicaltrials.gov/ct2/show/NCT02703272?term=NCT02703272&draw=2&rank=1")
#' ctgovVersion("https://clinicaltrials.gov/ct2/results?cond=", "")
#'
#' ctgovVersion("https://www.clinicaltrials.gov/search?term=NCT04412252,%20NCT04368728", "")
#' ctgovVersion("term=NCT04412252,%20NCT04368728", "CTGOV2")
#' ctgovVersion("https://www.clinicaltrials.gov/search?distance=50&cond=Cancer", "")
#'
ctgovVersion <- function(url, register) {

  # in case the input is from dbQueryHistory
  if (!is.atomic(url)) try({url <- url[["query-term"]]}, silent = TRUE)
  if (inherits(url, "try-error") || is.null(url)) return(register)

  # logic 1
  if (grepl(paste0(
    "clinicaltrials[.]gov/ct2/|",
    # these are classic-specific
    "[?&]rsub=|[?&]type=|[?&]rslt=|[?&]gndr=|[?&]recrs=|[?&]phase=|",
    "[?&]age=|[?&]cntry=|[?&][a-z]+_[a-z]+="), url)) { # e.g. strd_s
    message("* Appears specific for CTGOV Classic website")
    return("CTGOV")
  }

  # logic 2
  if (grepl(paste0(
    # clear identifiers of CTGOV2
    "aggFilters|clinicaltrials[.]gov/(search|study)[/?]|",
    "[?&]country=|[:][^/]|%3[aA]"), url)) {
    message("* Appears specific for CTGOV REST API 2.0")
    return("CTGOV2")
  }

  # default return
  message("Not overruling register label ", register)
  return(register)

}


#' ctgovClassicToCurrent
#'
#' Fully translates a user's search query URL from the classic website
#' into a query for the current website, with all search parameters.
#' added to accomodate classic website retirement as of 2024-06-25.
#' Note this function only handles search queries, but not display
#' URLs such as https://clinicaltrials.gov/ct2/show/NCT02703272.
#' The function is to be called by ctrGetQueryUrl(), which turns
#' search and display URLs into queries. See also
#' ./inst/tinytest/more_test_ctrdata_param_checks.R
#'
#' @param url url intended for a search in the classic CTGOV website
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom countrycode countrycode
#' @importFrom utils URLdecode
#'
#' @returns string url suitable for a search current CTGOV website
#'
#' @examples
#'
#' ctgovClassicToCurrent("https://www.clinicaltrials.gov/search?term=NCT04412252,%20NCT04368728")
#' ctgovClassicToCurrent("https://classic.clinicaltrials.gov/ct2/results?cond=&term=NCT02703272&cntry=")
#' ctgovClassicToCurrent("https://clinicaltrials.gov/ct2/results?cond=&term=NCT02703272&cntry=")
#' ctgovClassicToCurrent("https://www.clinicaltrials.gov/search?distance=50&cond=Cancer")
#' ctgovClassicToCurrent("https://classic.clinicaltrials.gov/ct2/results?term=AREA[MaximumAge]+RANGE[0+days,+28+days]")
#'
ctgovClassicToCurrent <- function(url, verbose = TRUE) {

  # apiParams is a kind of dictionary for
  # mapping classic to current params
  #
  # - not matched:
  #   CTGOV2 studyComp
  #   CTGOV dist
  #   CTGOV rsub
  #
  apiParams <- list(
    #
    # start aggFilters
    #
    "ages:" = list(
      "extract" = c(
        "age=0(&|$)",
        "age=1(&|$)",
        "age=2(&|$)"
      ),
      "replace" = c(
        "child",
        "adult",
        "older"
      ),
      "collapse" = " ",
      "out" = character()
    ),
    #
    "phase:" = list(
      "extract" = c(
        "phase=4(&|$)",
        "phase=0(&|$)",
        "phase=1(&|$)",
        "phase=2(&|$)",
        "phase=3(&|$)"),
      "replace" = c(
        "0",
        "1",
        "2",
        "3",
        "4"),
      "collapse" = " ",
      "out" = character()
    ),
    #
    "docs:" = list(
      "extract" = c(
        "u_prot=Y(&|$)",
        "u_sap=Y(&|$)",
        "u_icf=Y(&|$)"),
      "replace" = c(
        "prot",
        "sap",
        "icf"),
      "collapse" = " ",
      "out" = character()
    ),
    #
    "results:" = list(
      "extract" = c(
        "rslt=With(&|$)",
        "rslt=Without(&|$)"),
      "replace" = c(
        "with",
        "without"),
      "collapse" = " ",
      "out" = character()
    ),
    #
    "funderType:" = list(
      "extract" = c(
        "fund=[013]*[2][013]*(&|$)",
        "fund=[123]*[0][123]*(&|$)",
        "fund=[023]*[1][023]*(&|$)",
        "fund=[012]*[3][012]*(&|$)"),
      "replace" = c(
        "industry", # 2
        "nih",      # 0
        "fed",      # 1
        "other"),   # 3
      "collapse" = " ",
      "out" = character()
    ),
    #
    "studyType:" = list(
      "extract" = c(
        "type=Intr",
        "type=Obsr",
        "type=PReg",
        "type=Expn",
        "ea_tmt=Yes",
        "ea_idv=Yes",
        "ea_int=Yes"
      ),
      "replace" = c(
        "int", # Interventional
        "obs", # Observational
        "obs_patreg", # Patient registries
        "exp",        # Expanded access
        "exp_treat",  # Treatment IND/Protocol
        "exp_indiv",  # Individual patients
        "exp_inter"   # Intermediate-size population
      ),
      "collapse" = " ",
      "out" = character()
    ),
    #
    "sex:" = list(
      "extract" = c(
        "gndr=Female",
        "gndr=Male"
      ),
      "replace" = c(
        "f",
        "m"
      ),
      "collapse" = " ",
      "out" = character()
    ),
    #
    "healthy:" = list(
      "extract" = "hlth=Y",
      "replace" = "y",
      "collapse" = " ",
      "out" = character()
    ),
    #
    "violation:" = list(
      "extract" = "f801=Yes",
      "replace" = "y",
      "collapse" = " ",
      "out" = character()
    ),
    #
    "status:" = list(
      "extract" = c(
        "recrs=a",
        "recrs=d",
        "recrs=b",
        "recrs=e",
        "recrs=h",
        "recrs=f",
        "recrs=g",
        "recrs=i",
        "recrs=m",
        "recrs=c",
        "recrs=j",
        "recrs=k",
        "recrs=l"
      ),
      "replace" = c(
        "rec", # Recruiting
        "act", # Active, not recruiting
        "not", # Not yet recruiting
        "com", # Completed
        "ter", # Terminated
        "enr", # Enrolling by invitation
        "sus", # Suspended
        "wit", # Withdrawn
        "unk", # Unknown
        "ava", # Available
        "nla", # No longer available
        "tna", # Temporarily not available
        "afm"), # Approved for marketing
      "collapse" = " ",
      "out" = character()
    ),
    #
    # end aggFilters
    #
    # dates
    "dates" = list(
      "extract" = list(
        "strd_s=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "strd_e=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "prcd_s=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "prcd_e=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "sfpd_s=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "sfpd_e=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "rfpd_s=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "rfpd_e=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "lupd_s=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)",
        "lupd_e=([0-9]{2})/([0-9]{2})/([0-9]{4})(&|$)"
      ),
      "replace" = list(
        "start=\\3-\\1-\\2_",
        "start=_\\3-\\1-\\2",
        "primComp=\\3-\\1-\\2_",
        "primComp=_\\3-\\1-\\2",
        "firstPost=\\3-\\1-\\2_",
        "firstPost=_\\3-\\1-\\2",
        "resFirstPost=\\3-\\1-\\2_",
        "resFirstPost=_\\3-\\1-\\2",
        "lastUpdPost=\\3-\\1-\\2_",
        "lastUpdPost=_\\3-\\1-\\2"
      ),
      "collapse" = "@",
      "out" = list()
    ),
    #
    # translate simple terms
    list(
      "extract" = c(
        "(cond|city|id|intr|lead|locn|outc|spons|state|titles|term)=(.+)(&|$)",
        "(cntry)=(.+)(&|$)"
      ),
      "replace" = c(
        "&\\1=\\2",
        "&country=\\2"
      ),
      "collapse" = "",
      "out" = character()
    )
    #
  ) # apiParams

  ## now operate on the input

  # mangle input
  queryterm <- utils::URLdecode(url)
  queryterm <- gsub("[+]", " ", queryterm)

  # some specifics found by chance
  queryterm <- sub("[?&]recr=Open", "&recrs=b&recrs=a&recrs=c", queryterm)
  queryterm <- sub("[?&]recr=Closed", "&recrs=f&recrs=d&recrs=g&recrs=h&recrs=e&recrs=i&recrs=m&recrs=j&recrs=k&recrs=l", queryterm)

  # split and focus on parameters
  queryterm <- strsplit(queryterm, split = "[&?]")[[1]]
  queryterm <- queryterm[!grepl("^https://", queryterm)]
  queryterm <- queryterm[queryterm != ""]

  # iterate over API terms
  for (t in seq_along(queryterm)) {
    for (a in seq_along(apiParams)) {
      for (i in seq_along(apiParams[[a]][["extract"]])) {
        if (grepl(apiParams[[a]][["extract"]][[i]], queryterm[t])) {
          item <-
            sub(apiParams[[a]][["extract"]][[i]],
                apiParams[[a]][["replace"]][[i]],
                queryterm[t]
            )
          apiParams[[a]][["out"]] <-
            paste0(
              c(apiParams[[a]][["out"]], item),
              collapse = apiParams[[a]][["collapse"]]
            )
        } # if extract
      } # extract
    } # apiParams
  } # queryterm

  # merge
  apiParams <- sapply(apiParams, "[[", "out")
  apiParams <- apiParams[lapply(apiParams, length) > 0L]

  # handle two dates parameters into one
  if (length(apiParams[["dates"]])) {
    tmpSplit <- strsplit(apiParams[["dates"]], "@", fixed = TRUE)[[1]]
    apiParams[["dates"]] <- ""
    for (t in unique(sub("(.+)=.+", "\\1", tmpSplit))) {
      apiParams[["dates"]] <- paste0(c(
        apiParams[["dates"]], paste0(
          t, "=", sub(
            "_+", "_",
            paste0(
              sub(".+=(.+)", "\\1", tmpSplit[grepl(t, tmpSplit)]),
              collapse = "_")),
          collapse = "")),
        collapse = "&")
    }}

  # handle parts within aggFilter
  for (t in seq_along(apiParams)) {
    if (grepl(":", names(apiParams[t]))) apiParams[t] <- paste0(
      names(apiParams[t]), paste0(
        unique(strsplit(apiParams[[t]], " ")[[1]]), collapse = " ")
    )
  }

  # merge other and aggFilter parts
  apiParams <- paste0(
    "https://clinicaltrials.gov/search?",
    paste0(
      unique(apiParams[!grepl(":", names(apiParams))]),
      collapse = ""),
    "&aggFilters=",
    paste0(
      unique(apiParams[grepl(":", names(apiParams))]),
      collapse = ",")
  )

  # handle country
  if (grepl("[?&]country=[^$&]", apiParams)) {

    countryCode <- sub(".+([?&]country=)([A-Z]+)([$&]).*", "\\2", apiParams)
    if (countryCode != apiParams) apiParams <-
        sub("([?&]country=)([A-Z]+)([$&])",
            paste0("\\1", countrycode::countrycode(
              countryCode, "iso2c", "iso.name.en"), "\\3"),
            apiParams)
  }

  # prettify
  apiParams <- gsub("&&", "&", apiParams)
  apiParams <- gsub("&aggFilters=$", "", apiParams)
  apiParams <- gsub("search[?]&", "search?", apiParams)

  ## inform user

  # inform user
  if (verbose) message(
    "Since 2024-06-25, the classic CTGOV servers are no longer available. ",
    "Package ctrdata has translated the classic CTGOV query URL from this ",
    "call of function ctrLoadQueryIntoDb(queryterm = ...) into a query URL ",
    "that works with the current CTGOV2. This is printed below and is also ",
    "part of the return value of this function, ctrLoadQueryIntoDb(...)$url. ",
    "This URL can be used with ctrdata functions. Note that the fields and ",
    "data schema of trials differ between CTGOV and CTGOV2. "
  )

  # inform user
  message(
    "\nReplace this URL:\n\n", url,
    "\n\nwith this URL:\n\n", apiParams, "\n")

  # return
  return(apiParams)

} # end ctgovClassicToCurrent


#' Check, write, read cache object for ctrdata
#'
#' @param xname name of variable to read or write
#'
#' @param xvalue value of variable to write
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @keywords internal
#' @noRd
#'
#' @return value of variable or `NULL` if variable does not exist
#'
ctrCache <- function(xname, xvalue = NULL, verbose = FALSE) {

  # hidden environment .ctrdataenv created in zzz.R

  # write or overwrite and exit early
  if (!is.null(xvalue)) {
    assign(x = xname, value = xvalue, envir = .ctrdataenv)
    if (verbose) message("- Wrote ", xname, " to cache ")
    return(xvalue)
  }

  # check and read any value for xname variable
  if (verbose) message("- Checking cache...")
  if (exists(x = xname, envir = .ctrdataenv)) {
    tmp <- try(get(x = xname, envir = .ctrdataenv), silent = TRUE)
    if (inherits(tmp, "try-error")) return(NULL)
    if (verbose) message("- Returning ", xname, " ")
    return(tmp)
  }

  # default
  return(NULL)
}


#' Check and prepare nodbi connection object for ctrdata
#'
#' @param con A database connection object, created with
#' \code{nodbi}. See section `1 - Database connection` in
#' \link{ctrdata}.
#'
#' @keywords internal
#'
#' @importFrom nodbi src_sqlite src_duckdb docdb_list
#' @importFrom utils capture.output
#'
#' @return Connection object as list, with collection
#'  element under root
#'
ctrDb <- function(con) {

  ## ensure requirements
  if (!requireNamespace("nodbi", quietly = TRUE)) {
    stop("Install package 'nodbi' to use this function.",
         call. = FALSE)
  }
  minV <- sub(
    ".*nodbi[(<>=[:space:]]+([.0-9]+?)\\).*", "\\1",
    utils::packageDescription("ctrdata", fields = "Imports")
  )
  if (!(utils::packageVersion("nodbi") >=
        package_version(minV))) {
    stop("Update package 'nodbi' to version ",
         minV, " or later to use this function.",
         call. = FALSE)
  }

  ## check constructor
  if (!inherits(con, "docdb_src")) {
    stop(
      "Database connection object 'con' was not, ",
      "but should be created with nodbi.",
      call. = FALSE)
  }

  ## postgres
  if (inherits(con, "src_postgres")) {

    if (is.null(con$collection)) {
      stop(
        "Specify attribute 'collection' with a table name, using ",
        "<nodbi src_postgres object>[[\"collection\"]] <- \"test\"), ",
        "for package ctrdata to work.",
        call. = FALSE)
    }

    # add database as element under root
    con <- c(
      con,
      "db" = con$dbname,
      "ctrDb" = TRUE)

    ## return
    return(structure(
      con,
      class = c("src_postgres", "docdb_src")))
  }

  ## sqlite
  if (inherits(con, "src_sqlite")) {

    if (is.null(con$collection)) {
      stop(
        "Specify parameter 'collection' with a table name, ",
        "such as nodbi::src_sqlite(collection = 'test'), ",
        "for package ctrdata to work.",
        call. = FALSE)
    }

    # check
    if (inherits(try(nodbi::docdb_list(con), silent = TRUE), "try-error")) {
      con <- nodbi::src_sqlite(
        dbname = con$dbname,
        collection = con$collection)
    }

    # add database as element under root
    con <- c(
      con,
      "db" = con$dbname,
      "ctrDb" = TRUE)

    # print warning
    if (grepl(":memory:", con$dbname)) {
      warning(
        "Database not persisting",
        call. = FALSE, noBreaks. = FALSE)
    }

    ## return
    return(structure(
      con,
      class = c("src_sqlite", "docdb_src")))
  }

  ## mongo
  if (inherits(con, "src_mongo")) {

    # rights may be insufficient to call info(),
    # hence this workaround that should always
    # work and be stable to retrieve name of
    # collection in the mongo connection
    # suppress... for reconnect info from mongolite
    coll <- suppressMessages(utils::capture.output(con$con)[1])
    coll <- sub("^.*'(.*)'.*$", "\\1", coll)

    # add collection as element under root
    con <- c(
      con,
      "collection" = coll,
      "ctrDb" = TRUE)

    ## return
    return(structure(
      con,
      class = c("src_mongo", "docdb_src")))
  }

  ## duckdb
  if (inherits(con, "src_duckdb")) {

    if (is.null(con$collection)) {
      stop(
        "Specify parameter 'collection' with a table name, ",
        "such as nodbi::src_duckdb(collection = 'test'), ",
        "for package ctrdata to work.",
        call. = FALSE)
    }

    # check
    if (inherits(try(nodbi::docdb_list(con), silent = TRUE), "try-error")) {
      con <- nodbi::src_duckdb(
        dbdir = attr(attr(con$con, "driver"), "dbdir"),
        collection = con$collection)
    }

    # add database as element under root
    con <- c(
      con,
      "db" = attr(attr(con$con, "driver"), "dbdir"),
      "ctrDb" = TRUE)

    # print warning about nodbi::src_duckdb()
    if (grepl(":memory:", attr(attr(con$con, "driver"), "dbdir"))) {
      warning(
        "Database not persisting\n",
        call. = FALSE, noBreaks. = FALSE)

    }

    ## return
    return(structure(
      con,
      class = c("src_duckdb", "docdb_src")))

  }

  ## unprepared for other nodbi adapters so far
  stop(
    "Please specify in parameter 'con' a database connection ",
    "created with nodbi::src_...() functions. ctrdata currently ",
    "supports src_mongo(), src_sqlite(), src_postgres() and src_duckdb().",
    call. = FALSE)

} # end ctrDb



#' Change type of field based on name of field
#'
#' @param dv a vector of character strings
#'
#' @param fn a field name
#'
#' @return a typed vector, same length as dv
#'
#' @importFrom xml2 xml_text read_html
#' @importFrom lubridate duration ymd_hms dyears dmonths ddays
#'
#' @keywords internal
#' @noRd
#'
typeField <- function(dv, fn) {

  # get function name
  ft <- typeVars[[fn]]

  # expand to function
  if (!is.null(ft)) ft <- switch(
    typeVars[[fn]],
    "ctrFactor" = "as.factor(x = x)",
    "ctrInt" = "as.integer(x = x)",
    "ctrIntList" = 'sapply(x, function(i) {i[i == "NA"] <- NA; as.integer(i)}, USE.NAMES = FALSE)',
    "ctrYesNo" = 'sapply(x, function(i) if (is.na(i)) NA else
       switch(i, "Yes" = TRUE, "No" = FALSE, NA), simplify = TRUE, USE.NAMES = FALSE)',
    "ctrFalseTrue" = 'if (is.numeric(x)) as.logical(x) else
       sapply(x, function(i) switch(tolower(i), "true" = TRUE, "false" = FALSE, NA), USE.NAMES = FALSE)',
    "ctrDate" = 'as.Date(x, tryFormats =
       c("%Y-%m-%d", "%Y-%m", "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
    "%d/%m/%Y", "%Y-%m-%dT%H:%M:%S%z"))',
    "ctrDateUs" = 'as.Date(x, tryFormats = c("%b %e, %Y", "%Y-%m-%d", "%Y-%m"))',
    "ctrDateTime" = "lubridate::ymd_hms(x)",
    "ctrDifftime" = 'as.difftime(as.numeric(lubridate::duration(
       tolower(x)), units = "days"), units = "days")',
    "ctrDifftimeDays" = "lubridate::ddays(x = as.numeric(x))",
    "ctrDifftimeMonths" = "lubridate::dmonths(x = as.numeric(x))",
    "ctrDifftimeYears" = "lubridate::dyears(x = as.numeric(x))",
    NULL
  )

  # clean up text
  if (is.null(ft)) {

    # - if NA or similar is a string, change to NA
    if (typeof(dv) == "character") dv[grepl("^N/?A$|^ND$", dv)] <- NA_character_

    # - check if any html entities
    htmlEnt <- grepl("&[#a-zA-Z]+;", dv)

    # - convert html entities to text and symbols
    if (any(htmlEnt) && all(sapply(dv, typeof) == "character")) {
      dv[htmlEnt] <-
        lapply(dv[htmlEnt], function(i) {
          sapply(i, function(ii) {
            xml2::xml_text(xml2::read_html(charToRaw(ii)))
          }, USE.NAMES = FALSE)
        })
    }

    # - check if conversion to numeric works
    if ((typeof(dv) == "character") && any(!is.na(dv))) {
      dvn <- suppressWarnings(as.numeric(dv))
      if (identical(is.na(dv), is.na(dvn))) return(dvn)
    }

    # - collapse unless list structure is heterogenous
    rowN1 <- sapply(dv, function(i) is.null(names(i)))
    rowN2 <- sapply(names(rowN1), function(i) is.null(i))
    rowType <- sapply(dv, function(i) typeof(unlist(i, recursive = FALSE)))
    #
    if (all(rowN1) &&
        all(rowN2) &&
        length(unique(rowN1)) <= 1L &&
        any(rowType == "character")) {
      #
      dv <- sapply(dv, function(i) {
        i <- gsub("\r", "\n", i)
        i <- sub("^Information not present in EudraCT", "", i)
        if (length(i) > 1L) {
          rowI <- paste0(i[!is.na(i)], collapse = " / ")
          if (nchar(rowI)) rowI else NA_character_
        } else {
          if (length(i) && !is.na(i)) i else NA_character_
        }
      })
    }

    # early return
    return(dv)

  }

  # early exit if already date or logical
  if (all(sapply(dv, class) %in%
          c("logical", "Date", "POSIXct", "POSIXt"))) return(dv)

  # record length of input dv for NULL handling
  lenDv <- length(dv)

  # apply typing function, returning
  # if possible a vector over list
  tryCatch(
    expr = {
      dv <- lapply(dv, function(x) {
        # - text mangling
        x <- ifelse(grepl("Information not present in EudraCT", x), NA, x)
        # - give Month Year a Day to allow conversion
        if (grepl("date", fn, ignore.case = TRUE)) {
          x <- sub("^ClinicalTrials.gov processed this data on ", "", x)
          x <- sub("^([a-zA-Z]+) ([0-9]{4})$", "\\1 15, \\2", x)
          x <- sub("^([0-9]{4}-[0-9]{2})$", "\\1-15", x)
        }
        # - apply function to x
        eval(parse(text = ft))
      })
    },
    error = function(e) {
      message(fn, ": returning untyped values, as ",
              ft, " raised an error when applied to ",
              paste0(unlist(dv), collapse = " / "))
      return(dv)
    },
    warning = function(w) {
      message(fn, ": returning untyped values, as ",
              ft, " raised a warning when applied to ",
              paste0(unlist(dv), collapse = " / "))
      return(dv)
    }
  )

  # exceptional case inform user
  if (is.null(dv)) {
    warning(paste0(
      fn, " could not be typed, please report here: ",
      "https://github.com/rfhb/ctrdata/issues"))
    dv <- rep_len(NA, lenDv)
  }

  # make original classes (e.g., Date) reappear
  if (!is.list(dv)) dv <- as.list(dv)
  if (all(sapply(dv, length) <= 1L)) {
    return(do.call("c", dv))
  }

  # return
  return(dv)

} # end typeField



#' Annotate ctrdata function return values
#'
#' @param x object to be annotated
#'
#' @inheritParams ctrDb
#'
#' @keywords internal
#' @noRd
#'
addMetaData <- function(x, con) {

  # add metadata
  attr(x, "ctrdata-dbname")         <- con$db
  attr(x, "ctrdata-collection")     <- con$collection
  attr(x, "ctrdata-dbqueryhistory") <- dbQueryHistory(con)

  # return annotated object
  return(x)

} # end addMetaData



#' ctrMultiDownload
#'
#' @param urls Vector of urls to be downloaded
#' @param destfiles Vector of local file names into which to download
#' @param progress Set to \code{FALSE} to not print progress bar
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @keywords internal
#' @noRd
#'
#' @return Data frame with columns such as status_code etc
#'
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_throttle req_perform_parallel
#' @importFrom dplyr left_join
#'
ctrMultiDownload <- function(
    urls,
    destfiles,
    progress = TRUE,
    verbose = TRUE) {

  stopifnot(length(urls) == length(destfiles))
  if (!length(urls)) return(data.frame())

  # starting values
  numI <- 1L

  # do not again download files that already exist
  # or that do not have an (arbitrary) minimal size.
  # nchar("Request failed.") is 15L
  toDo <- rep.int(TRUE, times = length(urls))
  toDo[file.exists(destfiles) &
         (is.na(file.size(destfiles)) |
            file.size(destfiles) > 20L)] <- FALSE

  downloadValue <- data.frame(
    "success" = !toDo,
    "destfile" = destfiles,
    "url" = utils::URLencode(urls),
    "status_code" = rep.int(NA_integer_, length(toDo)),
    "urlResolved" = rep.int(NA_character_, length(toDo)),
    "content_type" = rep.int(NA_character_, length(toDo))
  )

  # remove any duplicates
  downloadValue <- unique(downloadValue)

  # does not error in case any of the individual requests fail.
  # inspect the return value to find out which were successful
  # make no more than 3 attempts to complete downloading
  while (any(toDo) && numI < 3L) {

    # use urlResolved if this has been filled below in CDN check
    downloadValue$url[!is.na(downloadValue$urlResolved)] <-
      downloadValue$urlResolved[!is.na(downloadValue$urlResolved)]

    # do download
    res <- httr2::req_perform_parallel(
      reqs = lapply(
        downloadValue$url,
        function(i) httr2::req_throttle(
          httr2::request(i),
          # TODO hard-coded throttling, max 4 MB/s
          # ensures that you never make more
          # than capacity requests in fill_time_s
          capacity = 20L * 10L,
          fill_time_s = 10L
        )),
      paths = downloadValue$destfile,
      on_error = "continue",
      progress = progress,
      max_active = 10L
    )

    # mangle results info
    res <- lapply(
      res,
      function(r) {
        if (inherits(r, "httr2_failure")) return(
          data.frame(
            "success" = FALSE,
            "status_code" = NA_integer_,
            "url" = r$request$url,
            "destfile" = NA_character_,
            "content_type" = NA_character_,
            "urlResolved" = NA_character_
          )
        )
        if (inherits(r, "httr2_error")) return(
          data.frame(
            "success" = FALSE,
            "status_code" = r$status,
            "url" = r$request$url,
            "destfile" = as.character(r$resp$body),
            "content_type" = NA_character_,
            "urlResolved" = NA_character_
          )
        ) # else
        return(
          data.frame(
            "success" = TRUE,
            "status_code" = r[["status_code"]],
            "url" = r[["url"]],
            "destfile" = as.character(r[["body"]]),
            "content_type" = r[["headers"]]$`content-type`,
            "urlResolved" = NA_character_
          )
        )})
    res <- as.data.frame(do.call(rbind, res))

    # check if download successful but CDN is likely used
    cdnCheck <- !is.na(res$status_code) &
      !is.na(res$destfile) &
      !is.na(res$content_type) &
      (res$status_code %in% c(200L, 206L, 416L)) &
      !grepl("[.]json$", res$destfile) &
      grepl("application/json", res$content_type)

    # replace url with CDN url and prepare to iterate
    if (any(cdnCheck)) {

      message("Redirecting to CDN...")

      # get CDN url
      res$urlResolved[cdnCheck] <- sapply(
        res$destfile[cdnCheck],
        # x can be a JSON string, URL or file
        function(x) jsonlite::fromJSON(x)$url,
        USE.NAMES = FALSE,
        simplify = TRUE)

      # remove files containing CDN url
      unlink(res$destfile[cdnCheck])

      # reset status
      res$status_code[cdnCheck] <- NA

    }

    # update input, mind row order
    downloadValue <- dplyr::rows_update(
      downloadValue,
      # do not include destfile as this may be NA in res
      res[, c("success", "status_code", "url", "content_type",
              "urlResolved"), drop = FALSE],
      by = "url"
    )

    if (inherits(downloadValue, "try-error")) {
      stop("Download failed; last error: ", class(downloadValue), call. = FALSE)
    }

    toDoThis <- is.na(downloadValue$success) |
      is.na(downloadValue$status_code) |
      !downloadValue$success |
      !(downloadValue$status_code %in% c(200L, 206L, 416L))

    # only count towards repeat attempts if
    # the set of repeated urls is unchanged
    if (identical(toDo, toDoThis)) numI <- numI + 1L

    toDo <- toDoThis

  }

  if (any(toDo)) {

    # remove any files from failed downloads
    unlink(downloadValue[toDo, c("destfile"), drop = TRUE])

    if (verbose) {
      message(
        "Download failed for: status code / url(s):"
      )
      apply(
        downloadValue[toDo, c("status_code", "url"), drop = FALSE],
        1, function(r) message(r[1], " / ", r[2], "\n", appendLF = FALSE)
      )
    }

  }

  return(downloadValue[!toDo, , drop = FALSE])

} # end ctrMultiDownload



#' ctrTempDir
#'
#' create empty temporary directory on localhost for
#' downloading from register into temporary directory
#'
#' @return path to existing directory
#'
#' @keywords internal
#' @noRd
#'
ctrTempDir <- function(verbose = FALSE) {

  # get temporary space
  tempDir <- getOption(
    "ctrdata.tempdir",
    default = tempfile(pattern = "ctrDATA"))

  # create and normalise for OS
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  tempDir <- normalizePath(tempDir, mustWork = TRUE)

  # retain tempdir for session to accelerate,
  # but only if session is user-interactive.
  # from ctrdata 1.16.0.9000 onwards, all
  # intermediate files are deleted before
  # finalising a ctrLoadQueryIntoDb() call
  # (that is, only downloaded files are kept).
  if (interactive()) options(ctrdata.tempdir = tempDir)

  # register deleting tempDir when exiting session
  assign("keeptempdir", verbose, envir = .ctrdataenv)
  delCtrdataTempDir <- function(x) {
    if (length(.ctrdataenv$keeptempdir) &&
        !is.null(.ctrdataenv$keeptempdir)) {
      if (.ctrdataenv$keeptempdir) {
        message(
          'ctrdata: "verbose = TRUE", not deleting temporary directory ', tempDir, "\r")
      } else {
        unlink(tempDir, recursive = TRUE)
        message("ctrdata: deleted temporary directory\r")
      }
    }
    assign("keeptempdir", NULL, envir = .ctrdataenv)
  }
  reg.finalizer(
    e = .ctrdataenv,
    f = delCtrdataTempDir,
    onexit = TRUE
  )

  # inform user
  if (verbose) message(
    "\nDEBUG: ", tempDir,
    "\nUsing any previously downloaded files of the ",
    length(dir(path = tempDir)),
    " files existing in this folder.\n")

  # return
  return(tempDir)

}



#' ctrDocsDownload
#'
#' download documents
#'
#' @param dlFiles data frame with columns _id, filename, url
#' @param documents.path parameter from parent call
#' @param documents.regexp parameter from parent call
#' @param multiplex use http/2 or not
#' @param verbose print parameter from parent call
#'
#' @return number of documents
#'
#' @keywords internal
#' @noRd
#'
ctrDocsDownload <- function(
    dlFiles,
    documents.path,
    documents.regexp,
    multiplex = TRUE,
    verbose) {

  # check and create directory
  createdDir <- try(
    dir.create(documents.path, recursive = TRUE, showWarnings = FALSE),
    silent = TRUE)

  # early return
  if (inherits(createdDir, "try-errror")) {

    warning("Directory could not be created for 'documents.path' ",
            documents.path, ", cannot download files", call. = FALSE)

    return(0L)
  }

  # continue after if
  message("- Downloading documents into 'documents.path' = ", documents.path)

  # canonical directory path
  documents.path <- normalizePath(documents.path, mustWork = TRUE)
  if (createdDir) message("- Created directory ", documents.path)

  # add destination file directory path
  dlFiles$filepath <- file.path(documents.path, dlFiles$`_id`)

  # create full filepathname
  dlFiles$filepathname <- file.path(dlFiles$filepath, dlFiles$filename)

  # check if destination document exists
  dlFiles$fileexists <- file.exists(dlFiles$filepathname) &
    file.size(dlFiles$filepathname) > 20L

  # placeholder or files
  if (is.null(documents.regexp)) {

    message("- Creating empty document placeholders (max. ", nrow(dlFiles), ")")

    # create subdirectories by trial
    invisible(sapply(
      unique(dlFiles$filepath), function(i) if (!dir.exists(i))
        dir.create(i, showWarnings = FALSE, recursive = TRUE)
    ))

    # create empty files
    tmp <-
      sapply(
        dlFiles$filepathname,
        function(i) if (!file.exists(i))
          file.create(i, showWarnings = TRUE),
        USE.NAMES = FALSE)

    tmp <- sum(unlist(tmp), na.rm = TRUE)

  } else {

    # inform
    message("- Applying 'documents.regexp' to ",
            nrow(dlFiles), " missing documents")

    # apply regexp
    dlFiles <- dlFiles[
      grepl(documents.regexp, dlFiles$filename, ignore.case = TRUE), ,
      drop = FALSE]

    # documents download
    message("- Creating subfolder for each trial")

    # create subdirectories by trial
    invisible(sapply(
      unique(dlFiles$filepath), function(i) if (!dir.exists(i))
        dir.create(i, showWarnings = FALSE, recursive = TRUE)
    ))

    # inform
    message("- Downloading ",
            nrow(dlFiles[!dlFiles$fileexists, , drop = FALSE]),
            " missing documents " , appendLF = FALSE)

    # check and remove duplicate filepathname rows
    duplicateFiles <- duplicated(tolower(dlFiles$filepathname))
    if (any(duplicateFiles)) {
      message(
        "(excluding ", sum(duplicateFiles), " ",
        "files with duplicate names for saving, e.g. ",
        paste0(
          sample(
            dlFiles$filepathname[duplicateFiles],
            min(length(dlFiles$filepathname[duplicateFiles]), 3L)),
          collapse = ", "),
        ") ", appendLF = FALSE)
      dlFiles <- dlFiles[!duplicateFiles, , drop = FALSE]
    }
    message()

    # do download
    tmp <- ctrMultiDownload(
      urls = dlFiles$url[!dlFiles$fileexists],
      destfiles = dlFiles$filepathname[!dlFiles$fileexists],
      verbose = verbose
    )

    # check results
    if (!nrow(tmp)) tmp <- 0L else {

      # handle failures despite success is true
      suppressMessages(invisible(sapply(
        tmp$destfile[!tmp$success],
        # delete but only micro files, possible remnants
        function(f) if (file.size(f) < 20L) unlink(f)
      )))
      tmp <- sum(tmp$success, na.omit = TRUE)

    }

  } # is.null(documents.regexp)

  # inform user
  message(sprintf(paste0(
    "= Newly saved %i ",
    ifelse(is.null(documents.regexp), "placeholder ", ""),
    "document(s) for %i trial(s); ",
    "%i of such document(s) for %i trial(s) already existed in %s"),
    tmp,
    length(unique(dlFiles$`_id`)),
    sum(dlFiles$fileexists),
    length(unique(dlFiles$`_id`[dlFiles$fileexists])),
    documents.path
  ))

  # return
  return(tmp)

} # end ctrDocsDownload



#' initTranformers
#'
#' https://cran.r-project.org/web/packages/V8/vignettes/npm.html
#'
#' @importFrom V8 v8 JS
#' @importFrom readr read_file
#'
#' @keywords internal
#' @noRd
#'
initTranformers <- function() {

  # prepare V8, see ./inst/js/
  ct <- V8::v8()

  # get javascript for xml to ndjson
  ct$source(system.file("js/bundle.js", package = "ctrdata"))

  # function for xml to ndjson conversion
  ct$assign(
    "parsexml",
    # https://www.npmjs.com/package/xml2js#options
    V8::JS("function(xml, opts) {injs.parseString(xml, opts, function (err, result)
           { out = result; }); return JSON.stringify(out); }"))

  # native javascript function for euctr txt to ndjson conversion
  ct$eval(readr::read_file(system.file("js/euctr2ndjson.js", package = "ctrdata")))

  # assign into package private environment, see zzz.R
  assign("ct", ct, envir = .ctrdataenv)

}



#' dbCTRLoadJSONFiles
#'
#' @param dir Path to local directory with JSON files
#' from downloading and converting
#'
#' @importFrom jsonlite validate
#' @importFrom nodbi docdb_create
#' @importFrom stats na.omit
#' @importFrom jqr jq
#'
#' @inheritParams ctrDb
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @return List with elements n (number of imported trials),
#' _id's of successfully imported trials and
#' _id's of trials that failed to import
#'
#' @keywords internal
#' @noRd
#'
dbCTRLoadJSONFiles <- function(dir, con, verbose) {

  # find files
  tempFiles <- dir(path = dir,
                   pattern = "^.+_trials_.*.ndjson$",
                   full.names = TRUE)

  # check
  if (!length(tempFiles)) stop("no .+_trials_.*.ndjson files found in ", dir)

  # initialise counters
  fc <- length(tempFiles)

  ## iterate ndjson files -----------------------------------------------------------------

  retimp <- lapply(
    X = seq_along(tempFiles),
    function(tempFile) {

      ## initialise output
      idSuccess <- NULL
      idFailed <- NULL
      idAnnotation <- NULL
      nImported <- 0
      ids <- NULL

      ## get _id's

      # inform user
      message(
        "JSON file #: ", tempFile, " / ", fc,
        "                               \r",
        appendLF = FALSE)

      # get all ids using jq, safet than regex
      ids <- gsub("\"", "", as.vector(jqr::jq(file(tempFiles[tempFile]), " ._id ")))

      ## existing annotations -------------------------------------------------

      # get annotations
      annoDf <- try({
        nodbi::docdb_query(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), "]}}"),
          fields = '{"_id": 1, "annotation": 1}')
      }, silent = TRUE)
      if (!inherits(annoDf, "try-error") && length(annoDf[["_id"]])) {
        annoDf <- merge(
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE),
          annoDf, all.x = TRUE) # only need input ids, do not need all.y
      } else {
        annoDf <-
          data.frame("_id" = ids, check.names = FALSE, stringsAsFactors = FALSE)
      }
      if (is.null(annoDf[["annotation"]]))
        annoDf[["annotation"]] <- rep(NA, length(ids))

      ## delete and import ----------------------------------------------------

      # delete any existing records
      tmp <- try({
        nodbi::docdb_delete(
          src = con,
          key = con$collection,
          query = paste0(
            '{"_id": {"$in": [',
            paste0('"', ids, '"', collapse = ","), ']}}'))
      }, silent = TRUE)

      # early exit
      if (inherits(tmp, "try-error") &&
          grepl("read.?only", tmp)) stop(
            "Database is read-only, cannot load trial records.\n",
            "Change database connection in parameter 'con = ...'",
            call. = FALSE
          )

      ## import
      tmp <- try({
        suppressWarnings(
          suppressMessages(
            nodbi::docdb_create(
              src = con,
              key = con$collection,
              value = tempFiles[tempFile]
            )))}, silent = TRUE)

      ## return values for lapply
      if (inherits(tmp, "try-error") || tmp == 0L || tmp != nrow(annoDf)) {

        # step into line by line mode
        fdLines <- file(tempFiles[tempFile], open = "rt", blocking = TRUE)
        while (TRUE) {
          tmpOneLine <- readLines(con = fdLines, n = 1L, warn = FALSE)
          if (length(tmpOneLine) == 0L || !nchar(tmpOneLine)) break
          id <- sub(".*\"_id\":[ ]*\"(.*?)\".*", "\\1", tmpOneLine)
          tmp <- suppressWarnings(suppressMessages(nodbi::docdb_create(
            src = con, key = con$collection, value = paste0("[", tmpOneLine, "]"))))
          nImported <- nImported + tmp
          if (tmp) idSuccess <- c(idSuccess, id)
          if (!tmp) idFailed <- c(idFailed, id)
          if (!tmp) warning("Failed to load: ", id, call. = FALSE)
          if (tmp) idAnnotation <- c(idAnnotation, annoDf[
            annoDf[["_id"]] == id, "annotation", drop = TRUE][1])
        }
        close(fdLines)

      } else {
        nImported <- nImported + tmp
        idSuccess <- c(idSuccess, annoDf[, "_id", drop = TRUE])
        idAnnotation <- c(idAnnotation, annoDf[, "annotation", drop = TRUE])
      }

      # return values
      list(success = idSuccess,
           failed = idFailed,
           n = nImported,
           annotations = idAnnotation)

    }) # sapply tempFiles

  # prepare return values, n is successful only
  n <- sum(sapply(retimp, "[[", "n"), na.rm = TRUE)
  success <- as.vector(unlist(sapply(retimp, "[[", "success")))
  failed <- as.vector(unlist(sapply(retimp, "[[", "failed")))
  annotations <- as.vector(unlist(sapply(retimp, "[[", "annotations")))

  # return
  return(list(n = n,
              success = success,
              failed = failed,
              annotations = annotations))

} # end dbCTRLoadJSONFiles


#' dbQueryAnnotateRecords
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_update
#'
dbCTRAnnotateQueryRecords <- function(
    recordnumbers,
    recordannotations,
    annotation.text,
    annotation.mode,
    con,
    verbose) {

  # debug
  if (verbose) message("Annotating records...")
  if (verbose) message(recordnumbers)
  if (verbose) message(annotation.mode)

  # df from existing annotations
  if (is.null(recordannotations)) recordannotations <- ""
  annotations <- data.frame(
    "_id" = recordnumbers,
    "annotation" = recordannotations,
    stringsAsFactors = FALSE,
    check.names = FALSE)

  # check if dataframe is as expected: columns _id and annotation
  # dataframe could be empty if _ids not yet imported
  if (nrow(annotations) == 0) {
    annotations <- data.frame("_id" = recordnumbers,
                              "annotation" = "",
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  }

  # modify the annotations
  annotations[["annotation"]] <- trimws(
    switch(
      annotation.mode,
      "replace" = paste0(annotation.text),
      "prepend" = paste0(annotation.text, " ", ifelse(
        is.na(annotations[["annotation"]]), "", annotations[["annotation"]])),
      paste0(ifelse(is.na(annotations[["annotation"]]), "", annotations[["annotation"]]),
             " ", annotation.text)
    ))

  # ensure columns including order
  annotations <- annotations[, c("_id", "annotation"), drop = FALSE]

  # debug
  if (verbose) message(annotations)

  # update the database
  result <- nodbi::docdb_update(
    src = con,
    key = con$collection,
    value = annotations,
    query = "{}")

  # inform user
  message("= Annotated retrieved records (", result, " records)")

} # end dbCTRAnnotateQueryRecords


#' dbCTRUpdateQueryHistory
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jsonlite toJSON
#' @importFrom nodbi docdb_delete docdb_create docdb_update
#'
dbCTRUpdateQueryHistory <- function(
    register,
    queryterm,
    recordnumber,
    con,
    verbose) {

  ## check database connection
  con <- ctrDb(con)

  # debug
  if (verbose) message("Running dbCTRUpdateQueryHistory...")

  # compose history entry from current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  newHist <- data.frame(
    "query-timestamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "query-register"  = register,
    "query-records"   = recordnumber,
    "query-term"      = queryterm,
    check.names = FALSE,
    stringsAsFactors = FALSE)

  # retrieve existing history data
  hist <- dbQueryHistory(con, verbose)

  # append current search
  # default for format methods is "%Y-%m-%d %H:%M:%S"
  if (!is.null(hist) &&
      nrow(hist)) {

    newHist <- rbind(hist, newHist)
    newHist <- list("queries" = newHist)

    tmp <- suppressMessages(
      nodbi::docdb_update(
        src = con,
        key = con$collection,
        value = newHist,
        query = '{"_id": "meta-info"}'
      ))

  } else {

    # to list
    newHist <- list(list(
      "_id" = "meta-info",
      "queries" = newHist))

    # write new document
    tmp <- suppressMessages(
      nodbi::docdb_create(
        src = con,
        key = con$collection,
        value = newHist
      ))
  }

  # inform user
  if (tmp == 1L) {
    message('Updated history ("meta-info" in "', con$collection, '")')
  } else {
    warning('Could not update history ("meta-info" in "', con$collection,
            '")', call. = FALSE, immediate. = FALSE)
  }
} # end dbCTRUpdateQueryHistory


#' dfOrTibble
#'
#' @return tibble or data frame, depending on loaded packages
#'
#' @param df data frame input
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom tibble as_tibble
#'
dfOrTibble <- function(df) {

  if (any(sapply(
    .packages(), function(i)
      any(i == c("tibble", "magrittr", "tidyr", "dplyr")))
  )) {

    return(tibble::as_tibble(df))

  } else {

    return(df)

  }

} # end dfOrTibble


#' fctChkFlds
#'
#' Calls for its side effect to stop if arguments
#' are not conforming to expectations (flds needs
#' to be a subset of dfFlds)
#'
#' @param dfFlds names of fields of a data frame
#' @param flds fields needed for a function
#'
#' @keywords internal
#' @noRd
#'
fctChkFlds <- function(dfFlds, flds) {

  flds <- unlist(flds, use.names = FALSE)

  flds <- flds[!sapply(flds, function(i) any(i == dfFlds))]

  if (length(flds)) stop(
    "Fields missing in 'df':\n", paste0(flds, "\n"),
    call. = FALSE)

  return(invisible(NULL))

}
