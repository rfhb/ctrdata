### ctrdata package

#' Find names of fields in the database collection
#'
#' Given part of the name of a field of interest to the user, this
#' function returns the full field names used in records that were
#' previously loaded into a collection
#' (using \link{ctrLoadQueryIntoDb}). The field names can be fed
#' into function \link{dbGetFieldsIntoDf} to extract the data
#' from the collection into a data frame.
#'
#' In addition to the full names of all child fields (e.g.,
#' \code{clinical_results.outcome_list.outcome.measure.class_list.class.title})
#' this function may return names of parent fields (e.g.,
#' \code{clinical_results}).
#' Data in parent fields is typically complex (nested) and can be
#' converted into individual data elements with \link{dfTrials2Long},
#' and subelements can then be accessed with \link{dfName2Value}.
#' For field definitions of the registers, see row
#' "Definition" in \link{ctrdata-registers}.
#' Note: Only when \code{dbFindFields} is first called after
#' \link{ctrLoadQueryIntoDb}, it will take a moment.
#'
#' @param namepart A character string (can include a regular expression,
#' including Perl-style) to be searched among all field names (keys)
#' in the collection, case-insensitive. Use ".*" to find all fields.
#'
#' @param verbose If \code{TRUE}, prints additional information
#' (default \code{FALSE}).
#'
#' @importFrom nodbi docdb_query
#'
#' @inheritParams ctrDb
#'
#' @return Vector of strings with full names of field(s) found,
#' ordered by register and alphabet. Names of the vector elements
#' are the register names for the respective fields.
#'
#' Only names of fields that have a value in the collection
#' are returned, and the names may be incomplete because they
#' are from sampled records. See here for obtaining a complete
#' set of field names (albeit without register names):
#' \url{https://github.com/rfhb/ctrdata/issues/26#issuecomment-1751452462}
#'
#' @export
#'
#' @examples
#'
#' dbc <- nodbi::src_sqlite(
#'     dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'     collection = "my_trials"
#' )
#'
#' dbFindFields(namepart = "date", con = dbc)
#'
dbFindFields <- function(namepart = "",
                         con,
                         verbose = FALSE) {
  ## sanity checks
  if (!is.atomic(namepart)) stop("'namepart' should be atomic.", call. = FALSE)
  if (length(namepart) > 1) stop("'namepart' should have one element.", call. = FALSE)
  if (namepart == "") stop("Empty 'namepart' parameter.", call. = FALSE)

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  ## check if cache environment has entry for the database
  keyslist <- ctrCache(
    xname = paste0("keyslist_", con$db, "/", con$collection),
    verbose = verbose
  )

  ## get cache reference value
  cacheRef <- as.character(rev(unlist(try(nodbi::docdb_query(
    src = con, key = con$collection, query = '{"_id": "meta-info"}',
    fields = '{"queries.query-timestamp": 1}'
  ), silent = TRUE)))[1])

  ## invalidate cache
  cacheOutdated <- is.null(keyslist) || (cacheRef != ctrCache(
    xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
    verbose = verbose
  ))

  ## get keyslist
  if (cacheOutdated) {
    # inform user
    message("Finding fields in database collection (may take some time)...")

    # helpder function
    getNodes <- function(fn) {
      nodesList <- strsplit(fn, split = ".", fixed = TRUE)
      nodesList <- sapply(nodesList, function(i) {
        i <- i[-length(i)]
        sapply(
          seq_along(i),
          function(ii) paste0(i[1:ii], collapse = "."),
          USE.NAMES = FALSE
        )
      }, USE.NAMES = FALSE)
      return(unique(unlist(nodesList)))
    }

    # helper function
    normNames <- function(df) {
      out <- names(unlist(df))
      if (!length(out)) {
        return("")
      }
      out <- ifelse(
        # exception for euctr protocol and results fields
        test = grepl("65To84|Over85|under_18", out),
        yes = out,
        no = sub("[0-9]+$", "", out)
      )
      out <- c(out, getNodes(out))
      return(sort(unique(out)))
    }

    # queries to be used
    queries <- list(
      "EUCTR" = c(
        '{"trialInformation.analysisStage.value": {"$regex": ".+"}}',
        '{"_id": {"$regex": "-[A-Z][A-Z]$"}}',
        '{"_id": {"$regex": "-3RD$"}}'
      ),
      "CTGOV" = c(
        '{"results_first_submitted": {"$regex": ".+"}}',
        '{"ctrname":"CTGOV"}'
      ),
      "CTGOV2" = c(
        '{"resultsFirstSubmitDate": {"$regex": ".+"}}',
        '{"ctrname":"CTGOV2"}'
      ),
      "ISRCTN" = c(
        '{"results.publicationStage": "Results"}',
        '{"ctrname":"ISRCTN"}'
      ),
      "CTIS" = c(
        '{"ctrname":"CTIS"}'
      )
    )

    # get names
    keyslist <- NULL
    # iterate over queries
    for (q in seq_along(queries)) {
      # iterate over query items
      for (i in seq_along(queries[[q]])) {
        # get record from register
        keysAdd <- normNames(nodbi::docdb_query(
          src = con, key = con$collection,
          query = queries[[q]][i], limit = 1L
        ))
        # give keys name of register
        names(keysAdd) <- rep(names(queries)[q], length(keysAdd))
        # accumulate keys
        keyslist <- c(keyslist, keysAdd)
      }
    }

    # clean empty entries and exclude _id for consistency
    # since different approaches above return _id or not
    keyslist <- keyslist[!duplicated(keyslist)]
    keyslist <- keyslist[keyslist != "_id" & keyslist != ""]
    keyslist <- sub("[.]$", "", keyslist)

    ## store keyslist to environment (cache)
    if (length(keyslist) > 1) {
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection),
        xvalue = keyslist, verbose = verbose
      )
      ctrCache(
        xname = paste0("keyslist_", con$db, "/", con$collection, "_timestamp"),
        xvalue = cacheRef, verbose = verbose
      )
      message("Field names cached for this session.")
    }
  } else {
    message("Using cache of fields.")
  } # generate keyslist

  ## inform user of unexpected situation
  if ((length(keyslist) == 0) || all(keyslist == "")) {
    warning("No keys could be extracted, please check database ",
            "and collection: ", con$db, "/", con$collection,
            call. = FALSE
    )
  }

  ## now do the actual search and find for key name parts
  fields <- keyslist[grepl(
    pattern = namepart, x = keyslist,
    ignore.case = TRUE, perl = TRUE
  )]

  # return value if no fields found
  if (!length(fields)) fields <- ""

  # return the match(es)
  return(fields)
} # end dbFindFields
