### ctrdata package

#' Get identifiers of deduplicated trial records
#'
#' Records for a clinical trial can be loaded from more than one
#' register into a collection. This function returns deduplicated
#' identifiers for all trials in the collection, respecting the
#' register(s) preferred by the user. All registers are recording
#' identifiers also from other registers, which are used by this
#' function to provide a vector of identifiers of deduplicated trials.
#'
#' Note that the content of records may differ between registers
#' (and, for "EUCTR", between records for different Member States).
#' Such differences are not considered by this function.
#'
#' @param preferregister A vector of the order of preference for
#' registers from which to generate unique _id's, default
#' \code{c("EUCTR", "CTGOV", "CTGOV2", "ISRCTN", "CTIS")}
#'
#' @param prefermemberstate Code of single EU Member State for which records
#' should returned. If not available, a record for DE or lacking this, any
#' random Member State's record for the trial will be returned.
#' For a list of codes of EU  Member States, please see vector
#' \code{countriesEUCTR}. Specifying "3RD" will return the Third Country
#' record of trials, where available.
#'
#' @param include3rdcountrytrials A logical value if trials should be retained
#' that are conducted exclusively in third countries, that is, outside
#' the European Union. Ignored if \code{prefermemberstate} is set to "3RD".
#'
#' @param verbose If \code{TRUE}, prints out the fields of registers used to
#' find corresponding trial records
#'
#' @importFrom nodbi docdb_query
#' @importFrom stats setNames
#'
#' @inheritParams ctrDb
#'
#' @return A named vector with strings of keys (field "_id") of
#' records in the collection that represent unique trials, where
#' names correspond to the register of the record.
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
#' dbFindIdsUniqueTrials(con = dbc)[1:10]
#'
dbFindIdsUniqueTrials <- function(
    preferregister = c("EUCTR", "CTGOV", "CTGOV2", "ISRCTN", "CTIS"),
    prefermemberstate = "DE",
    include3rdcountrytrials = TRUE,
    con,
    verbose = FALSE) {
  # parameter checks
  if (!all(preferregister %in% registerList)) {
    stop("'preferregister' unknown: ", preferregister, call. = FALSE)
  }
  if (length(prefermemberstate) != 1L ||
      !any(prefermemberstate == countriesEUCTR)) {
    stop("'prefermemberstate' unknown: ", prefermemberstate, call. = FALSE)
  }
  # complete if preferregister does not have all
  preferregister <- unique(preferregister)
  preferregister <- union(preferregister, registerList)

  # objective: create a vector of database record identifiers (_id)
  # that represent unique records of clinical trials, based on user's
  # preferences for selecting the preferred from any multiple records

  ## check database connection
  if (is.null(con$ctrDb)) con <- ctrDb(con = con)

  # inform user
  message("Searching for duplicate trials... ")

  # fields for database query
  fields <- c(
    "ctrname",
    # euctr
    "a2_eudract_number",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a51_isrctn_international_standard_randomised_controlled_trial_number",
    "trialInformation.isrctnIdentifier",
    "a41_sponsors_protocol_code_number",
    # ctgov
    "id_info",
    # isrctn
    "externalRefs",
    "isrctn",
    # ctis
    "ctNumber",
    "eudraCtInfo.eudraCtCode",
    "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
    # ctgov2
    "protocolSection.identificationModule.nctId",
    "protocolSection.identificationModule.secondaryIdInfos.id",
    "protocolSection.identificationModule.nctIdAliases",
    "protocolSection.identificationModule.orgStudyIdInfo.id"
  )

  # check if cache environment has entry for the database
  listofIds <- ctrCache(
    xname = paste0("listofids_", con$db, "/", con$collection),
    verbose = FALSE
  )

  # get cache reference value
  cacheRef <- as.character(rev(unlist(try(nodbi::docdb_query(
    src = con, key = con$collection, query = '{"_id": "meta-info"}',
    fields = '{"queries.query-timestamp": 1}'
  ), silent = TRUE)))[1])

  # cache validity
  cacheOutdated <- is.null(listofIds) || (cacheRef != ctrCache(
    xname = paste0("listofids_", con$db, "/", con$collection, "_timestamp"),
    verbose = FALSE
  ))

  # inform user
  message("- Getting all trial identifiers...", appendLF = FALSE)

  # cache outdated
  if (cacheOutdated) {
    # inform user
    message("\b\b\b (may take some time)...", appendLF = FALSE)

    # get identifiers
    listofIds <- try(
      suppressMessages(suppressWarnings(
        dbGetFieldsIntoDf(
          fields = fields,
          con = con,
          verbose = FALSE
        )
      )),
      silent = TRUE
    )

    # error check
    if (inherits(listofIds, "try-error") ||
        !length(listofIds) || !nrow(listofIds)) {
      stop("No records found, check collection '", con$collection, "'",
           call. = FALSE
      )
    }

    # write cache entries
    ctrCache(
      xname = paste0("listofids_", con$db, "/", con$collection),
      xvalue = listofIds, verbose = FALSE
    )
    ctrCache(
      xname = paste0("listofids_", con$db, "/", con$collection, "_timestamp"),
      xvalue = cacheRef, verbose = FALSE
    )
  } # if outdated

  # inform user
  message("\b\b\b, ", nrow(listofIds), " found in collection")

  # copy attributes
  attribsids <- attributes(listofIds)

  # target fields for adding cols for mangling below
  fields <- c(
    "_id",
    "ctrname",
    # euctr
    "a2_eudract_number",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a51_isrctn_international_standard_randomised_controlled_trial_number",
    "trialInformation.isrctnIdentifier",
    "a41_sponsors_protocol_code_number",
    # ctgov
    "id_info.secondary_id",
    "id_info.org_study_id",
    "id_info.nct_id",
    "id_info.nct_id",
    "id_info.nct_alias",
    "id_info.secondary_id",
    "id_info.secondary_id",
    "id_info.org_study_id",
    # isrctn
    "externalRefs.eudraCTNumber",
    "externalRefs.clinicalTrialsGovNumber",
    "externalRefs.clinicalTrialsGovNumber",
    "isrctn",
    "externalRefs.protocolSerialNumber",
    # ctis
    "ctNumber",
    "eudraCtInfo.eudraCtCode",
    "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
    "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
    # ctgov2
    "protocolSection.identificationModule.nctId",
    "protocolSection.identificationModule.nctId",
    "protocolSection.identificationModule.secondaryIdInfos.id",
    "protocolSection.identificationModule.secondaryIdInfos.id",
    "protocolSection.identificationModule.nctIdAliases",
    "protocolSection.identificationModule.orgStudyIdInfo.id"
  )
  if (verbose) {
    message(
      "\nFields used for finding corresponding register records of trials: ",
      "\n\n", paste0(fields, collapse = ", "), "\n"
    )
  }

  # add any missing columns
  missFields <- setdiff(fields, names(listofIds))
  if (length(missFields)) {
    missCols <- matrix(nrow = nrow(listofIds), ncol = length(missFields))
    missCols <- data.frame(missCols)
    names(missCols) <- missFields
    listofIds <- cbind(listofIds, missCols)
  }

  # replicate columns to make data frame fit subsequent steps
  listofIds <- listofIds[, fields, drop = FALSE]

  # rename columns for content mangling, needs to
  # correspond to columns and sequence in "fields"
  # for mapping identifiers across registers
  names(listofIds) <- c(
    "_id", "ctrname",
    # euctr
    "euctr.1", "ctgov.1a", "ctgov.1b", "ctgov2.1a", "ctgov2.1b", "isrctn.1a", "isrctn.1b", "sponsor.1",
    # ctgov
    "euctr.2a", "euctr.2b", "ctgov.2a", "ctgov2.2", "ctgov.2b", "isrctn.2",
    "sponsor.2a", "sponsor.2b",
    # isrctn
    "euctr.3", "ctgov.3", "ctgov2.3", "isrctn.3", "sponsor.3",
    # ctis
    "ctis.1", "euctr.4", "ctgov.4", "ctgov2.4",
    # ctgov2
    "ctgov2.5", "ctgov.5a", "euctr.5", "sponsor.4a", "ctgov.5b", "sponsor.4b"
  )

  # keep only relevant content
  # - in certain raw value columns
  colsToMangle <- list(
    c("ctgov.1a", regCtgov),
    c("ctgov.1b", regCtgov),
    c("ctgov.2a", regCtgov),
    c("ctgov.2b", regCtgov),
    c("ctgov.3", regCtgov),
    c("ctgov.4", regCtgov),
    c("ctgov.5a", regCtgov),
    c("ctgov.5b", regCtgov),
    c("ctgov2.1", regCtgov2),
    c("ctgov2.2", regCtgov2),
    c("ctgov2.3", regCtgov2),
    c("ctgov2.4", regCtgov2),
    c("ctgov2.5", regCtgov2),
    c("isrctn.1a", regIsrctn),
    c("isrctn.1b", regIsrctn),
    c("isrctn.2", regIsrctn),
    c("isrctn.3", regIsrctn),
    c("euctr.1", regEuctr),
    c("euctr.2a", regEuctr),
    c("euctr.2b", regEuctr),
    c("euctr.3", regEuctr),
    c("euctr.4", regEuctr),
    c("euctr.5", regEuctr),
    c("ctis.1", regCtis)
  )

  # - inconsistency:
  #   isrctn.3 = 12345678, but isrctn.1a
  #   and isrctn.1b have ISRCTN12345678
  listofIds["isrctn.1a"] <- sub("^ISRCTN", "", listofIds[["isrctn.1a"]])
  listofIds["isrctn.1b"] <- sub("^ISRCTN", "", listofIds[["isrctn.1b"]])

  # - do mangling; prerequisite is
  #   that each of the columns holds
  #   a single character vector,
  #   possibly collapsed with " / "
  invisible(sapply(
    colsToMangle,
    function(ctm) {
      colMangled <- regmatches(
        listofIds[[ctm[[1]]]],
        regexec(ctm[[2]], listofIds[[ctm[[1]]]])
      )
      colMangled[!lengths(colMangled)] <- ""
      listofIds[[ctm[[1]]]] <<- unlist(colMangled)
    }
  ))
  # - merge columns for register ids and sponsor ids
  for (reg in c(registerList, "SPONSOR")) {
    listofIds[[reg]] <- apply(
      listofIds[
        , grepl(paste0("^", reg, "[.][0-9]"), names(listofIds),
                ignore.case = TRUE
        ),
        drop = FALSE
      ],
      MARGIN = 1,
      function(r) {
        gsub(
          "^ ?/ | / ?$", "",
          paste0(na.omit(unique(r)), collapse = " / ")
        )
      }
    )
  }
  # - delete raw columns
  listofIds <- listofIds[
    , c("_id", "ctrname", registerList, "SPONSOR"),
    drop = FALSE
  ]

  # inform user
  message("- Finding duplicates among registers' and sponsor ids...")

  # find duplicates
  colsToCheck <- match(c(preferregister, "SPONSOR"), names(listofIds))
  outSet <- NULL
  for (i in seq_along(preferregister)) {
    # to be added
    tmp <- listofIds[
      listofIds[["ctrname"]] == preferregister[i], ,
      drop = FALSE
    ]
    row.names(tmp) <- NULL

    # check if second etc. set has identifiers
    # in the previously rbind'ed sets
    if (i > 1L && nrow(tmp)) {
      # check for duplicates
      dupes <- mapply(
        function(c1, c2) {
          tmpIs <- intersect(
            unlist(strsplit(c1, " / ")),
            unlist(strsplit(c2, " / "))
          )
          if (length(tmpIs)) {
            # map found intersecting names back
            # to the rows of the input data frame
            grepl(paste0(tmpIs, collapse = "|"), c1)
          } else {
            rep(FALSE, times = length(c1))
          }
        },
        tmp[, colsToCheck, drop = FALSE],
        outSet[, colsToCheck, drop = FALSE],
        SIMPLIFY = FALSE
      )

      # mangle dupes for marginal cases, e.g. one record
      dupes <- do.call(cbind, dupes)
      dupes <- as.data.frame(dupes)

      # keep uniques
      tmp <- tmp[rowSums(dupes) == 0L, , drop = FALSE]
      rm(dupes)
    }

    # add to output set
    outSet <- rbind(outSet, tmp,
                    make.row.names = FALSE,
                    stringsAsFactors = FALSE
    )
  }
  rm(tmp)

  # keep necessary columns
  listofIds <- outSet[, c("_id", "EUCTR", "ctrname")]
  names(listofIds)[2] <- "a2_eudract_number"
  rm(outSet)

  # find unique, preferred country version of euctr
  listofIds <- dfFindUniqueEuctrRecord(
    df = listofIds,
    prefermemberstate = prefermemberstate,
    include3rdcountrytrials = include3rdcountrytrials
  )

  # prepare output
  listofIds <- setNames(
    object = listofIds[["_id"]],
    nm = listofIds[["ctrname"]]
  )

  listofIds <- sort(listofIds)

  # count
  countIds <- table(names(listofIds))

  # sort by user's input
  countIds <- countIds[preferregister]
  countIds[is.na(countIds)] <- 0L
  countIds <- setNames(countIds, preferregister)

  # append attributes
  attributes(listofIds) <- c(
    attributes(listofIds),
    attribsids[startsWith(names(attribsids), "ctrdata-")]
  )

  # avoid returning list() if none found
  if (length(listofIds) == 0) listofIds <- character()

  # inform user
  message(
    "- Keeping ", paste0(countIds, collapse = " / "), " records",
    " from ", paste0(names(countIds), collapse = " / ")
  )
  message(
    "= Returning keys (_id) of ", length(listofIds),
    " records in collection \"", con$collection, "\""
  )

  # return
  return(listofIds)
}
# end dbFindIdsUniqueTrials



#' Select a single trial record from records of different EU Member States
#'
#' The EUCTR provides one record per trial per EU Member State in which the
#' trial is conducted. For all trials conducted in more than one Member State,
#' this function returns only one record per trial.
#'
#' Note: To deduplicate trials from different registers,
#' please first use function \link{dbFindIdsUniqueTrials}.
#'
#' @param df A data frame created from the database collection that includes
#'   the columns "_id" and "a2_eudract_number", for example created with
#'   function dbGetFieldsIntoDf(c("_id", "a2_eudract_number")).
#'
#' @inheritParams dfFindIdsUniqueTrials
#'
#' @return A data frame as subset of \code{df} corresponding to the sought
#'   records.
#'
#' @keywords internal
#' @noRd
#
dfFindUniqueEuctrRecord <- function(
    df = NULL,
    prefermemberstate = "DE",
    include3rdcountrytrials = TRUE) {
  # check parameters
  if (!any(class(df) %in% "data.frame")) {
    stop("Parameter df is not a data frame.", call. = FALSE)
  }
  #
  if (is.null(df[["_id"]]) ||
      is.null(df[["a2_eudract_number"]])) {
    stop('Data frame does not include "_id"',
         ' and "a2_eudract_number" columns.',
         call. = FALSE
    )
  }
  #
  if (nrow(df) == 0) {
    stop("Data frame does not contain records (0 rows).",
         call. = FALSE
    )
  }
  #
  if (!(prefermemberstate %in% countriesEUCTR)) {
    stop("Value specified for prefermemberstate does not match",
         " one of the recognised codes: ",
         paste(sort(countriesEUCTR), collapse = ", "),
         call. = FALSE
    )
  }

  # notify it mismatching parameters
  if (prefermemberstate == "3RD" && !include3rdcountrytrials) {
    warning("Preferred EUCTR version set to 3RD country trials, but ",
            "'include3rdcountrytrials' was FALSE, setting it to TRUE.",
            call. = FALSE,
            noBreaks. = FALSE,
            immediate. = FALSE
    )
    include3rdcountrytrials <- TRUE
  }

  # count total
  totalEuctr <- unique(df[["a2_eudract_number"]])
  totalEuctr <- na.omit(totalEuctr[totalEuctr != ""])
  totalEuctr <- length(totalEuctr)

  # as a first step, handle 3rd country trials e.g. 2010-022945-52-3RD
  # if retained, these trials would count as record for a trial
  if (!include3rdcountrytrials) {
    df <- df[!grepl("-3RD", df[["_id"]]), , drop = FALSE]
  }

  # count number of records by eudract number
  tbl <- table(df[["_id"]], df[["a2_eudract_number"]])
  tbl <- as.matrix(tbl)
  # nms has names of all records
  nms <- dimnames(tbl)[[1]]

  # nrs has eudract numbers for which is there more than 1 record
  nrs <- colSums(tbl)
  nrs <- nrs[nrs > 1]
  nrs <- names(nrs)

  # nst is a list of nrs trials of a logical vector along nms
  # that indicates if the indexed record belongs to the trial
  nms2 <- substr(nms, 1, 14)
  nst <- lapply(nrs, function(x) nms2 %in% x)

  # helper function to find the Member State version
  removeMSversions <- function(indexofrecords) {
    # given a vector of records (nnnn-nnnnnnn-nn-MS) of a single trial, this
    # returns all those _ids of records that do not correspond to the preferred
    # Member State record, based on the user's choices and defaults.
    # Function uses prefermemberstate, nms from the caller environment
    recordnames <- nms[indexofrecords]
    #
    # fnd should be only a single string, may need to be checked
    if (sum(fnd <- grepl(prefermemberstate, recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    #
    if (sum(fnd <- grepl("DE", recordnames)) != 0) {
      result <- recordnames[!fnd]
      return(result)
    }
    #
    # default is to list all but first record
    # the listed records are the duplicates
    # 3RD country trials would be listed first
    # hence selected, which is not desirable
    # unless chosen as prefermemberstate
    return(rev(sort(recordnames))[-1])
  }

  # finds per trial the desired record;
  # uses prefermemberstate and nms
  result <- lapply(
    nst,
    function(x) removeMSversions(x)
  )
  result <- unlist(result, use.names = FALSE)

  # eleminate the unwanted EUCTR records
  df <- df[!(df[["_id"]] %in% result), , drop = FALSE]

  # also eliminate the meta-info record
  df <- df[!(df[["_id"]] == "meta-info"), , drop = FALSE]

  # inform user about changes to data frame
  if (length(nms) > (tmp <- length(result))) {
    message(
      "- ", tmp,
      " EUCTR _id were not preferred EU Member State record for ",
      totalEuctr, " trials"
    )
  }

  # return
  return(df)
}
# end dfFindUniqueEuctrRecord
