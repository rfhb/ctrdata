### ctrdata package

#' ctrLoadQueryIntoDbCtis
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom jqr jq jq_flags
#' @importFrom utils read.table URLencode
#' @importFrom nodbi docdb_update
#' @importFrom jsonlite stream_in fromJSON
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed
#' @importFrom httr GET status_code content
#'
ctrLoadQueryIntoDbCtis <- function(
    queryterm = queryterm,
    register,
    euctrresults,
    euctrresultshistory,
    documents.path,
    documents.regexp,
    annotation.text,
    annotation.mode,
    only.count,
    con, verbose,
    queryupdateterm) {

  ## create empty temporary directory
  tempDir <- ctrTempDir(verbose)

  ## output mangle helper -----------------------------------------------

  mangleText <- function(t) {

    stringi::stri_replace_all_fixed(str = t, pattern = "'", replacement = "")

  }

  ## ctis api -----------------------------------------------------------

  ctisEndpoints <- paste0(
    #
    # root
    "https://euclinicaltrials.eu/ct-public-api-services/services",
    #
    # endpoints
    c(
      # - 1 trial overview - %s is for pagination
      "/ct/publiclookup?&paging=%s,%s&sorting=+ctNumber&%s",
      #
      # - 2 trial information - %s is ctNumber
      "/ct/%s/publicview", # partI and partsII
      #      
      # - 3 additional info public events - %s is id, %s is type of notification
      "/ct/%s/eventdetails?notificationType=%s",
      #
      # - 4 trial information - %s is ctNumber
      "/ct/%s/publicevents", # serious breach, unexpected event, 
      # urgent safety measure, inspection outside EEA, temporary halt
      #
      # - 5-9 additional information - %s is ctNumber
      "/ct/public/%s/summary/list",
      "/ct/public/%s/layperson/list",
      "/ct/public/%s/csr/list", # clinical study report
      "/ct/public/%s/cm/list",  # corrective measures
      "/ct/public/%s/inspections/list",
      #
      # - 10 trial information - %s is an application id
      "/ct/%s/publicEvaluation",
      #
      # - 11 download files - %s is document url
      "/ct/public/download/%s",
      #
      # - 12-21 documents - %s is entity identifier, lists
      "/document/part1/%s/list", # appl auth
      "/document/part2/%s/list", # appl auth
      "/document/product-group-common/%s/list",
      "/document/part1/%s/list?documentType=274", # p1 AR
      "/document/part2/%s/list/?documentType=43", # auth letter
      "/document/part2/%s/list/?documentType=42", # p2 ARs
      "/document/rfi/%s/list",                    # rfis
      "/document/notification/%s/list?documentType=101", # events
      "/document/cm/%s/list", # corrective measures
      "/document/smr/%s/list"
      #
      # unclear or not publicly accessible
      # https://euclinicaltrials.eu/ct-public-api-services/services/document/part1/4433/list?documentType=93
      # https://euclinicaltrials.eu/ct-public-api-services/services/document/part1/4433/list?documentType=94
      # https://euclinicaltrials.eu/ct-public-api-services/services/document/part2/14808/list/?documentType=41
      # https://euclinicaltrials.eu/ct-public-api-services/services/document/considerationDoc/32137/list
    )
  )

  ## api_1: overviews ---------------------------------------------------------

  # this is for importing overview (recruitment, status etc.) into database
  message("* Checking trials in CTIS...")

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  message("(1/5) Downloading trials list ", appendLF = FALSE)

  # prepare
  i <- 0L
  di <- 200L
  idsTrials <- NULL
  fTrialsNdjson <- file.path(tempDir, "ctis_trials_1.ndjson")
  unlink(fTrialsNdjson)

  # need to iterate / paginate as total number cannot be determined
  while (TRUE) {

    # {"totalSize":299,"pageInfo":{"offset":200,"limit":200,"pageNumber":2}
    url <- sprintf(ctisEndpoints[1], i, di, queryterm)
    url <- utils::URLencode(url)
    trialsJson <- httr::GET(url)
    message(". ", appendLF = FALSE)

    # early exit
    if (httr::status_code(trialsJson) != 200L) {
      warning("Could not be retrieved, check 'queryterm' and / or 'register'. ",
              "\nAPI returned: ", httr::content(trialsJson),
              call. = FALSE
      )
      message("API call: ", url)
      return(emptyReturn)
    }

    # extract json
    trialsJson <- suppressMessages(
      httr::content(trialsJson, as = "text")
    )

    # get total size
    totalSize <- as.numeric(
      jqr::jq(trialsJson, " {name: .totalSize} | .[]")
    )

    # extract trial information
    # and convert to ndjson
    trialsJson <- jqr::jq(
      trialsJson,
      paste0(
        # extract trial records
        " .elements | .[] ",
        # add element _id
        '| .["_id"] = .ctNumber',
        # keep only standardised fields
        "| del(.id, .ctNumber, .product, .endPoint, .eudraCtInfo, .ctTitle,
             .eudraCtInfo, .primaryEndPoint, .sponsor, .conditions) "
      ),
      flags = jqr::jq_flags(pretty = FALSE)
    )

    # get ids
    idsTrialsBatch <- gsub(
      '"', "", as.character(
        jqr::jq(
          trialsJson,
          ' ."_id" '
        )))

    # check for any duplicates
    nonDuplicates <- !(idsTrialsBatch %in% idsTrials)
    idsTrials <- c(idsTrials, idsTrialsBatch[nonDuplicates])

    # sanitise
    trialsJson <- mangleText(trialsJson)

    # save and append to ndjson
    cat(
      trialsJson[nonDuplicates],
      sep = "\n",
      file = fTrialsNdjson,
      append = TRUE
    )

    # iterate or break
    if (totalSize < (i + di)) break

    # update batch parameters
    i <- i + di
  }

  # early exit
  if (!totalSize) {
    warning("No trials found, check 'queryterm' and 'register'")
    return(emptyReturn)
  }

  # duplicates?
  if (totalSize != length(idsTrials)) {
    warning("Overview retrieval resulted in duplicate ",
            "trial records, only first record was kept. ")
  }

  # inform user
  message("found ", length(idsTrials), " trials")

  # only count?
  if (only.count) {
    # return
    return(list(
      n = length(idsTrials),
      success = NULL,
      failed = NULL
    ))
  }

  ## api_2: partI, partsII ---------------------------------------------------

  # this is imported as the main data into the database

  message("(2/5) Downloading and processing part I and parts II... (",
          "estimate: ", length(idsTrials) * 0.15, " Mb)")

  urls <- sprintf(ctisEndpoints[2], idsTrials)

  fPartIPartsIIJson <- function(i) {
    file.path(tempDir, paste0("ctis_trial_partIpartsII_", i, ".json"))
  }

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  tmp <- ctrMultiDownload(urls, fPartIPartsIIJson(idsTrials), verbose = verbose)

  importString <- paste0(
    '"ctrname":"CTIS",\\1,"record_last_import":"',
    strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), '",')

  # convert partI and partsII details into ndjson file
  fPartIPartsIINdjson <- file.path(tempDir, "ctis_add_2.ndjson")
  unlink(fPartIPartsIINdjson)

  for (fn in tmp[["destfile"]]) {
    if (!file.exists(fn)) next
    cat(
      # files include id, ctNumber and others repeatedly
      # only replace first instance for updating records
      # sanitise texts removing various quotation marks
      sub("(\"id\":[0-9]+),", importString,
          sub("(\"ctNumber\"):(\"[-0-9]+\"),", '\\1:\\2,"_id":\\2,',
              mangleText(readLines(fn, warn = FALSE))
          )),
      file = fPartIPartsIINdjson,
      append = TRUE,
      sep = "\n")
    message(". ", appendLF = FALSE)
  }

  # address and mangle "applications" which has (as only field in ctis)
  # '"partIIInfo": "<integer number>": {...}' by replacing with array
  # '{"partIIInfo": [{"partIIIinfoKey": <integer number>, ...}]}'
  ctisMangle <- list(
    # file in, file out, pattern, replacement
    c(fPartIPartsIINdjson, paste0(fPartIPartsIINdjson, "a"), '"([0-9]+)": ?[{]', '{"partIIIinfoKey":$1,'),
    c(paste0(fPartIPartsIINdjson, "a"), paste0(fPartIPartsIINdjson, "b"), '"partIIInfo":[{]', '"partIIInfo":['),
    c(paste0(fPartIPartsIINdjson, "b"), fPartIPartsIINdjson, '[}],"(reporting|decision)Date"', '],"$1Date"')
  )
  for (i in ctisMangle) {
    cat(stringi::stri_replace_all_regex(
      str = readLines(i[1], warn = FALSE),
      pattern = i[3], replacement = i[4]
    ), file = i[2], sep = "\n")
    message(". ", appendLF = FALSE)
  }
  if (!verbose) unlink(paste0(fPartIPartsIINdjson, c("a", "b")))

  ## api_3-9: more data ----------------------------------------------------
  
  # helper function
  publicEventsMerger <- function(publicEvents) {

    # get event types that have data with ids of events
    eventTypes <- jqr::jq(
      publicEvents,
      " to_entries[] | select(.value | length > 0) | ([.key] + (.value[] | [.id])) ")

    # loop over event type
    for (eventType in eventTypes) {

      # get ids
      ids <- jqr::jq(eventType, " .[] ")
      ids[1] <- gsub("\"", "", ids[1])

      # get data
      urls <- sprintf(ctisEndpoints[3], ids[2],
                      toupper(gsub("([A-Z])", "_\\1", gsub("List", "", ids[1]))))
      eventData <- httr::GET(urls)
      if (httr::status_code(eventData) != 200L) next
      eventData <- suppressMessages(httr::content(eventData, as = "text"))

      # update input json with event data
      eventData <- paste0(
        " .", ids[1], " |= map( [select( .id == ", ids[2], ") | .details = ",
        eventData, "], [select( .id != ", ids[2], ")] | select( . | length > 0) ) ")
      publicEvents <- jqr::jq(publicEvents, eventData)

    }

    # return updated publicEvents json
    return(publicEvents)

  }

  message("\n(3/5) Downloading and processing additional data:")

  for (e in 4:9) {

    urls <- sprintf(ctisEndpoints[e], idsTrials)
    ep <- sub(".+/(.+?)$", "\\1", sub("/list$", "", urls[1]))
    message(ep, ", ", appendLF = FALSE)

    fAddJson <- function(i) {
      file.path(tempDir, paste0("ctis_add_", e, "_", i, ".json"))
    }

    # "HTTP server doesn't seem to support byte ranges. Cannot resume."
    tmp <- ctrMultiDownload(urls, fAddJson(idsTrials), progress = FALSE, verbose = verbose)

    # convert into ndjson file
    fAddNdjson <- file.path(tempDir, paste0("ctis_add_", e, ".ndjson"))
    unlink(fAddNdjson)

    for (fi in seq_len(nrow(tmp))) {

      fn <- tmp[["destfile"]][fi]

      if (file.size(fn) < 150L) next # sizes 99, 100, 134 byte

      # get data
      jOut <- readLines(fn, warn = FALSE)

      # sanitise
      jOut <- mangleText(jOut)

      # remove irrelevant information
      jOut <- sub('^.*"elements":(.*?)}?$', "\\1", jOut)
      jOut <- sub('(,?)"showWarning":(false|true)(,?)', "\\3", jOut)
      jOut <- sub('(,?)"totalSize":[0-9]+(,?)', "\\2", jOut)
      jOut <- sub('(,?)"pageInfo":[{].+?[}](,?)', "\\2", jOut)
      if (!nchar(jOut) || jOut == "[]") next

      # if publicevents (ctisEndpoints[4]), obtain additional data
      if (e == 4L) jOut <- publicEventsMerger(jOut)

      # reconstruct trial id
      id <- sub(paste0(".+/(", regCtis, ")/.+"), "\\1", tmp[["url"]][fi])

      # write out into ndjson file
      cat(
        paste0(
          # use endpoint as name for top level element
          '{"_id":"', id, '","', ep, '":', jOut, '}'),
        file = fAddNdjson,
        append = TRUE,
        sep = "\n")

    }

  }

  ## add_10: publicevaluation -----------------------------------------------------

  message("publicevaluation")

  fApplicationsJson <- file.path(tempDir, "ctis_add_10.json")

  # get ids of trial applications
  jqr::jq(
    file(fPartIPartsIINdjson),
    ' { ctNumber: .ctNumber, applicationIds: [ .applications[] | .id ] } ',
    flags = jqr::jq_flags(pretty = FALSE),
    out = fApplicationsJson
  )

  idsApplications <- jsonlite::stream_in(file(fApplicationsJson), verbose = FALSE)

  dlFiles <- apply(idsApplications, 1, function(r) {
    data.frame(
      "_id" = unlist(r[1], use.names = TRUE), r[-1],
      check.names = FALSE, stringsAsFactors = FALSE,
      row.names = NULL, check.rows = FALSE)
  })

  dlFiles <- do.call(rbind, dlFiles)
  dlFiles$url <- sprintf(ctisEndpoints[10], dlFiles$applicationIds)
  dlFiles$filepathname <- file.path(
    tempDir, paste0("ctis_add_10_", dlFiles$applicationIds, ".json"))

  # "HTTP server doesn't seem to support byte ranges. Cannot resume."
  tmp <- ctrMultiDownload(
    dlFiles$url,
    dlFiles$filepathname,
    resume = FALSE,
    verbose = verbose)

  fApplicationsNdjson <- file.path(tempDir, "ctis_add_10.ndjson")
  unlink(fApplicationsNdjson)

  for (i in seq_len(nrow(idsApplications))) {

    # read all files for _id into vector
    fApps <- file.path(tempDir, paste0(
      "ctis_add_10_", unlist(idsApplications$applicationIds[i]), ".json"))

    fApps <- fApps[file.size(fApps) >= 50L]
    fn <- tmp[["destfile"]][fi]
    if (!length(fApps)) next

    # get data
    jApps <- sapply(fApps, readLines, warn = FALSE, USE.NAMES = FALSE)
    if (!length(jApps)) next

    # sanitise
    jApps <- mangleText(jApps)

    # add applicationId
    jApps <- mapply(function(i, j) sub(
      "^[{]", paste0('{"id":', i, ","), j),
      unlist(idsApplications$applicationIds[i]),
      jApps)

    # compose array
    jApps <- paste0(
      '{"_id":"', idsApplications[["ctNumber"]][i],
      '","publicEvaluation":[',
      paste0(jApps, collapse = ","),
      ']}')

    # write ndjson
    cat(
      jApps,
      file = fApplicationsNdjson,
      append = TRUE,
      sep = "\n")

    message(i, rep("\b", nchar(i)), appendLF = FALSE)

  }

  ## database import -----------------------------------------------------

  message("\n(4/5) Importing JSON records into database...")

  # dbCTRLoadJSONFiles operates on pattern = ".+_trials_.*.ndjson"
  imported <- dbCTRLoadJSONFiles(dir = tempDir, con = con, verbose = verbose)

  # iterating over any additional ndjson files
  resAll <- NULL
  message("(5/5) Updating with additional data: ", appendLF = FALSE)

  for (f in dir(path = tempDir, pattern = "^ctis_add_[1-9]+[0-9]*.ndjson$", full.names = TRUE)) {

    message(". ", appendLF = FALSE)
    res <- nodbi::docdb_update(src = con, key = con$collection, query = "", value = f)
    resAll <- c(resAll, res)

  }
  message("")

  ## api_12-21: documents -------------------------------------------------------

  if (!is.null(documents.path)) {

    # 1 - get ids of lists (which include urls to download)
    message("- Getting ids of lists with document information")

    # get temporary file
    downloadsNdjson <- file.path(tempDir, "ctis_downloads.ndjson")

    ## extract ids of lists per parts per trial
    
    # extract ids from parts 1 and 2
    jqr::jq(
      file(fPartIPartsIINdjson),
      ' ._id |= gsub("\\""; "") | { _id: ._id,
          part1appl: [ .applications[].partI.id ],
          part1auth: [ .authorizedPartI.id ],
          parts2appl: [ .applications[].partIIInfo[].id ],
          parts2auth: [ .authorizedPartsII[].id ],
          prodappl: [ .applications[].partI.productRoleGroupInfos[].id ],
          prodauth: [ .authorizedPartI.productRoleGroupInfos[].id ],
          p1ar: [ .applications[].partI.id ],
          p2ars: [ .applications[].partIIInfo[].id ],
          ctaletter: [ .applications[].partIIInfo[].id ]
        } ',
      flags = jqr::jq_flags(pretty = FALSE),
      out = downloadsNdjson
    )
    
    # extract ids from additional data
    for (i in 5L:9L) {
      inF <- file.path(tempDir, paste0("ctis_add_", i, ".ndjson"))
      outF <- file.path(tempDir, paste0("ctis_downloads_add_", i, ".ndjson"))
      if (!file.exists(inF)) next
      jqr::jq(
        file(inF),
        paste0(
          ' ._id |= gsub("\\""; "") | { _id: ._id, ',
          sub(".+/%s/(.+?)/list", "\\1", ctisEndpoints[i]), ': [ .',
          sub(".+/%s/(.+?)/list", "\\1", ctisEndpoints[i]), '[].id ] ',
          '} '),
        flags = jqr::jq_flags(pretty = FALSE),
        out = outF
      )
    }

    # extract ids of rfis from publicevaluation (ctisEndpoints[10])
    rfiIds1 <- jqr::jq(
      file(fApplicationsNdjson),
      '{ _id: ._id, rfis1: [ .publicEvaluation[].partIRfis[].id ]}')
    rfiIds1 <- jsonlite::fromJSON(paste0("[", paste0(rfiIds1, collapse = ","), "]"))
    rfiIds2 <- jqr::jq(
      file(fApplicationsNdjson),
      '{ _id: ._id, rfis2: [ .publicEvaluation[].partIIEvaluationList[].partIIRfis[].id ]}')
    rfiIds2 <- jsonlite::fromJSON(paste0("[", paste0(rfiIds2, collapse = ","), "]"))

    # extract ids of documents from publicEvents (ctisEndpoints[3])
    if (file.exists(file.path(tempDir, "ctis_add_3.ndjson"))) {
      eventIds <- jqr::jq(file(file.path(tempDir, "ctis_add_3.ndjson")),
                          " {_id: ._id, events: [ .publicevents[][][].id ]}")
      eventIds <- jsonlite::fromJSON(paste0("[", paste0(eventIds, collapse = ","), "]"))
    } else {
      eventIds <- data.frame()
    }

    # convert and merge by ids
    dlFiles <- jsonlite::stream_in(file(downloadsNdjson), verbose = FALSE)
    if (nrow(rfiIds1)) {dlFiles <- merge(dlFiles, rfiIds1, all.x = TRUE)}
    if (nrow(rfiIds2)) {dlFiles <- merge(dlFiles, rfiIds2, all.x = TRUE)}
    if (nrow(eventIds)) {dlFiles <- merge(dlFiles, eventIds, all.x = TRUE)}
    for (i in 5L:9L) {
      outF <- file.path(tempDir, paste0("ctis_downloads_add_", i, ".ndjson"))
      if (!file.exists(outF)) next
      tmp <- jsonlite::stream_in(file(outF), verbose = FALSE)
      if (nrow(tmp)) {dlFiles <- merge(dlFiles, tmp, all.x = TRUE)}
    }

    # map
    epTyp <- ctisEndpoints[12:21]
    names(epTyp) <- c(
      "part1", "parts2", "prod", "p1ar", "p2ars", "ctaletter", "rfis", 
      "events", "cm", "layperson")
    epTyp <- as.list(epTyp)

    # define order for factor for sorting
    orderedParts <- c(
      "ctaletter", "p1ar", "p2ars", "part1auth", "part1appl",
      "parts2auth", "parts2appl", "prodauth", "prodappl", "rfis",
      "events", "cm", "layperson")
    
    # ordering files list
    dlFiles <- apply(dlFiles, 1, function(r) {
      tmp <- data.frame(id = unlist(r[-1], use.names = TRUE), r[1],
                        check.names = FALSE, stringsAsFactors = FALSE)
      # remove rows from absent elements (5 to 9 above)
      tmp <- tmp[!is.na(tmp$id), , drop = FALSE]
      #
      # if url occurs repeatedly, only use last from defined order
      tmp$part <- sub("[0-9]+$", "", row.names(tmp))
      tmp$part <- ordered(tmp$part, orderedParts)
      tmp$typ <- sub("appl|auth", "", tmp$part)
      #
      # construct url
      tmp$url <- mapply(
        function(t, i) sprintf(epTyp[t][[1]], i), tmp$typ, tmp$id)
      #
      # if url occurs repeatedly, only use last 
      tmp <- tmp[order(tmp$url, tmp$part), , drop = FALSE]
      rl <- rle(tmp$url)
      rl <- unlist(sapply(rl$lengths, function(i) c(TRUE, rep(FALSE, i - 1L))))
      tmp[rl, , drop = FALSE]
    })
    #
    dlFiles <- do.call(rbind, dlFiles)
    dlFiles <- na.omit(dlFiles)

    # do downloads of list files
    message("- Downloading ", nrow(dlFiles),
            " lists with document information (estimate: ",
            nrow(dlFiles) * 0.02, " Mb)")

    fFilesListJson <- function(t, p, id) {
      file.path(tempDir, paste0("ctis_fileslist_", t, "_", p, "_", id, ".json"))
    }

    dlFiles$destfile <- fFilesListJson(
      dlFiles[["_id"]], dlFiles[["part"]], dlFiles[["id"]])

    tmp <- ctrMultiDownload(
      dlFiles[["url"]],
      dlFiles[["destfile"]],
      verbose = verbose)

    if (sum(tmp$status_code != 200L, na.rm = TRUE)) {
      warning("Could not download these lists with document information: ",
              paste0(tmp$url[tmp$status_code != 200L], collapse = ", "))
      tmp <- tmp[tmp$status_code == 200L, , drop = FALSE]
    }

    # 2 - create data frame with info on documents (url, name, extension etc.)

    message("- Processing document information in ", nrow(tmp), " lists")
    epTypChars <- paste0(names(epTyp), "appl", "auth", collapse = "")
    epTypChars <- rawToChar(unique(charToRaw(epTypChars)))
    unlink(downloadsNdjson)

    for (fi in seq_len(nrow(tmp))) {

      fn <- tmp[["destfile"]][fi]
      if (file.size(fn) < 50L) next # size 49

      # reconstruct trial id
      id <- sub(paste0(".+_(", regCtis, ")_.+"), "\\1", tmp[["destfile"]][fi])

      # reconstruct part
      part <- sub(paste0(
        "^.+_([", epTypChars, "]+?)_[0-9]+[.]json$"),
        "\\1", tmp[["destfile"]][fi])

      # get data
      jOut <- readLines(fn, warn = FALSE)

      # remove irrelevant information
      jOut <- sub('^.*"elements":(.*?)}?$', "\\1", jOut)
      jOut <- sub('(,?)"showWarning":(false|true)(,?)', "\\3", jOut)
      jOut <- sub('(,?)"totalSize":[0-9]+(,?)', "\\2", jOut)
      jOut <- sub('(,?)"pageInfo":[{].+?[}](,?)', "\\2", jOut)
      jOut <- gsub('"versions":[[][{].+?[}][]],', "", jOut) # reconsider
      if (!nchar(jOut) || jOut == "[]") next

      jOut <- paste0(
        '{"_id":"', id, '",',
        stringi::stri_extract_all_regex(jOut, '"url":"[-a-z0-9]+?",')[[1]],
        stringi::stri_extract_all_regex(jOut, '"title":".+?",')[[1]],
        stringi::stri_extract_all_regex(jOut, '"fileTypeLabel":"[A-Z]+?",')[[1]],
        stringi::stri_extract_all_regex(jOut, '"documentIdentity":[0-9]+?,')[[1]],
        '"part":"', part, '"}'
      )

      jOut <- jOut[!grepl('",NA"', jOut)]
      if (!length(jOut)) next

      cat(
        jOut,
        file = downloadsNdjson,
        append = TRUE,
        sep = "\n")

      message(fi, rep("\b", nchar(fi)), appendLF = FALSE)

    } # for

    # 3 - documents download
    dlFiles <- jsonlite::stream_in(file(downloadsNdjson), verbose = FALSE)

    # check if any documents
    if (!nrow(dlFiles)) {
      message("= No documents identified for downloading.")
    } else {
      
      # remove duplicate files based on their title
      dlFiles$part <- ordered(dlFiles$part, orderedParts)
      dlFiles <- dlFiles[order(dlFiles$title, dlFiles$part), , drop = FALSE]
      rl <- rle(dlFiles$title)
      rl <- unlist(sapply(rl$lengths, function(i) c(TRUE, rep(FALSE, i - 1L))))
      dlFiles <- dlFiles[rl, , drop = FALSE]
      
      # add destination file name
      dlFiles$filename <- paste0(
        dlFiles$part, "_",
        # robustly sanitise file name
        gsub("[^[:alnum:] ._-]", "",  dlFiles$title),
        ".", dlFiles$fileTypeLabel)
      
      #### api_11: urls ####
      
      # calculate url
      dlFiles$url <- sprintf(ctisEndpoints[11], dlFiles$url)
      
      # do download
      resFiles <- ctrDocsDownload(
        dlFiles[, c("_id", "filename", "url"), drop = FALSE],
        documents.path, documents.regexp, verbose)
      
    } # if (!nrow(dlFiles))
    
  } # !is.null(documents.path)

  ## inform user -----------------------------------------------------

  #  find out number of trials imported into database
  message("= Imported / updated ",
          paste0(c(imported$n, resAll), collapse = " / "),
          " records on ", length(idsTrials), " trial(s)")

  # return
  return(imported)
}
# end ctrLoadQueryIntoDbCtis
