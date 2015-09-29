### ctrdata package
### main functions

#' Retrieve information on clinical trials from register and store in database
#'
#' Note that upsert is used to store in database, which means that records may be accumulated. If you want to insert into an empty database,
#' you have to include a mongo connection object to such an empty database.
#'
#' @param queryterm Part of the URL of a search in a register. The queryterms is recorded in the collection for later use to update records.
#' @param register Vector of abbreviations of registers to query, defaults to "EUCTR"
#' @param details If \code{TRUE}, retrieve full protocol-related information from EUCTR (rather slow), if \code{FALSE} retrieve only summary information (no country-specific information). Default is \code{TRUE}.
#' @param mongo (\link{mongo}) A mongo connection object. If not provided, defaults to database "users" on localhost port 27017.
#' @param ns Name of the collection in mongo database ("namespace"), defaults to "ctrdata"
#' @param updaterecords Re-run last query for this collection. This parameter takes precedence over \code{queryterm}.
#' @param parallelretrievals Number of parallel downloads of information from the register
#' @param debug Printing additional information if set to \code{TRUE}; default is \code{FALSE}.
#' @return Number of trials imported or updated in the database
#' @examples
#' # Retrieve protocol-related information on a single trial identified by EudraCT number
#' \dontrun{
#' getCTRdata (queryterm = "2013-001291-38 ")
#' }
#'
#' # For use with EudraCT: define paediatric population and cancer terms, for a query of more than 400 trials
#' \dontrun{
#' queryEuDefPaedPopulation  <- "age=adolescent&age=children&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18"
#' queryEuDef01paedOncTrials <- "cancer leukaem leukem sarcoma tumour tumor blastom gliom lymphom malign hodgkin ewing rhabdo teratom tumeur leucemi"
#' queryEuDef01paedOncTrials <- gsub (" ", "%20OR%20", queryEuDef01paedOncTrials)
#' queryEuDef01paedOncTrials <- paste (queryEuDef01paedOncTrials, queryEuDefPaedPopulation, sep="&")
#' getCTRdata (queryterm = queryEuDef01paedOncTrials, parallelretrivals = 5)
#' }
#'
#' # Retrieve protocol-related information on ongoing interventional cancer trials in children
#' \dontrun{
#' getCTRdata (queryterm = "cancer&recr=Open&type=Intr&age=0", register = "CTGOV")
#' }
#' \dontrun{
#' getCTRdata (queryterm = "NCT02239861", register = "CTGOV")
#' }
#'
#' @import RCurl rmongodb curl
#' @export getCTRdata
#'
getCTRdata <- function(queryterm = "", register = "EUCTR", updaterecords = FALSE, details = TRUE, parallelretrievals = 10,
                        mongo = rmongodb::mongo.create(host = "localhost:27017", db = "users"), ns = "ctrdata", debug = FALSE) {

  # deal with querystring such as returned from getCTRQueryUrl()
  if (is.list(queryterm)) {
    #
    tmp <- queryterm
    #
    queryterm <- tmp$queryterm
    register  <- tmp$register
    #
  }
  # basic sanity check if query term should be valid
  if (grepl("[^a-zA-Z0-9=+&%_]", queryterm)) stop('Queryterm has unexpected characters: "', queryterm, '" (expected: a-zA-Z0-9=+&%_).')

  # other sanity checks
  if ((queryterm == "") & !updaterecords) stop("Empty query term.")
  if (class(mongo) != "mongo") stop("'mongo' is not a mongo connection object.")
  if (register  == "")         stop("Register name empty.")

  # check program availability
  if (.Platform$OS.type == "windows") {
    findMongo()
    if (is.na(mongoBinaryLocation)) stop("Not starting getCTRdata because mongoimport was not found.")
  }

  # remove trailing or leading whitespace
  queryterm <- gsub("^\\s+|\\s+$", "", queryterm)

  # helper function for adding query parameters and results to database
  dbCTRUpdateQueryHistory <- function(register, queryterm, recordnumber, mongo, ns){
    #
    bson <- paste0('{"_id": "meta-info", "query-terms": "', queryterm, '", "query-register": "', register,
                   '", "query-timestamp": ', '"', format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), '"}')
    #
    bson <- rmongodb::mongo.bson.from.JSON(bson)
    #
    #
    # TODO retrieve existing history data and add to it
    #
    rmongodb::mongo.insert(mongo, paste0(attr(mongo, "db"), ".", ns), bson)
    #
  }


  ############################

  if ("CTGOV" %in% register) {
    #stop("CTGOV query is not yet implemented.")

    # create empty temporary directory on localhost for
    # download from register into temporary directy
    tempDir <- tempfile(pattern = "ctrDATA")
    dir.create(tempDir)

    # CTGOV standard identifiers
    queryUSRoot  <- "https://clinicaltrials.gov/"
    queryUSType1 <- "ct2/results/download?term="
    queryUSPost  <- "&down_stds=all&down_typ=fields&down_flds=all&down_fmt=csv&show_down=Y"

    # CTGOV field names - use NCT for mongodb index
    fieldsCTGOV  <- c("Rank","NCT Number","Title","Recruitment","Study Results","Conditions","Interventions","Sponsor/Collaborators",
                      "Gender","Age Groups","Phases","Enrollment","Funded Bys","Study Types","Study Designs","Other IDs","First Received",
                      "Start Date","Completion Date","Last Updated","Last Verified","Results First Received","Acronym",
                      "Primary Completion Date","Outcome Measures","URL")
    fieldsCTGOV <- sub("NCT Number", "_id", fieldsCTGOV)
    write(fieldsCTGOV, paste0(tempDir, "/field_names.txt"))

    # try to re-use previous query as recorded in the collection
    if (updaterecords && queryterm != "") warning("New query term specified despite updaterecords = TRUE, continuing with new query", immediate. = TRUE)
    if (updaterecords && queryterm == "") {
      rerunquery <- rmongodb::mongo.find.one(mongo, paste0(attr(mongo, "db"), ".", ns),
                                         query  = list('_id' = 'meta-info'),
                                         fields = list("query-terms" = 1L, "query-timestamp" = 1L))
      if (is.null(rerunquery)) stop("Could not find previous query in specified collection, aborting because of updaterecords = TRUE.")
      rerunquery <- rmongodb::mongo.bson.to.list(rerunquery)
      if (rerunquery$`query-register` == "CTGOV") {
        message(paste0("Rerunning query: ", rerunquery$`query-terms` , "\nLast run: ", rerunquery$`query-timestamp`))
        queryterm <- rerunquery$`query-terms`
      }
    }

    # get results
    message(paste0("Downloading trials from CTGOV ..."))
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = FALSE)
    curl::curl_download(paste0(queryUSRoot, queryUSType1, queryterm, queryUSPost), paste0(tempDir, "/ctgov.zip"), mode = "wb", handle = h)
    unzip(paste0(tempDir, "/ctgov.zip"), exdir = tempDir)
    resultsCTGOV <- paste0(tempDir, "/study_fields.csv")

    # call to import in csv format (not possible from within R)
    ctgov2mongo <- paste0('mongoimport --host="', attr(mongo, "host"), '" --db="', attr(mongo, "db"), '" --collection="', ns, '"',
                          ifelse(attr(mongo, "username") != "", paste0(' --username="', attr(mongo, "username"), '"'), ''),
                          ifelse(attr(mongo, "password") != "", paste0(' --password="', attr(mongo, "password"), '"'), ''),
                          ' --fieldFile="', paste0(tempDir, '/field_names.txt"'),
                          ' --upsert --type=csv --file="', resultsCTGOV, '"')
    #
    if (.Platform$OS.type == "windows") {
      #
      ctgov2mongo <- paste0(mongoBinaryLocation, ctgov2mongo)
      #
    } else {
      #
      # mongoimport does not return exit value, hence redirect stderr to stdout
      ctgov2mongo <- paste(ctgov2mongo, '2>&1')
      #
    }
    #
    message(paste0("Importing CTGOV CSV into mongoDB ..."))
    if (debug) message(ctgov2mongo)
    imported <- system(ctgov2mongo, intern = TRUE)

    # remove document that was the headerline in the imported file
    rmongodb::mongo.remove(mongo, paste0(attr(mongo, "db"), ".", ns), list("_id" = "NCT Number"))

    # split otherids into new array for later perusal
    cursor <- rmongodb::mongo.find(mongo, paste0(attr(mongo, "db"), ".", ns),
                                   query = list('Other IDs' = list('$gt' = '')), fields = list("Other IDs" = 1L))
    while (rmongodb::mongo.cursor.next(cursor)) {
      # get other ids
      oids <- unlist(strsplit(rmongodb::mongo.bson.to.list(rmongodb::mongo.cursor.value(cursor))[[2]], "[|]"))
      # update record with additional field
      rmongodb::mongo.update(mongo, paste0(attr(mongo, "db"), ".", ns), criteria = rmongodb::mongo.cursor.value(cursor),
                             objNew = list('$set' = list("otherids" = oids)))
    } # cleanup
    rmongodb::mongo.cursor.destroy(cursor)

    # add index on otherids for later queries
    rmongodb::mongo.index.create(mongo, paste0(attr(mongo, "db"), ".", ns), key = list("otherids" = 1L))

    # find out number of trials imported into database
    imported <- as.integer(gsub(".*imported ([0-9]+) document.*", "\\1", imported[length(imported)])) - 1
    if (!is.numeric(imported) || imported == 0) stop("Import has apparently failed, returned ", imported)
    message(paste0("Done - imported or updated ", imported, " trial(s)."))

    # clean up temporary directory
    if (!debug) unlink(tempDir, recursive = TRUE)

    # add query parameters to database
    dbCTRUpdateQueryHistory(register = register, queryterm = queryterm, recordnumber = imported, mongo = mongo, ns = ns)

    # update keys database
    dbFindCTRkey(forceupdate = TRUE, mongo = mongo, ns = ns)

  }

  ############################

  if ("EUCTR" %in% register) {

    # check program version as acceptable json format changed from 2.x to 3.x
    checkMongoVersionOk()

    # create empty temporary directory on localhost for
    # download from register into temporary directy
    tempDir <- tempfile(pattern = "ctrDATA")
    dir.create(tempDir)

    # EUCTR standard identifiers
    queryEuRoot  <- "https://www.clinicaltrialsregister.eu/"
    queryEuType1 <- "ctr-search/search?query="
    queryEuType2 <- "ctr-search/rest/download/summary?query="
    queryEuType3 <- "ctr-search/rest/download/full?query="
    queryEuPost  <- "&mode=current_page&format=text&dContent=summary&number=current_page&submit-download=Download"

    # try to re-use previous query as recorded in the collection
    if (updaterecords && queryterm != "") warning("New query term specified despite updaterecords = TRUE, continuing with new query", immediate. = TRUE)
    if (updaterecords && queryterm == "") {
      rerunquery <- rmongodb::mongo.find.one(mongo, paste0(attr(mongo, "db"), ".", ns),
                                             query  = list('_id' = 'meta-info'),
                                             fields = list("query-terms" = 1L, "query-timestamp" = 1L))
      if (is.null(rerunquery)) stop("Could not find previous query in specified collection, aborting because of updaterecords = TRUE.")
      rerunquery <- rmongodb::mongo.bson.to.list(rerunquery)
      if (rerunquery$`query-register` == "EUCTR") {
        message(paste0("Rerunning query: ", rerunquery$`query-terms` , "\nLast run: ", rerunquery$`query-timestamp`))
        queryterm <- rerunquery$`query-terms`
      }
    }

    # get first result page
    h = RCurl::getCurlHandle() # does not work: , httpheader = c(Accept = "Accept-Encoding: gzip,deflate")
    resultsEuPages <- RCurl::getURL(paste0(queryEuRoot, queryEuType1, queryterm), curl = h, ssl.verifypeer = FALSE)
    resultsEuNumTrials <- sub(".*Trials with a EudraCT protocol \\(([0-9,.]*)\\).*", "\\1", resultsEuPages)
    resultsEuNumTrials <- as.numeric(gsub("[,.]", "", resultsEuNumTrials))
    resultsEuNumPages  <- ceiling(resultsEuNumTrials / 20) # this is simpler than parsing "next" or "last" links ...
    if (is.na(resultsEuNumPages) | is.na(resultsEuNumTrials)) stop("first result page empty")
    message(paste0("Retrieved overview: ", resultsEuNumTrials, " trials from ", resultsEuNumPages, " page(s) are to be downloaded."))

    # get data
    resultsNumBatches <- resultsEuNumPages %/% parallelretrievals
    resultsNumModulo  <- resultsEuNumPages %%  parallelretrievals
    message(paste0("Downloading trials (from ", parallelretrievals, " page(s) in parallel):"))
    #
    for (i in 1:(resultsNumBatches + 1) ) {
      # parallel requests by using startpage:stoppage
      # TODO use queue and re-queueing
      startpage <- (i - 1) * parallelretrievals + 1
      stoppage  <- ifelse(i > resultsNumBatches, startpage + resultsNumModulo, startpage + parallelretrievals) - 1
      message(paste0("(", i, ") ", startpage, "-", stoppage, ". "))
      #
      tmp <- RCurl::getURL(paste0(queryEuRoot, ifelse(details, queryEuType3, queryEuType2), queryterm, "&page=", startpage:stoppage,
                                  queryEuPost), curl = h, async = TRUE, binary = FALSE, ssl.verifypeer = FALSE)
      #
      for (i in startpage:stoppage)
        write(tmp[[1 + i - startpage]],
              paste0(tempDir, "/euctr-trials-page_", formatC(i, digits = 0, width = nchar(resultsEuNumPages), flag = 0), ".txt"))
    }

    # compose commands: for external script on all files in temporary directory and for import
    euctr2json <- system.file("exec/euctr2json.sh", package = "ctrdata", mustWork = TRUE)
    euctr2json <- paste(euctr2json, tempDir)
    json2mongo <- paste0('mongoimport --host="', attr(mongo, "host"), '" --db="', attr(mongo, "db"), '" --collection="', ns, '"',
                         ifelse(attr(mongo, "username") != "", paste0(' --username="', attr(mongo, "username"), '"'), ''),
                         ifelse(attr(mongo, "password") != "", paste0(' --password="', attr(mongo, "password"), '"'), ''),
                         ' --upsert --type=json --file="', tempDir, '/allfiles.json"',
                         ifelse(checkMongoVersionOk(), '', ' --jsonArray'))
    #
    if (.Platform$OS.type == "windows") {
      #
      # euctr2json requires cygwin. transform paths for cygwin use, for testing:
      # euctr2json <- 'C:/Programme/R/R-3.2.2/library/ctrdata/exec/euctr2json.sh C:\\Temp\\RtmpUpg0Dt\\ctrDATAb83435686'
      euctr2json <- gsub("\\\\", "/", euctr2json)
      euctr2json <- gsub("([A-Z]):/", "/cygdrive/\\1/", euctr2json)
      euctr2json <- paste0('cmd.exe /c c:\\cygwin\\bin\\bash.exe --login -c "', euctr2json, '"')
      #
      json2mongo <- paste0(mongoBinaryLocation, json2mongo)
      #
    } else {
      #
      # mongoimport does not return exit value, hence redirect stderr to stdout
      json2mongo <- paste(json2mongo, '2>&1')
      #
    }
    #
    # run conversion
    message(paste0("Converting to JSON ..."))
    if (debug) message(euctr2json)
    imported <- system(euctr2json, intern = TRUE)
    #
    # run import
    message(paste0("Importing JSON into mongoDB ..."))
    if (debug) message(json2mongo)
    imported <- system(json2mongo, intern = TRUE)

    # find out number of trials imported into database
    imported <- as.integer(gsub(".*imported ([0-9]+) document.*", "\\1", imported[length(imported)]))
    if (!is.numeric(imported) || imported == 0) stop("Import has apparently failed, returned ", imported)
    message(paste0("Done - imported or updated ", imported, " records on ", resultsEuNumTrials, " trial(s)."))

    # clean up temporary directory
    if (!debug) unlink(tempDir, recursive = TRUE)

    # add query parameters to database
    dbCTRUpdateQueryHistory(register = register, queryterm = queryterm, recordnumber = imported, mongo = mongo, ns = ns)

    # update keys database
    dbFindCTRkey(forceupdate = TRUE, mongo = mongo, ns = ns)

  }

  ############################

  # return some useful information
  invisible(imported)
}
