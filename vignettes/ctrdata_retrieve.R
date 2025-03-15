## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
# library(ctrdata)
# citation("ctrdata")

## -----------------------------------------------------------------------------
# # Please review and respect register copyrights:
# ctrOpenSearchPagesInBrowser(
#   copyright = TRUE
# )
# 
# # Open browser with example search:
# ctrOpenSearchPagesInBrowser(
#   url = "cancer&age=under-18",
#   register = "EUCTR"
# )

## ----include=FALSE------------------------------------------------------------
# q <- "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&age=under-18&resultsstatus=trials-with-results"
# clipr::write_clip(q)

## -----------------------------------------------------------------------------
# q <- ctrGetQueryUrl()
# # * Using clipboard content as register query URL: https://www.clinicaltrialsregister.eu/
# # ctr-search/search?query=cancer&age=under-18&resultsstatus=trials-with-results
# # * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results
# 
# q
# #                                                    query-term  query-register
# # 1 query=cancer&age=under-18&resultsstatus=trials-with-results           EUCTR
# 
# # To check, this opens a browser with the query
# ctrOpenSearchPagesInBrowser(url = q)

## -----------------------------------------------------------------------------
# # Count number of trial records
# ctrLoadQueryIntoDb(
#   queryterm = q,
#   only.count = TRUE
# )$n
# # * Checking trials in EUCTR...
# # Retrieved overview, multiple records of 376 trial(s) from 19 page(s) to be
# # downloaded (estimate: 50 MB)
# # [1] 376
# 
# # Connect to a database and chose a collection (table)
# db <- nodbi::src_sqlite(
#   dbname = "database.sqlite",
#   collection = "test"
# )
# 
# # Retrieve records, download into database
# ctrLoadQueryIntoDb(
#   queryterm = q,
#   con = db
# )
# 
# # Show which queries have been downloaded into database
# dbQueryHistory(con = db)
# #       query-timestamp query-register query-records
# # 1 2025-03-02 13:05:08          EUCTR          1470
# #                                                    query-term
# # 1 query=cancer&age=under-18&resultsstatus=trials-with-results

## -----------------------------------------------------------------------------
# # Show all queries
# dbQueryHistory(con = db)
# 
# # Repeat last query
# ctrLoadQueryIntoDb(
#   querytoupdate = "last",
#   only.count = TRUE,
#   con = db
# )
# # First result page empty - no (new) trials found?
# # Updated history ("meta-info" in "test")

## -----------------------------------------------------------------------------
# Sys.time(); ctrLoadQueryIntoDb(
#   querytoupdate = "last",
#   forcetoupdate = TRUE,
#   euctrresults = TRUE,
#   con = db
# ); Sys.time();
# # * Checking trials in EUCTR...
# # Retrieved overview, multiple records of 376 trial(s) from 19 page(s) to be downloaded (estimate: 50 MB)
# # (1/3) Downloading trials...
# # Note: register server cannot compress data, transfer takes longer (estimate: 500 s)
# # (2/3) Converting to NDJSON (estimate: 8 s)...
# # (3/3) Importing records into database...
# # = Imported or updated 1470 records on 376 trial(s)
# # * Checking results if available from EUCTR for 376 trials:
# # (1/4) Downloading results...
# # Download status: 376 done; 0 in progress. Total size: 45.77 Mb (100%)... done!
# # - extracting results (. = data, F = file[s] and data, x = none):
# # . . . . . F . F . . . F . F . . F . F . . . . . . . . . . . . . . . F . . . .
# # . . . . . . . . . . . . . . . . . . F . . . . . . . . F . . . . . . . . . F . .
# # . . F . F . . . . . F . . . . . F . . . F . . . . . . . . . F F . F . . . . . F
# # . . . . . . F F . F F . . . . F F . . . F F . . . . . . . . . . . F . . . . . .
# # F . F . . . . . . . F . . . . . . F F . . . F . . F . . . . . F F F . . . . . .
# # . . F . F . . . . . F . . F . . . . F F F . . F . . . . F F . F . . . . . . . .
# # . . . . . . F . F . . . . . . . . . . F F . . . . . . . . F . F . . . . . . . F
# # . . F F . . . . F F . . F . . . . F F . . . F . . . . . . . . . . F . . . . . .
# # . . . . . . F . F . . . . . . . F . F . . . F . . . . . . . . . . . F . F . F .
# # . . F F F . . . . . . . . . . . F
# # (2/4) Converting to NDJSON (estimate: 40 s)...
# # (3/4) Importing results into database (may take some time)...
# # (4/4) Results history: not retrieved (euctrresultshistory = FALSE)
# # = Imported or updated results for 376 trials
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 1470

## -----------------------------------------------------------------------------
# # Loading specific query into same collection
# ctrLoadQueryIntoDb(
#   queryterm = "cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com",
#   register = "CTGOV2",
#   con = db
# )
# 
# # Use same query details to obtain queries
# queries <- ctrGenerateQueries(
#   condition = "neuroblastoma",
#   recruitment = "completed",
#   phase = "phase 2",
#   population = "P"
# )
# 
# # Open queries in registers' web interfaces
# sapply(queries, ctrOpenSearchPagesInBrowser)
# 
# # Load all queries into database collection
# result <- lapply(queries, ctrLoadQueryIntoDb, con = db)
# 
# # Show results of loading
# sapply(result, "[[", "n")
# # EUCTR CTGOV2 ISRCTN   CTIS
# #   180    111      0      1
# 
# # Overview of queries
# dbQueryHistory(con = db)
# #       query-timestamp query-register query-records
# # 1 2025-03-02 13:05:08          EUCTR          1470
# # 2 2025-03-02 13:26:49          EUCTR             0
# # 3 2025-03-02 13:31:53          EUCTR          1470
# # 4 2025-03-02 13:49:09         CTGOV2           111
# # 5 2025-03-02 13:56:01          EUCTR           180
# # 6 2025-03-02 13:56:02         CTGOV2           111
# # 7 2025-03-02 13:56:03           CTIS             1
# #                                                                                                   query-term
# # 1                                                query=cancer&age=under-18&resultsstatus=trials-with-results
# # 2                                                query=cancer&age=under-18&resultsstatus=trials-with-results
# # 3                                                query=cancer&age=under-18&resultsstatus=trials-with-results
# # 4                                                cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# # 5                             query=neuroblastoma&phase=phase-two&age=children&age=under-18&status=completed
# # 6                                                cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# # 7 searchCriteria={"medicalCondition":"neuroblastoma","trialPhaseCode":[4],"ageGroupCode":[2],"status":[5,8]}

## -----------------------------------------------------------------------------
# # Annotate a query in CTGOV2 defined above
# ctrLoadQueryIntoDb(
#   queryterm = queries["CTGOV2"],
#   annotation.text = "site_DE ",
#   annotation.mode = "append",
#   con = db
# )
# # * Appears specific for CTGOV REST API 2.0
# # * Found search query from CTGOV2: cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# # * Checking trials using CTGOV REST API 2.0, found 111 trials
# # (1/3) Downloading in 1 batch(es) (max. 1000 trials each; estimate: 11 Mb total)
# # (2/3) Converting to NDJSON...
# # (3/3) Importing records into database...
# # JSON file #: 1 / 1
# # = Imported or updated 111 trial(s)
# # = Annotated retrieved records (111 records)
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 111

## -----------------------------------------------------------------------------
# # Search for synonyms
# ctrFindActiveSubstanceSynonyms(
#   activesubstance = "imatinib"
# )
# #  [1] "imatinib"          "CGP 57148"         "CGP 57148B"
# #  [4] "CGP57148B"         "Gleevec"           "GLIVEC"
# #  [7] "Imatinib"          "Imatinib Mesylate" "NSC 716051"
# # [10] "ST1571"            "STI 571"           "STI571"

## ----include=FALSE------------------------------------------------------------
# # cleanup
# unlink("sqlite_file.sql")

## -----------------------------------------------------------------------------
# # Generate queries to identify trials
# urls <- ctrGenerateQueries(
#   searchPhrase = paste0(
#     "basket OR platform OR umbrella OR master protocol OR ",
#     "multiarm OR multistage OR subprotocol OR substudy OR ",
#     "multi-arm OR multi-stage OR sub-protocol OR sub-study"),
#   startAfter = "2010-01-01")
# 
# # See
# help("ctrGenerateQueries")
# 
# # Open queries in register web interface
# sapply(urls, ctrOpenSearchPagesInBrowser)
# 
# # Count number of studies found in the register
# result <- lapply(urls, ctrLoadQueryIntoDb, only.count = TRUE)
# 
# sapply(result, "[[", "n")
# # EUCTR CTGOV2 ISRCTN   CTIS
# #  2808   2377   1429    290
# 
# # and see examples in
# vignette("ctrdata_summarise")
# 
# # Load studies, include EUCTR results data for analysis
# result <- lapply(urls, ctrLoadQueryIntoDb, con = db, euctrresults = TRUE)
# 
# # See next section for adding related trials

## -----------------------------------------------------------------------------
# # Use a trial concept to calculate related identifiers
# help("ctrdata-trial-concepts")
# #
# dbQueryHistory(con = db)
# #
# df <- dbGetFieldsIntoDf(
#   fields = "ctrname",
#   calculate = c(
#     "f.isUniqueTrial",
#     "f.likelyPlatformTrial",
#     "f.trialTitle"
#   ),
#   con = db
# )
# # reduce to unique trials
# df <- df[df$.isUniqueTrial,]
# nrow(df)
# 
# # Number of recognised set of trials
# length(unique(df$.maybeRelatedTrial))
# # 571
# 
# # Trials with which _id are mission?
# missingIds <- na.omit(setdiff(unlist(df$.maybeRelatedTrial), df$`_id`))
# 
# # Load missing trials by _id
# res <- list()
# for (i in seq_along(missingIds)) {
#   message(i, ": ", missingIds[i])
#   res <- c(res, suppressMessages(
#     list(ctrLoadQueryIntoDb(missingIds[i], euctrresults = TRUE, con = db))))
# }
# 
# # Trials that could not be loaded are likely phase 1 trials
# # which are not publicly accessible in the in EUCTR register
# missingIds[which(sapply(res, "[[", "n") == 0L)]

## -----------------------------------------------------------------------------
# # ids of trials of interest
# ctIds <- c(
#   "NCT00001209", "NCT00001436", "NCT00187109", "NCT01516567", "NCT01471782",
#   "NCT00357084", "NCT00357500", "NCT00365755", "NCT00407433", "NCT00410657",
#   "NCT00436852", "NCT00445965", "NCT00450307", "NCT00450827", "NCT00471679",
#   "NCT00492167", "NCT00499616", "NCT00503724"
# 
# )
# 
# # split into sets of each 10 trial ids
# # (larger sets e.g. 50 may still work)
# idSets <- split(ctIds, ceiling(seq_along(ctIds) / 10))
# 
# # variable to collect import results
# result <- NULL
# 
# # iterate over sets of trial ids
# for (idSet in idSets) {
# 
#   setResult <- ctrLoadQueryIntoDb(
#     queryterm = paste0("term=", paste0(idSet, collapse = " ")),
#     register = "CTGOV2",
#     con = db
#   )
# 
#   # check that queried ids have
#   # successfully been loaded
#   stopifnot(identical(
#     sort(setResult$success), sort(idSet)))
# 
#   # append result
#   result <- c(result, list(setResult))
# }
# 
# # inspect results
# as.data.frame(do.call(rbind, result))[, c("n", "failed")]
# #    n failed
# # 1 10   NULL
# # 2  8   NULL
# 
# # queryterms for other registers for retrieving trials by their identifier:
# #
# # CTIS (note the comma separated values):
# # https://euclinicaltrials.eu/ctis-public/search#searchCriteria=
# # {"containAny":"2025-521008-22-00, 2024-519446-67-00, 2024-517647-31-00"}
# #
# # EUCTR (note the country suffix os to be removed, values separated with OR):
# # https://www.clinicaltrialsregister.eu/ctr-search/search?
# # query=2008-001606-16+OR+2008-001721-34+OR+2008-002260-33
# #

## ----remote_mongo-------------------------------------------------------------
# # Specify base uri for remote MongoDB server,
# #  as part of the encoded connection string
# db <- nodbi::src_mongo(
#   # Note: this provides read-only access
#   url = "mongodb+srv://DWbJ7Wh:bdTHh5cS@cluster0-b9wpw.mongodb.net",
#   db = "dbperm",
#   collection = "dbperm")
# 
# # Since the above access is read-only,
# # just obtain fields of interest:
# dbGetFieldsIntoDf(
#   fields = c("a2_eudract_number",
#              "e71_human_pharmacology_phase_i"),
#   con = db)
# #                  _id a2_eudract_number e71_human_pharmacology_phase_i
# # 1 2010-024264-18-3RD    2010-024264-18                           TRUE
# # 2  2010-024264-18-AT    2010-024264-18                           TRUE
# # 3  2010-024264-18-DE    2010-024264-18                           TRUE
# # 4  2010-024264-18-GB    2010-024264-18                           TRUE
# # 5  2010-024264-18-IT    2010-024264-18                           TRUE
# # 6  2010-024264-18-NL    2010-024264-18                           TRUE

