## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
# library(ctrdata)
# citation("ctrdata")

## ----cite_ctrdata, eval=TRUE, results='asis', echo=c(-1)----------------------
cat(rev(format(citation("ctrdata"), style = "text")), sep = " or <br/>")

## -----------------------------------------------------------------------------
# # Please review and respect register copyrights:
# ctrOpenSearchPagesInBrowser(
#   copyright = TRUE
# )
# 
# # Open browser with example search:
# ctrOpenSearchPagesInBrowser(
#   url = "cancer&age=under-18&resultsstatus=trials-with-results",
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
# # * Checking trials in EUCTR, found 409 trials
# # [1] 409
# 
# # Connect to a database and chose a collection (table)
# db <- nodbi::src_sqlite(
#   dbname = "database_name.sql",
#   collection = "test"
# )
# 
# # Retrieve records, load into database
# ctrLoadQueryIntoDb(
#   queryterm = q,
#   con = db
# )
# # * Checking trials in EUCTR, found 409 trials
# # - Downloading in 21 batch(es) (20 trials each; estimate: 30 s)...
# # - Downloading 1628 records of 409 trials (estimate: 80 s)...
# # - Converting to NDJSON (estimate: 4 s)...
# # - Importing records into database...
# # = Imported or updated 1628 records on 409 trial(s)
# # No history found in expected format.
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 1628
# 
# # Show which queries have been downloaded into database
# dbQueryHistory(con = db)
# #       query-timestamp query-register query-records
# # 1 2026-03-07 16:51:19          EUCTR          1628
# #                                                    query-term
# # 1 query=cancer&age=under-18&resultsstatus=trials-with-results

## -----------------------------------------------------------------------------
# # Retrieve records, load into database
# ctrLoadQueryIntoDb(
#   queryterm = q,
#   euctrprotocolsall = FALSE,
#   con = db
# )
# # * Checking trials in EUCTR, found 409 trials
# # - Downloading in 21 batch(es) (20 trials each; estimate: 30 s)...
# # - Downloading 409 records of 409 trials (estimate: 20 s)...
# # - Converting to NDJSON (estimate: 1 s)...
# # - Importing records into database...
# # = Imported or updated 409 records on 409 trial(s)
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 409

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
# # * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results
# # * Query last run: 2026-03-07
# # * Checking for new or updated trials...
# # First result page empty - no (new) trials found?
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 0

## -----------------------------------------------------------------------------
# ctrLoadQueryIntoDb(
#   querytoupdate = "last",
#   euctrprotocolsall = FALSE,
#   forcetoupdate = TRUE,
#   euctrresults = TRUE,
#   con = db
# )
# # * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results
# # * Query last run: 2026-03-07
# # * Checking trials in EUCTR, found 409 trials
# # - Downloading in 21 batch(es) (20 trials each; estimate: 30 s)...
# # - Downloading 409 records of 409 trials (estimate: 20 s)...
# # - Converting to NDJSON (estimate: 1 s)...
# # - Importing records into database...
# # = Imported or updated 409 records on 409 trial(s)
# # * Checking results if available from EUCTR for 409 trials:
# # - Downloading results...
# # - Extracting results (. = data, F = file[s] and data, x = none): F F . . . . F
# # . . F . . . F . . F F F . . . . F F F . . . F . . . . F F . F . . . . . F . .
# # . . . . . . . . . F . . . . . . F . . . . . . . . . F . F . . . . . . . . . .
# # . . . . . . . F F . F . . . F . . . F F . . . . . . . . . . . . F F . F . . .
# # . . . . . . . F F . F F . . . . F F . . . F . F . . . F . . . . . . . . . . .
# # F . F . . . . . F . F . . . F F F . F . . F . F . . F . . . . . F F . F . . .
# # . . . . . . . . . F . . . . . . . . . . . . . . . . . . F . . . F . . . . . .
# # . . F . F . . . . . . . F . . . . . . . . . . . . . . . F F . . . . . . F . .
# # . . F F F . . . . . F F . . F . . . . . . . . . . . . F . . . . . F . . . . F
# # . . . F F . . F . . . . . . . . . . . . . F . . . . . F . . . . . . . F . . .
# # F . . F . . . F F . . . . . . F . F . . . . . F . . F . . . . F . . . . F F .
# # . . . F . . . . . . . .
# # - Data found for 409 trials
# # - Converting to NDJSON (estimate: 10 s)...
# # - Importing 409 results into database (may take some time)...
# # - Results history: not retrieved (euctrresultshistory = FALSE)
# # = Imported or updated results for 409 trials
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 409

## -----------------------------------------------------------------------------
# # Loading specific query into same collection
# ctrLoadQueryIntoDb(
#   queryterm = "cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com",
#   register = "CTGOV2",
#   con = db
# )
# #  Found search query from CTGOV2: cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# # * Checking trials in CTGOV2, found 113 trials
# # - Downloading in 1 batch(es) (max. 1000 trials each; estimate: 0.31 s)...
# # - Load and convert batch 1...
# # - Importing records into database...
# # JSON file #: 1 / 1
# # = Imported or updated 113 trial(s)
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 113
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
# # EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS
# #   180            0          105          105            2
# 
# # Overview of queries
# dbQueryHistory(con = db)
# #       query-timestamp query-register query-records
# # 1 2026-03-07 16:51:19          EUCTR          1628
# # 2 2026-03-07 17:04:08          EUCTR           409
# # 3 2026-03-07 17:05:22          EUCTR             0
# # 4 2026-03-07 17:08:23          EUCTR           409
# # 5 2026-03-07 17:09:27         CTGOV2           113
# # 6 2026-03-07 17:10:13          EUCTR           180
# # 7 2026-03-07 17:10:14         CTGOV2           105
# # 8 2026-03-07 17:10:15         CTGOV2           105
# # 9 2026-03-07 17:10:16           CTIS             2
# #
# # query-term
# # 1 query=cancer&age=under-18&resultsstatus=trials-with-results
# # 2 query=cancer&age=under-18&resultsstatus=trials-with-results
# # 3 query=cancer&age=under-18&resultsstatus=trials-with-results
# # 4 query=cancer&age=under-18&resultsstatus=trials-with-results
# # 5 cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# # 6 query=neuroblastoma&phase=phase-two&age=children&age=adolescent&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18&status=completed
# # 7 cond=neuroblastoma&intr=Drug OR Biological&term=AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)&aggFilters=phase:2,ages:child,status:com,studyType:int
# # 8 term=AREA[ConditionSearch]"neuroblastoma" AND (AREA[Phase]"PHASE2") AND (AREA[StdAge]"CHILD") AND (AREA[OverallStatus]"COMPLETED") AND (AREA[StudyType]INTERVENTIONAL) AND (AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)) AND (AREA[InterventionSearch](DRUG OR BIOLOGICAL))
# # 9 searchCriteria={"medicalCondition":"neuroblastoma","trialPhaseCode":[4],"ageGroupCode":[2],"status":[5,8]}

## -----------------------------------------------------------------------------
# # Annotate a query in CTGOV2 defined above
# ctrLoadQueryIntoDb(
#   queryterm = queries["CTGOV2"],
#   annotation.text = "site_DE ",
#   annotation.mode = "append",
#   con = db
# )
# # * Found search query from CTGOV2: cond=neuroblastoma&intr=Drug OR Biological&term=AREA[DesignPrimaryPurpose](DIAGNOSTIC OR PREVENTION OR TREATMENT)&aggFilters=phase:2,ages:child,status:com,studyType:int
# # * Checking trials in CTGOV2, found 105 trials
# # - Downloading in 1 batch(es) (max. 1000 trials each; estimate: 0.29 s)...
# # - Load and convert batch 1...
# # - Importing records into database...
# # JSON file #: 1 / 1
# # = Imported or updated 105 trial(s)
# # = Annotated retrieved records (105 records)
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 105

## -----------------------------------------------------------------------------
# # Search for synonyms
# ctrFindActiveSubstanceSynonyms(
#   activesubstance = "imatinib"
# )
# #  [1] "imatinib"          "Bosulif"           "Carcemia"          "CGP 57148"
# #  [5] "CGP 57148B"        "CGP57148"          "CGP57148B"         "Gleevac"
# #  [9] "Gleevec"           "Glevec"            "GLIVEC"            "Imarech"
# # [13] "Imat"              "Imatinib"          "Imatinib Mesylate" "Imkeldi"
# # [17] "Impentri"          "NSC #716051"       "NSC 716051"        "PegIntron"
# # [21] "QTI571"            "Sprycel"           "STI 571"           "STI571"
# # [25] "Tasigna"

## ----include=FALSE------------------------------------------------------------
# # cleanup
# unlink("database_name.sql")

## -----------------------------------------------------------------------------
# # Generate queries to identify trials
# queries <- ctrGenerateQueries(
#   searchPhrase = paste0(
#     "basket OR platform OR umbrella OR master protocol OR ",
#     "multiarm OR multistage OR subprotocol OR substudy OR ",
#     "multi-arm OR multi-stage OR sub-protocol OR sub-study"),
#   startAfter = "2015-01-01")
# 
# # See
# help("ctrGenerateQueries")
# 
# # Open queries in register web interface
# sapply(queries, ctrOpenSearchPagesInBrowser)
# 
# # Count number of studies found in the register
# result <- lapply(queries, ctrLoadQueryIntoDb, only.count = TRUE)
# 
# sapply(result, "[[", "n")
# # EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS
# #  1635          236         2507         2507          302
# 
# # Connect to a database and chose a collection (table)
# db <- nodbi::src_sqlite(
#   dbname = "database_name.sql",
#   collection = "test"
# )
# 
# # Load studies, include EUCTR results data for analysis
# result <- lapply(
#   queries, ctrLoadQueryIntoDb, con = db,
#   euctrprotocolsall = FALSE, euctrresults = TRUE)
# 
# sapply(result, "[[", "n")
# # EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS
# #  1633          236         2507         2507          302
# 
# # See next section for adding related trials

## -----------------------------------------------------------------------------
# # Use a trial concept to calculate related identifiers
# help("ctrdata-trial-concepts")
# 
# # Get data from trials loaded above
# df <- dbGetFieldsIntoDf(
#   fields = "ctrname",
#   calculate = c(
#     "f.isUniqueTrial",
#     "f.likelyPlatformTrial",
#     "f.trialTitle"
#   ),
#   con = db
# )
# # To review trial concepts details, call 'help("ctrdata-trial-concepts")'
# # Querying database (25 fields)...
# # Searching for duplicate trials...
# # - Getting all trial identifiers (may take some time), 4678 found in collection
# # - Finding duplicates among registers' and sponsor ids...
# # - Unique are 0 / 2507 / 149 / 474 / 202 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
# # = Returning keys (_id) of 3332 records in collection "test"
# # Searching for duplicate trials... ..
# # - Getting all trial identifiers, 4678 found in collection
# # Calculating f.trialTitle...
# 
# # Show names of calculated columns in the
# # data frame with possible platform trials
# names(df)
# # [1] "_id"
# # [2] "ctrname"
# # [3] ".isUniqueTrial"
# # [4] ".likelyPlatformTrial"
# # [5] ".likelyRelatedTrials"
# # [6] ".maybeRelatedTrials"
# # [7] ".trialTitle"
# 
# # Reduce to unique trials
# df <- df[df$.isUniqueTrial, ]
# nrow(df)
# # [1] 3332
# 
# # Number of recognised set of trials
# length(unique(df$.maybeRelatedTrials))
# # 224
# 
# # Trials with which _id are missing?
# missingIds <- unique(na.omit(setdiff(
#   unlist(df$.maybeRelatedTrials), df$`_id`)))
# 
# # Load missing trials by _id
# res <- list()
# for (i in seq_along(missingIds)) {
#   message(i, ": ", missingIds[i])
#   res <- c(res, suppressMessages(
#     list(ctrLoadQueryIntoDb(
#       missingIds[i], euctrresults = TRUE,
#       euctrprotocolsall = FALSE, con = db)
#     )))
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
#   "NCT00492167", "NCT00499616", "NCT00503724")
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

## ----include=FALSE------------------------------------------------------------
# # cleanup
# unlink("database_name.sql")

