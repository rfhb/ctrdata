## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## --------------------------------------------------------------------------------------------------------------------------------------------------
# library(ctrdata)
# citation("ctrdata")

## --------------------------------------------------------------------------------------------------------------------------------------------------
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

## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------
# q <- "https://www.clinicaltrialsregister.eu/ctr-search/search?query=cancer&age=under-18&resultsstatus=trials-with-results"
# clipr::write_clip(q)

## --------------------------------------------------------------------------------------------------------------------------------------------------
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

## --------------------------------------------------------------------------------------------------------------------------------------------------
# # Count number of trial records
# ctrLoadQueryIntoDb(
#   queryterm = q,
#   only.count = TRUE
# )$n
# # * Checking trials in EUCTR...
# # Retrieved overview, multiple records of 390 trial(s) from 20 page(s) to be
# # downloaded (estimate: 50 MB)
# # [1] 390
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
# 
# # Show which queries have been downloaded into database
# dbQueryHistory(con = db)
# #       query-timestamp query-register query-records
# # 1 2025-06-29 15:06:10          EUCTR          1541
# #                                                    query-term
# # 1 query=cancer&age=under-18&resultsstatus=trials-with-results

## --------------------------------------------------------------------------------------------------------------------------------------------------
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
# # * Query last run: 2025-06-29 15:30:47
# # * Checking for new or updated trials...
# # Rerunning query: query=cancer&age=under-18&resultsstatus=trials-with-results
# # Last run: 2025-06-29
# # * Checking trials in EUCTR...
# # Retrieved overview, multiple records of 2 trial(s) from 1 page(s) to be downloaded (estimate: 0.3 MB)
# # $n
# # [1] 2

## --------------------------------------------------------------------------------------------------------------------------------------------------
# ctrLoadQueryIntoDb(
#   querytoupdate = "last",
#   forcetoupdate = TRUE,
#   euctrresults = TRUE,
#   con = db
# )
# # * Found search query from EUCTR: query=cancer&age=under-18&resultsstatus=trials-with-results
# # * Query last run: 2025-06-29 15:30:47
# # * Checking trials in EUCTR...
# # Retrieved overview, multiple records of 390 trial(s) from 20 page(s) to be downloaded (estimate: 50 MB)
# # Note: Server cannot compress data, transfer takes longer (estimate: 500 s)
# # - Downloading trial pages...
# # - Converting to NDJSON (estimate: 8 s)...
# # - Importing records into database...
# # = Imported or updated 1541 records on 390 trial(s)
# # * Checking results if available from EUCTR for 390 trials:
# # - Downloading results...
# # - Extracting results (. = data, F = file[s] and data, x = none): . . . . . F
# # . . F . . . . F . . . . F . . F . . . . . . . . . F . F . . . . . . . . . .
# # . F . . . . . . . . . . . . . . . F . F . . . . . F . F . . . F . F . . . .
# # . F . F . . . F . . . . . . . . . . . . . . . F . . . F F . . . . . . F . F
# # . . F . . . . . . . . . . . F . . . . . . . . . . . . . . . . F F . F . . .
# # F . . . F F . . . . . . . . . . . F F . F . . . . . . F . . . F F . . F . .
# # . . . . . . . . . . F . . . . . F . . . . . . F F . F . . F . . . . F F F .
# # . . . . . . F . . F . . . . . . . . . . . F . . . . . . . . . . F . . . . .
# # . . . . . . . . F F . . . . F . . F . . . F . . F F F . . . . . F . . . . .
# # . . . . . . F . F . . . . . . . . . . F F . F F . . . . F F . . . F F F . .
# # F . . . . F F . F . . . . . . F . . . . . . . . F . . . . . . F . . F F F .
# # . . . F
# # - Converting to NDJSON (estimate: 40 s)...
# # - Importing results into database (may take some time)...
# # - Results history: not retrieved (euctrresultshistory = FALSE)
# # = Imported or updated results for 390 trials
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 1541

## --------------------------------------------------------------------------------------------------------------------------------------------------
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
# # EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS
# #   180            0          110          110            1
# 
# # Overview of queries
# dbQueryHistory(con = db)
# #       query-timestamp query-register query-records
# # 1 2025-06-29 15:30:47          EUCTR          1541
# # 2 2025-06-29 15:34:46          EUCTR          1541
# # 3 2025-06-29 15:36:24         CTGOV2           110
# # 4 2025-06-29 15:37:46          EUCTR           180
# # 5 2025-06-29 15:37:47         CTGOV2           110
# # 6 2025-06-29 15:37:48         CTGOV2           110
# # 7 2025-06-29 15:37:49           CTIS             1
# # query-term
# # 1 query=cancer&age=under-18&resultsstatus=trials-with-results
# # 2 query=cancer&age=under-18&resultsstatus=trials-with-results
# # 3 cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com
# # 4 query=neuroblastoma&phase=phase-two&age=children&age=adolescent&age=infant-and-toddler&age=newborn&age=preterm-new-born-infants&age=under-18&status=completed
# # 5 cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com,studyType:int
# # 6 term=AREA[ConditionSearch]"neuroblastoma" AND (AREA[Phase]"PHASE2") AND (AREA[StdAge]"CHILD") AND (AREA[OverallStatus]"COMPLETED") AND (AREA[StudyType]INTERVENTIONAL)
# # 7 searchCriteria={"medicalCondition":"neuroblastoma","trialPhaseCode":[4],"ageGroupCode":[2],"status":[5,8]}

## --------------------------------------------------------------------------------------------------------------------------------------------------
# # Annotate a query in CTGOV2 defined above
# ctrLoadQueryIntoDb(
#   queryterm = queries["CTGOV2"],
#   annotation.text = "site_DE ",
#   annotation.mode = "append",
#   con = db
# )
# # * Found search query from CTGOV2: cond=neuroblastoma&aggFilters=phase:2,ages:child,status:com,studyType:int
# # * Checking trials in CTGOV, found 110 trials
# # - Downloading in 1 batch(es) (max. 1000 trials each; estimate: 11 Mb total)
# # - Converting to NDJSON...
# # - Importing records into database...
# # JSON file #: 1 / 1
# # = Imported or updated 110 trial(s)
# # = Annotated retrieved records (110 records)
# # Updated history ("meta-info" in "test")
# # $n
# # [1] 110

## --------------------------------------------------------------------------------------------------------------------------------------------------
# # Search for synonyms
# ctrFindActiveSubstanceSynonyms(
#   activesubstance = "imatinib"
# )
# #  [1] "imatinib"          "CGP 57148"         "CGP 57148B"
# #  [4] "CGP57148B"         "Gleevec"           "GLIVEC"
# #  [7] "Imatinib"          "Imatinib Mesylate" "NSC 716051"
# # [10] "ST1571"            "STI 571"           "STI571"

## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------
# # cleanup
# unlink("database_name.sql")

## --------------------------------------------------------------------------------------------------------------------------------------------------
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
# #  1635          165         1559         1559          237
# 
# # Note that for EUCTR, the number of trials is shown;
# # since each trial can have multiple records in this
# # register, the number is higher when actually loaded
# 
# # and see examples in
# vignette("ctrdata_summarise")
# 
# # Connect to a database and chose a collection (table)
# db <- nodbi::src_sqlite(
#   dbname = "database_name.sql",
#   collection = "test"
# )
# 
# # Load studies, include EUCTR results data for analysis
# result <- lapply(queries, ctrLoadQueryIntoDb, con = db, euctrresults = TRUE)
# 
# sapply(result, "[[", "n")
# # EUCTR       ISRCTN       CTGOV2 CTGOV2expert         CTIS
# #  7673          165         1559         1559          237
# 
# # See next section for adding related trials

## --------------------------------------------------------------------------------------------------------------------------------------------------
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
# # Querying database (24 fields)...
# # Searching for duplicate trials...
# # - Getting all trial identifiers (may take some time), 9634 found in collection
# # - Finding duplicates among registers' and sponsor ids...
# # - 5599 EUCTR _id were not preferred EU Member State record for 1766 trials
# # - Keeping 1559 / 1480 / 0 / 123 / 95 records from CTGOV2 / EUCTR / CTGOV / ISRCTN / CTIS
# # = Returning keys (_id) of 3257 records in collection "test"
# # Searching for duplicate trials... ..
# # - Getting all trial identifiers, 9634 found in collection
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
# 
# # Number of recognised set of trials
# length(unique(df$.maybeRelatedTrials))
# # 183
# 
# # Trials with which _id are missing?
# missingIds <- na.omit(setdiff(unlist(df$.maybeRelatedTrials), df$`_id`))
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

## --------------------------------------------------------------------------------------------------------------------------------------------------
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

## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------
# # cleanup
# unlink("database_name.sql")

## ----remote_mongo----------------------------------------------------------------------------------------------------------------------------------
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
#   fields = c(
#     "a2_eudract_number",
#     "e71_human_pharmacology_phase_i"),
#   con = db)
# #                  _id a2_eudract_number e71_human_pharmacology_phase_i
# # 1 2010-024264-18-3RD    2010-024264-18                           TRUE
# # 2  2010-024264-18-AT    2010-024264-18                           TRUE
# # 3  2010-024264-18-DE    2010-024264-18                           TRUE
# # 4  2010-024264-18-GB    2010-024264-18                           TRUE
# # 5  2010-024264-18-IT    2010-024264-18                           TRUE
# # 6  2010-024264-18-NL    2010-024264-18                           TRUE

