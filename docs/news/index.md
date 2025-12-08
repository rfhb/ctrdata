# Changelog

## ctrdata 1.25.1.9000

- new development version

## ctrdata 1.25.1

### Enhancement

- Work around upstream issue
  ([\#59](https://github.com/rfhb/ctrdata/issues/59), CTIS delivering at
  most 10000 trials per query)
- Add CITATION file with recent reference publication

### Bug fix

- Re-added field missing from EUCTR conversion
- Changed API call after long deprecated by ISRCTN
- Ordered messages from
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md),
  [`f.isUniqueTrial()`](https://rfhb.github.io/ctrdata/reference/f.isUniqueTrial.md)
- Adapt to extended tokens used by CTGOV2

## ctrdata 1.25.0

CRAN release: 2025-08-26

### Possibly breaking down-stream workflows

- Save EUCTR result files in folders by trial, as done for documents for
  all registers

### Enhancement

- Revised how EUCTR trials are loaded and updated with results,
  accelerated operations
- Added retrying requests, to handle refused or throttled responses
  (e.g., CTIS, [\#55](https://github.com/rfhb/ctrdata/issues/55))
- Added incrementing numbers of historic versions of CTIS when running
  [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
- Simplified, accelerated loading NDJSON files, minimising database
  record deletions
- Changed deleting and preserving temporary files (see also news for
  version 1.17.0)

### Bug fixes

- Refined indicator variables for
  [`f.hasResults()`](https://rfhb.github.io/ctrdata/reference/f.hasResults.md)
- Added missing status code in
  [`f.statusRecruitment()`](https://rfhb.github.io/ctrdata/reference/f.statusRecruitment.md)
- Corrected missing value handling in
  [`f.sponsorType()`](https://rfhb.github.io/ctrdata/reference/f.sponsorType.md)

## ctrdata 1.24.1

CRAN release: 2025-07-25

- Bug fix for
  [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  (country reference list, parameter chaining)
- Handle conversion of numbers with thousands separator
- Fixed documentation gaps

## ctrdata 1.24.0

CRAN release: 2025-07-21

- Added parameter `euctrprotocolsall` for loading from EUCTR; if
  `FALSE`, this speeds up loading as only a single and no all records of
  a trial are loaded
- Tightened query for medicine-interventional trials in
  [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  for CTGOV2
- Added trial concepts
  [`f.assignmentType()`](https://rfhb.github.io/ctrdata/reference/f.assignmentType.md),
  [`f.externalLinks()`](https://rfhb.github.io/ctrdata/reference/f.externalLinks.md),
  [`f.hasResults()`](https://rfhb.github.io/ctrdata/reference/f.hasResults.md)
- Improved trial concept
  [`f.controlType()`](https://rfhb.github.io/ctrdata/reference/f.controlType.md)
  and added “mixed”to
  [`f.sponsorType()`](https://rfhb.github.io/ctrdata/reference/f.sponsorType.md)
- Expanded functionality of
  [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  for CTGOV2 expert searches
- Bug fix for subsetting data frames when calculating trial concepts
- Removed long deprecated parameters from
  [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md),
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
- Increased testing coverage to more than 94% of code lines

## ctrdata 1.23.0

CRAN release: 2025-07-02

### Improvements

- Refactored for EUCTR protocol data
  [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  for large speed gain
- Improved downloading more documents from ISRCTN, however needs live
  webpage read
- Replaced `curl` with `httr2` and further refactored file retrieval
- Reduced dependencies using equivalent alternative functions

### Bug fixes

- Cover multiple sponsors in EUCTR for
  [`f.sponsorType()`](https://rfhb.github.io/ctrdata/reference/f.sponsorType.md)
- Change priority of CTIS fields for
  [`f.sponsorType()`](https://rfhb.github.io/ctrdata/reference/f.sponsorType.md)
- Added typing of newly appearing CTIS fields

## ctrdata 1.22.3

CRAN release: 2025-06-08

- Added dev version of `nodbi` to GitHub Action
- Added typing to CTIS field for `f.statusRecruitment`
- Improved `f.likelyPlatfromTrial` (mangling title excerpts, add a
  missing default, sort ids in `.maybeRelatedTrials`, added back
  associated trials, updated similarity and user info)
- Accelerated file operations that still used `cat`

## ctrdata 1.22.2

CRAN release: 2025-05-17

- Added typing for further CTGOV2 and CTIS fields
- Adapt a test to modified terminal output in certain IDEs
- Refactored
  [`dfMergeVariablesRelevel()`](https://rfhb.github.io/ctrdata/reference/dfMergeVariablesRelevel.md)
  and keep type if rows include only a single value
- Fixed counting trial sites outside the EEA for EUCTR
- Fixed regression in
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
- Fixed typo in
  [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
- Updated documentation

## ctrdata 1.22.1

CRAN release: 2025-05-05

### Bug fix

- Adapt testing after SQLite 3.48.0 (in RSQLite 2.3.10 since 2025-05-02)
  accepts now up to 1000 arguments
- Fixed EUCTR conversion to `NDJSON` in Javascript for rare specific
  trial(s), thanks [@machado-t](https://github.com/machado-t)
  ([\#45](https://github.com/rfhb/ctrdata/issues/45))
- Revised
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
  to work with more than 80,000 records, thanks
  [@machado-t](https://github.com/machado-t)
  ([\#48](https://github.com/rfhb/ctrdata/issues/48))

### Internal

- Modify internal function `.dbMapIdsTrials()` to use both CTIS1 and
  CTIS2 field names, and defer de-duplication to parent function

## ctrdata 1.22.0

CRAN release: 2025-04-14

### Improvements

- When using `querytoupdate` for CTIS, set `ctishistory = TRUE` to
  create historic versions from previously downloaded trial record, see
  [`vignette("ctrdata_summarise")`](https://rfhb.github.io/ctrdata/articles/ctrdata_summarise.md)
- Trial concept
  [`f.trialObjectives()`](https://rfhb.github.io/ctrdata/reference/f.trialObjectives.md)
  improved with structured data from CTIS and EUCTR
- [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  now defaults to interventional trials with medicines
- [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  additionally provides a CTGOV expert search query
- [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  now also covers countries of trial conduct

### Bug fix

- Fixed `ISRCTN` API term mangling
- Fixed digesting marginal cases in notifications in
  [`f.statusRecruitment()`](https://rfhb.github.io/ctrdata/reference/f.statusRecruitment.md)
- Fixed `ctrGenerateQueries` to handle ‘or’ as needed for certain
  filters

## ctrdata 1.21.1

CRAN release: 2025-03-15

### Bug fix

- Fixed all `f...` clinical trial concept functions to handle NAs,
  included in testing
- Fixed round brackets for nesting logical operators in `queryterm`,
  thanks [@jacksknnr](https://github.com/jacksknnr)
  ([\#46](https://github.com/rfhb/ctrdata/issues/46))
- Fixed CRAN NOTE on package size

## ctrdata 1.21.0

CRAN release: 2025-03-09

### Improvements

- New feature: built-in functions that implement and calculate common
  trial concepts across different registers, see
  [`help("ctrdata-trial-concepts")`](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
  which should largely avoid that users have to code themselves and
  repeatedly common trial concepts, such as recruitment start date,
  trial investigating a medicine as intervention, trial being a platform
  trial etc.
- New feature: function
  [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  to search all registers from a user’s single input, e.g. intervention,
  phase, dates
- Refactored
  [`dfMergeVariablesRelevel()`](https://rfhb.github.io/ctrdata/reference/dfMergeVariablesRelevel.md),
  factored out returning data frame or tibble, thanks
  [@machado-t](https://github.com/machado-t)
  ([\#45](https://github.com/rfhb/ctrdata/issues/45))

### Bug fix

- [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
  excludes GB records from `EUCTR` whenever there is at least one Member
  State or Third country record
- `EUCTR` no longer allows to search for the status of a clinical trial,
  adapted testing to it
- Correct
  [`ctrShowOneTrial()`](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
  and
  [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  for certain EUCTR trials
- Early stopping if database collection is read-only
- Added typing of fields newly appearing in CTIS
- Documentation simplification and update
- Changing vignettes from PDF to HTML

## ctrdata 1.20.0

CRAN release: 2024-12-14

### Improvements

- New function
  [`ctrShowOneTrial()`](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
  to inspect a trial structure and data, and to select fields for
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md);
  in this context, make
  [`ctrGetQueryUrl()`](https://rfhb.github.io/ctrdata/reference/ctrGetQueryUrl.md)
  work for identifiers of single trials
- Improve and update documentation

### Bug fix

- Refactor `dfTrials2Long` to correct generating identifiers for
  single-item groups
- Correct using certain testing conditions

## ctrdata 1.19.5

CRAN release: 2024-11-10

### Improvements

- Refactored
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  for speed and efficiency
- Adapted to newly available expert search in `CTGOV2`

### Bug fix

- Correct `CTGOV2` endpoint for document downloads
- Adapted to `curl` v6 having renamed the timeout parameter
- Documents download (folders only for trials with documents; excluding
  duplicates)

## ctrdata 1.19.4

CRAN release: 2024-09-29

- Fix vignettes and update documentation website (pkgdown)
- Make
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  more robust, specify scalar search fields to identify records

## ctrdata 1.19.3

CRAN release: 2024-09-22

- Improve Tampermonkey script (this serves to copy searches in any
  register to the clipboard, and also can open a `CTIS` search from an
  URL, see
  <https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser>
- Refactored parts of documents downloading for handling CDN links
- Simplify code now that `CTIS` provides an advanced search
- Code styling and linting

## ctrdata 1.19.2

CRAN release: 2024-08-28

### Improvements

- Added typing for newly appearing variables in `CTIS`
- Accelerated `CTIS` trial data processing

### Bug fixes

- Fix incomplete downloads from `CTIS` by disabling HTTP/2 multiplexing
  in another function

## ctrdata 1.19.1

CRAN release: 2024-07-18

### Improvements

- Revised translation of location elements from search URL to API call
  for `CTGOV2`

## ctrdata 1.19.0

CRAN release: 2024-06-30

### Possibly breaking changes

- `CTGOV` has retired on 2024-06-25 the classic website and API used by
  `ctrdata` since 2015. To support users, `ctrdata` now automatically
  translates and redirects queries to the current website. This helps
  with automatically updating previously loaded queries
  (`ctrLoadQueryIntoDb(querytoupdate = <n>)`), manually migrating
  queries and reproducible work on clinical trials information. This new
  functionality in `ctrdata` translates a user’s search query URL from
  the classic website into a query for the current `CTGOV` website, for
  all search parameters. Since the structure and format of data differs
  between data retrieved from the current API and previously retrieved
  from the classic API, `ctrdata` will continue to identify the current
  API as `register = "CTGOV2"`, to support the analysis stage. In
  addition, `ctrdata` documentation continues to include examples of
  analyses with `CTGOV` data, as this may have been downloaded earlier.
- `CTIS` has been relaunched on 2024-06-17, and `ctrdata` has been fully
  updated to it. At the moment, `CTIS` provides basic searches and no
  search query URL. To support users, `ctrdata` includes an updated
  script that extracts a user’s search parameters from the register
  search page to the clipboard and into the browser URL bar. In
  addition, the script triggers a search for trials when opening such a
  query URL, see
  <https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser>.
  File names of `CTIS` documents start now with the document type,
  e.g. `SbjctInfaICF - L1 SIS and ICF NL for publication.pdf`, since the
  prefix (document source) introduced in version 1.18.0 is no more
  applicable.

### Improvements

- Reduced size of demo database in package, addressing `CRAN` notes
- Adapted
  [`ctrFindActiveSubstanceSynonyms()`](https://rfhb.github.io/ctrdata/reference/ctrFindActiveSubstanceSynonyms.md)
  to `CTGOV2` API; this is now based on terms used in studies
- Updated vignette and added inspecting a specific trial
- Updated register information, adding caveats and issues
- Updated estimated download sizes and user information
- Avoided duplicate data and file retrievals
- New attribute `ctrdata-collection` for data frames with trial
  information; same content as attribute `ctrdata-table`, which will be
  removed by end 2024
- Split utils.R into files for functions and fields

### Bug fixes

- Correct partial date brackets for `CTGOV2`
- Disable HTTP/2 multiplexing for `CTIS`

## ctrdata 1.18.0 (2024-05-13)

CRAN release: 2024-05-13

### Possibly breaking change

- File names for documents downloaded from `CTIS` now include the
  document type and use different separators (e.g., “parts2auth -
  SbjctInfaICF - …” as abbreviation of “Subject information and informed
  consent form (for publication)”, was previously “parts2auth\_…”)

### Improvements

- Add retrieval of historic versions of trial records from `CTGOV2`
  (automatically retrieved from `CTIS`, not available for other
  registers)
- Added typing of newly appearing fields from `CTIS`
- Increase testing coverage to 93.7% locally
- Added missing CI for the combination of `DuckDB` and `CTGOV2`

### Bug fixes

- Correct typing certain fields as (lists of) integers
- Handle marginal case in
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
- Changed `unlink` of intermediate files
- Minor vignettes corrections

## ctrdata 1.17.2 (2024-02-25)

CRAN release: 2024-02-25

### Bug fixes

- Switch sequence of API endpoints used with `CTIS`
- Correct handling multiple public events with `CTIS`
- Re-use `CTIS` downloads in a given session

## ctrdata 1.17.1 (2024-02-05)

CRAN release: 2024-02-04

### Improvements

- Additional `CTIS` field types and possibly documents (associated
  clinical trials)
- Use `ctId` instead of `id` as `CTIS` top-level field (a further
  synonym used in the API is `clinicalTrialId`)

### Bug fixes

- Added handling of unexpected Unicode in protocol-related data from one
  trial in `EUCTR`
- Improved predicted download sizes and times for `EUCTR`
- Return with message if `CTIS` query did not result in any trials

## ctrdata 1.17.0 (2024-01-22)

CRAN release: 2024-01-22

### Possibly breaking changes

- Reimplemented
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  to accelerate and have more predictable, simplified returns, in
  particular for nested fields; also attempts to recursively expand
  simply nested data into additional columns in the returned data frame
- Reimplemented
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  to accelerate; both based on improved
  [`nodbi::docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.html)
- [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  now digests a sample of records to quickly find fields, or all records
  if `sample = FALSE` but taking increasing time with increasing number
  of records
- If using `nodbi::scr_postgres()`, parameter `fields` of
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  is limited to less than 50 fields; a message flags for any backend
  potential compatibility issues, suggesting to use parent fields, e.g.,
  `a.b` instead of `c("a.b.c.d", "a.b.c.e")`
- Parameter `stopifnodata` of
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  is no more needed and deprecated
- Reimplemented typing fields to speed up and to simplify

### Improvements

- Register data are re-used and not downloaded again in an interactive
  session (that is, the same temporary folder is now re-used throughout
  a user’s session)
- Temporary folder can be set by users with
  `options(ctrdata.tempdir = "<user_specified_folder>")`
- Inform MS Windows users if `cygwin` was found so that they may chose
  to delete it
- Many fields added for typing e.g. as date in
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)

### Bug fixes

- Adapted and corrected information loading to newly available data in
  `CTIS`
- Corrected escaping, and back-conversion, of characters in `JSON` from
  `CTIS`

## ctrdata 1.16.0 (2023-11-24)

CRAN release: 2023-11-24

### Possibly breaking changes

#### XML files are converted slightly differently

- EUCTR result-related information in attributes: e.g. new:
  `{"id":"PostAssignmentPeriod-46349"}`, was:
  `{"@attributes":{"id":"PostAssignmentPeriod-46349"}}`
- Consequently, it should work to just delete `@attributes` from field
  names such as
  `dbGetFieldsIntoDf("clinical_results.baseline.analyzed_list.analyzed.count_list.count.@attributes.value", db)`
- EUCTR protocol-related information although no differences were found
  yet
- CTGOV attributes of bare values remain not included in the resulting
  `NDJSON` (e.g., some but not all records have
  `<start_date type="Actual">March 15, 2004</start_date>`, and this is
  converted to `{"start_date":"March 15, 2004"}`)

#### EUCTR: some renaming to harmonise EU and 3rd country trial fields

- new: `e83_single_site_trial`, was (EU trials):
  `e83_the_trial_involves_single_site_in_the_member_state_concerned`
- new: `e83_single_site_trial`, was (3rd country trials):
  `e83_will_this_trial_be_conducted_ at_a_single_site_globally`
- new: `e863_trial_sites_planned_in`, was (EU trials):
  `e863_specify_the_regions_in_which_trial_sites_are_planned`
- new: `e863_trial_sites_planned_in`, was (3rd country trials):
  `e863_specify_the_countries_outside_of_the_eea_in_which_trial_sites_are_planned`
- new: `e84_multiple_sites_in_member_state`, was (EU trials):
  `e84_the_trial_involves_multiple_sites_in_the_member_state_concerned`
- new: `e840_multiple_sites_globally`, was (3rd country trials):
  `e84_will_this_trial_be_conducted_at_multiple_sites_globally`

See also
<https://github.com/rfhb/ctrdata/issues/26#issuecomment-1749555081>

### Bug fixes

- corrected batch iterations over CTIS trials accommodating unclear
  `totalSize` response
- corrected translation of some fields from the browser URL to the API
  call for CTGOV2 (closes <https://github.com/rfhb/ctrdata/issues/32>)
- corrected minimum curl version to 5.1.0 (closes
  <https://github.com/rfhb/ctrdata/issues/31>)
- handled errors when saving EUCTR results (e.g., too long file path
  name, closes <https://github.com/rfhb/ctrdata/issues/30> and
  <https://github.com/rfhb/ctrdata/issues/28>)

### Improvements

#### Major

- **No external tools required any more** (`Cygwin`, `perl`, `cat`,
  `sed`, `php` functionality for transforming text, XML and `NDJSON`
  replaced by Javascript using `R` package `V8`); addresses personally
  communicated concerns and faciliates use of package `ctrdata` in more
  environments (e.g., <https://github.com/rfhb/ctrdata/issues/26>);
  consequently, this might be a breaking change for analysing certain
  fields, see above which fields are affected.

#### Other

- added results summary download for CTIS
- added documents download for ISRCTN
- factored out document download function
- ensure
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  returns fields for EU and 3rd country trials in EUCTR (addresses
  <https://github.com/rfhb/ctrdata/issues/26>)
- changed order of importing from CTIS into database, improved speed
- better checked data downloads, and repeat them where necessary
- factored out temporary folder creation
- added using `options(ctrdata.tempdir = ...)` if set
- removed `dfListExtractKey()`, long deprecated
- removed `dfMergeTwoVariablesRelevel()`, long deprecated
- reorganised code file layout

## ctrdata 1.15.2 (2023-09-10)

CRAN release: 2023-09-17

- fix handling as utf8 upstream multi-language strings from CTIS
- correct creating lists for downloading documents for ctis
- adding missing endpoints for CTIS found with increasing amount of
  data, e.g. publicevents.temporaryHaltList.details for
  2022-501559-99-00

## ctrdata 1.15.1 (2023-08-29)

CRAN release: 2023-08-30

- correct LaTeX documentation resulting in CRAN error
- correct parts of downloading from `CTIS`, including file name
  sanitisation

## ctrdata 1.15.0 (2023-08-27)

CRAN release: 2023-08-27

- added CTGOV REST API 2.0.0.-test as new register with identifier
  `CTGOV2`
- handle CTGOV classic interface as register `CTGOV`
- improved `ctrdataURLcopier.js` to only rewrite searches and views from
  `CTIS`
- mangle CTIS: change `partIIInfo` object into array, adding a new
  `partIIIinfoKey` so that
  `'{"partIIInfo": "<int>": {...}, "<int>": {...}}'` becomes
  `'{"partIIInfo": [{"partIIIinfoKey": <int>, ...}, {"partIIIinfoKey": <int>, ...}]}')`
- correct dbGetFieldsIntoDf() for specific lists

## ctrdata 1.14.0 (2023-07-16)

CRAN release: 2023-07-16

- fix
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
  for single-record register contents
- expand number of CTIS variables that are typed as date
- [`dfMergeVariablesRelevel()`](https://rfhb.github.io/ctrdata/reference/dfMergeVariablesRelevel.md)
  superseeds `dfMergeTwoVariablesRelevel()`

## ctrdata 1.13.3 (2023-06-24)

CRAN release: 2023-06-24

- typo in
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
- use only CTGOV classic website (ctrdata is being adapted to new
  website)
- correct missing names and attributes on return vector of
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)

## ctrdata 1.13.2 (2023-05-27)

CRAN release: 2023-05-26

- correct selection of lists with ids for documents to download from
  CTIS
- reduce dependencies (rvest, dplyr removed)

## ctrdata 1.13.1 (2023-05-07)

CRAN release: 2023-05-07

- load more CTIS data (publicEvaluation) and download documents
- integrate downloading documents into ctrLoadQueryIntoDb() also for
  CTGOV
- use documents.path for CTGOV, EUCTR, CTIS; deprecated
  euctrresultsfilespath
- added documents.regexp to select documents for downloading based on
  their file name

## ctrdata 1.13.0 (2023-04-23)

CRAN release: 2023-04-24

- data from CTIS is imported more completely
- adapt other functions to accommodate CTIS
- provide Tampermonkey script to get the URL of a user’s query in a
  register
- speed up
  [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  for CTIS with nodbi \>=0.9.2.9000
- keep register names on vector returned by
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
- correct
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  for EUCTR

## ctrdata 1.12.1 (2023-03-29)

CRAN release: 2023-03-29

- fix escaping hash symbol in PDF rendition of an help page
- fix file encoding for CTIS downloads under MS Windows

## ctrdata 1.12.0 (2023-03-25)

CRAN release: 2023-03-26

- added first access to new register: CTIS, the EU Clinical Trial
  Information System
- stop (instead of warning) if register host errors (e.g. incorrect
  number of records)
- switch to use
  [`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
  which can resume retrievals from registers
- require curl \>= 5.0

## ctrdata 1.11.1 (2022-11-20)

CRAN release: 2022-11-20

- cater for very short EUCTR results-related information
- show warning as beta CTGOV website is not supported
- limit unit testing to MongoDB and SQLite
- return error for ctrGetQueryUrl() if no query URL
- prevent re-using connections to reduce http/2 layer errors
- update query history when querytoupdate was used but no new records
  found
- make
  [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  to always return visible result
- correct
  [`dfTrials2Long()`](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
  identifier (EUCTR no more top “1” across fields)
- correct non-ASCII characters

## ctrdata 1.11.0 (2022-11-02)

CRAN release: 2022-11-02

- now works with DuckDB (\>= v0.6.0) as database backend, using nodbi
  (\>= v0.9.0)
- reduced default number of parallel connections to EUCTR from 10 to 4

## ctrdata 1.10.2 (2022-08-20)

CRAN release: 2022-08-20

- fix slow speed in
  [`dfName2Value()`](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
- fix to remove row names from
  [`dfName2Value()`](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
- fix for internal function to handle tibble
- fix for handling certain ISRCTN queries
- fix
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  with missing data
- fix timeouts and methods in package testing
- fix
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  for rare complex fields
- fix URL in Rd file
- make examples runnable with demo database
- include `wherevalue` in
  [`dfName2Value()`](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
  result

## ctrdata 1.10.1 (2022-07-24)

CRAN release: 2022-07-24

- fix documentation issues
  (<https://stat.ethz.ch/pipermail/r-package-devel/2022q3/008240.html>)
- fix unit test with unused but missing argument
- fix GitHub actions and tests

## ctrdata 1.10.0 (2022-07-01)

CRAN release: 2022-07-06

- [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  new parameter `euctrresultsfilespath`, deprecating
  `euctrresultspdfpath`
- [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  now also extracts and saves results files other than PDF files
- [`ctrFindActiveSubstanceSynonyms()`](https://rfhb.github.io/ctrdata/reference/ctrFindActiveSubstanceSynonyms.md)
  returns NULL for non-existing active substance

## ctrdata 1.9.1 (2022-04-24)

CRAN release: 2022-04-24

- type e811… variables
- bugfix in dbGetFieldsIntoDf
- bugfix annotations mix up for some backends
- editorial update vignettes

## ctrdata 1.9.0 (2022-03-13)

CRAN release: 2022-03-13

- chunked trial batches in `NDJSON` files for accelerated database
  import
- if package dplyr is loaded, functions return a tibble instead of a
  data frame
- update and correct documentation
- [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  returns a vector of fields which now has as names the register in
  which a field occurs
- accelerated binary checks (cygwin / Windows)
- remove internet proxy mangling in order to use system configuration
  (e.g., transparent proxies used, or environment variable https_proxy
  specified by user)
- refactored internal caching
- correct
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  for specific nested data structures
- correct
  [`dfTrials2Long()`](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
  for specific fields
- correct
  [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
  when only single trial in any register
- modify field typing to decode HTML entities
- type some fields as difftime, e.g. `min_age` in CTGOV
- speed up parts of
  [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  and simplify more fields
- [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  returns names of all leaf and node fields
- improve and update documentation
- changed EU Member State to default to DE for dbFindIdsUniqueTrials()
- corrected `installCygwinWindowsDoInstall()` to properly update an
  installation (remove –prune-install)
- test all binaries after `installCygwinWindowsDoInstall()` and only
  cache successful binary testing
- correct typing `required_header.download_date`
- improve numbering in
  [`dfTrials2Long()`](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md),
  covering nested items
- thorough documentation improvement
- simplified
  [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
- cleaned up testing binaries
- cleaned up helper scripts
- removed `ctrGetQueryUrlFromBrowser()`, long deprecated
- uses nodbi 0.6.0
- can use PostgreSQL as backend
- include PostgreSQL in Github Actions

## ctrdata 1.8.0 (2021-11-18)

CRAN release: 2021-11-21

- changes to match nodbi 0.5.0
- simplifying database operations (user-visible functions:
  ctrLoadQueryIntoDb, dbFindIdsUniqueTrials, dbGetFieldsIntoDf), without
  changes to API

## ctrdata 1.7.1 (2021-08-22)

CRAN release: 2021-08-22

- fix DBI not needed in Imports any more (CRAN Note)
- fix potential file name issue in conversion script
- fix dbFindFields() to never return \_id (previously depended on
  database backend)
- changed tests (not on CRAN detection, register availability,
  additional tests)

## ctrdata 1.7.0 (2021-07-24)

CRAN release: 2021-07-25

- much reduced database backend-specific code, using nodbi 0.4.3
  (released 2021-07-23) which also introduces transactions for sqlite
  using RSQLite \>=2.2.4 (released 2021-03-12)
- temporary directory creation only when needed, more automated deletion
- changes in detecting non-functioning register servers
- further streamlined unit testing

## ctrdata 1.6.0 (2021-05-09)

CRAN release: 2021-05-09

- added support for ISRCTN
- refactored checking binaries and caching this info
- updated EUCTR download parameters
- refactored ctrGetQueryUrl and ctrOpenSearchPagesInBrowser
- harmonised error checking
- avoid some errors with external scripts
- refactored url / query mangling, added detailed testing
- refactored storing JSON into database (handle big files, reduce
  memory)
- improved dbFindIdsUniqueTrials (speed, memory, register coverage)
- factored out conversion to JSON
- accelerated EUCTR results and history download and storage
- external scripts now create multiple chunks of records
- use further identifier fields with dbFindIdsUniqueTrials
- adding user info which field entries could not be typed

## ctrdata 1.5.3 (2021-04-19)

CRAN release: 2021-04-19

- include message how to handle server certificate issues, by
  propagating user settings for httr to curl operations
- ensure identical return structures when no new trials found
- dfTrials2Long: harmonise identifier level assignment, address cases
  where field occurs only once in input df
- dfMergeTwoVariablesRelevel: corrected and improved user info
- dfName2Value: remove duplicate rows, e.g. from duplicated criteria

## ctrdata 1.5.2 (2021-04-05)

CRAN release: 2021-04-06

- bugfix of EOL for converting EUCTR files

## ctrdata 1.5.1 (2021-03-21)

CRAN release: 2021-03-21

- bugfix for non-matching euctr protocol and result ids: any trials from
  EUCTR for which results were downloaded with version 1.5.0 should be
  downloaded again (ctrLoadQueryIntoDb)
- dfTrials2Long refactored and accelerated
- API change: dfTrials2Long return value (identifier replaces main_id
  and sub_id)
- new option to save EUCTR results PDF files in user-specified directory

## ctrdata 1.5.0 (2021-03-14)

CRAN release: 2021-03-16

- return values of dbGetFieldsIntoDf are now mostly identical whether
  using src_mongo or src_sqlite, to best ensure portability of analysis
  code
- permit dots in queries / URLs
- improved handling of queryterm
- renamed ctrGetQueryUrlFromBrowser to ctrGetQueryUrl
- soft deprecated ctrGetQueryUrlFromBrowser
- ensure parallel retrievals from EUCTR
- speed up routines in dbGetFieldsIntoDf
- make dfTrials2Long handle NA better
- improved documentation, clarified examples
- simplified internals for typing fields, start typing results fields

## ctrdata 1.4.1 (2020-11-03)

CRAN release: 2020-11-03

- reset row names on data frames returned by functions
- update curl parameters when accessing EUCTR

## ctrdata 1.4 (2020-10-17)

- new: easy access to variables with dfTrials2Long() + dfName2Value()
- improved dfMergeTwoVariablesRelevel() to maintain type of data
- revised and simplified vignettes
- deprecated: dfListExtractKey()
- refactored parts of euctr retrieval
- notify user when euctr register server does not permit compression and
  how long retrieval will take
- fixed identifying unique ids

## ctrdata 1.3.2 (2020-10-03)

CRAN release: 2020-10-05

- quote system file paths

## ctrdata 1.3.1 (2020-08-01)

CRAN release: 2020-08-02

- fix error in CI tests

## ctrdata 1.3.0 (2020-07-27)

CRAN release: 2020-07-27

- workaround EUCTR certificate issue
- streamline ctrGetQueryUrlFromBrowser()
- better handling of complex fields
- include further tests for query string handling, checking more
  parameters and return values
- better clean-up after testing
- ctrLoadQueryIntoDb(querytorerun = …) now looks for the date when the
  querytorerun was last run, to more often use euctr update options
- switching from travis to github action
- upped coverage of code tested

## ctrdata 1.2.1 (2020-05-18)

CRAN release: 2020-05-18

- tinytest \>= 1.2.1 to avoid regression error
- correct testing detail

## ctrdata 1.2 (2019-12-07)

CRAN release: 2019-12-08

- correct cygwin install detail

## ctrdata 1.1 (2019-11-12)

CRAN release: 2019-11-12

- release after nodbi 0.4 is available
- update description for installation from github
- handled mixed arrays and text values in same key of ctgov trial
  records
- more user information during importing
- further nesting added for euctr trial records
- user verbose information extended for record importing
- improved parsing of euctr trial records
- correct re-opening of sqlite connection

## ctrdata 1.0.1 (2019-10-22)

CRAN release: 2019-10-23

- correction of testing

## ctrdata 1.0 (2019-10-16)

CRAN release: 2019-10-16

- switch to nodbi::scr\_{mongo,sqlite}() with re-implementation of most
  functions
- switch from testthat to tinytest, so that users can check with
  tinytest::test_package(“ctrdata”)
- improvements to euctr trial import  
- new function dfListExtractKey
- speed up testing bash commands under windows

## ctrdata 0.18.2 (2019-04-30)

CRAN release: 2019-05-01

- extended compatibility with cygwin and Windows
- find and use any cygw\* under windows
- refactored escaping bash command when called under windows
- corrected typing date fields

## ctrdata 0.18.1 (2019-04-14)

- simplified cygwin install
- updated documentation
- corrected inconsistent handling of query terms
- added automated proxy handling

## ctrdata 0.18 (2019-04-11)

CRAN release: 2019-04-11

- release version
- bug fixes in field typing
- move to use remote mongodb server
- updated vignettes

## ctrdata 0.17 (2019-03-27)

- improve dbFindFields() formatting
- added parameter to force running a query again
- added further typing (some of the numeric fields)
- improve cygwin install attempts and information
- removed dependency on local mongodb installation (major rewrite)
- improved support for remote mongodb servers (note changes in host / db
  / uri parameters)
- Important: Added no checking of SSL certificates for EUCTR because the
  EUCTR server is not sending the required intermediate and root
  certificates, thus failing curl and httr, see
  <https://www.digicert.com/help/?host=www.clinicaltrialsregister.eu>
- fixed EUCTR results retrieval (curl return value order not
  predictable)
- removed second time adding metadata in one function
- streamlined user information and progress feedback

## ctrdata 0.14.3 (2019-03-12)

- turned error into message when no new trials are found
- prevent failing tests if no new trials found in rss feed

## ctrdata 0.14.2 (2019-03-07)

- harmonise user information

## ctrdata 0.14.1 (2019-03-07)

CRAN release: 2019-03-12

- replaced RCurl (which failed for some register servers) by httr and
  curl
- create README.md from README.Rmd

## ctrdata 0.14 (2019-03-06)

- intention to submit to CRAN, therefore changing several warnings to
  messages, improve testing of tool chain applications

## ctrdata 0.13.3 (2019-03-03)

- prettified dbFindFields() by removing count symbols (XX.) from paths
- improve converting of invalid XML from EUCTR result files to JSON

## ctrdata 0.13.2 (2019-02-28)

- made EUCTR retrieval more robust
- added marginal case for url of single trial in EUCTR
- extended timeout for checking online status of EUCTR

## ctrdata 0.13.1 (2019-02-24)

- added typing of dates and some logical fields when using
  dbGetFieldsIntoDf()

## ctrdata 0.13 (2019-01-06)

- dbGetVariablesIntoDf() is deprecated, use dbGetFieldsIntoDf() instead
- dbFindVariable() is deprecated, use dbFindFields() instead
- in dbMergeTwoVariablesRelevel() parameter varnames is deprecated, use
  colnames instead

## ctrdata 0.12.1 (2018-12-15)

- added function ctrFindActiveSubstanceSynonyms() to obtain synonyms for
  an active substance
- added user information on number of trials in CTGOV to be downloaded,
  and limit this to 5000 per query
- corrected import from EUCTR for details = FALSE

## ctrdata 0.12 (2018-05-19)

- added possibility to add user’s annotations to records retrieved with
  a query (new option annotate.text)

## ctrdata 0.11.2 (2018-04-22)

- changed queryterm processing

## ctrdata 0.11.1 (2018-04-07)

- improved installFindMongoBinaries(), should now better detect mongo
  binary locations and use for example in cron scripts, which may not
  have access to a user’s path information

## ctrdata 0.11 (2018-01-28)

- newly retrieved: EUCTR results history, into new fields
  “firstreceived_results_date” and “version_results_history”
- adding feature as per issue
  [\#8](https://github.com/rfhb/ctrdata/issues/8)

## ctrdata 0.10.4 (2017-12-28)

- note on compatibility with mongoDB versions
- fixing issue [\#8](https://github.com/rfhb/ctrdata/issues/8)
- simplified license

### ctrdata 0.10.3 (2017-11-24)

- changed custom-built “x5_trial_status” to “p_end_of_trial_status”
  provided by EUCTR

## ctrdata 0.10.2 (2017-11-22)

- editorial project updates

## ctrdata 0.10.1 (2017-07-30)

- now loading results from euctr where available as xml

## ctrdata 0.10.0 (2017-07-25)

- fully load results from ctgov
- prepare loading results from euctr

## ctrdata 0.9.14 (2017-06-28)

- refactored system calls
- windows now part of continuous integration

## ctrdata 0.9.13 (2017-06-23)

- refactored ctrLoadQueryIntoDb

## ctrdata 0.9.12 (2017-06-18)

- Preparing for new CTGOV interface
- Improved documentation
- Corrected ctrGetQueryUrlFromBrowser return value

## ctrdata 0.9.11.1 (2017-02-04)

- Improved documentation

## ctrdata 0.9.11 (2017-01-15)

- Added functionality: EUCTR fallback import mechanism if large JSON
  file fails to import into mongoDB (by splitting and importing one JSON
  file for each trial, tested with several thousand trials)

## ctrdata 0.9.10.1 (2017-01-12)

- Fixes issues with conversion of EUCTR records that did not have
  details.
- Fixes issue that placebo information was converted into IMP fields.

## ctrdata 0.9.10.0 (2016-12-28)

- Added metadata attributes to returned objects to indicate database,
  query, timestamp etc.

## ctrdata 0.9.9.5 (2016-12-14)

- Added option ctrLoadQueryIntoDb(querytoupdate = “last”) to re-download
  last query in collection

## ctrdata 0.9.9.4 (2016-11-18)

- Added progress indicator to ctrLoadQueryIntoDb() to indicate network
  download traffic

## ctrdata 0.9.9.3 (2016-11-17)

- deduplication in dbFindIdsUniqueTrials() optimised for speed and
  memory, added check by ISRCTN

## ctrdata 0.9.9.2 (2016-11-13)

- corrected deduplication in dbFindIdsUniqueTrials()

## ctrdata 0.9.9.1 (2016-11-12)

- renamed ctrQueryHistoryInDb() to dbQueryHistory()
- note: change in json format of query history, breaking compatibility
- refactored all concerned functions to use mongolite
- rmongodb is no more supported

## ctrdata 0.9 (2016-10-17)

- changed implementation of dbFindIdsUniqueTrials()
- editorial changes to examples

## ctrdata 0.8.1 (2016-09-07)

- added field to indicate source register
- improved ctrLoadQueryIntoDb() with details = FALSE
- added example for map plotting

## ctrdata 0.8 (2016-09-04)

- dbFindIdsUniqueTrials now encapsulates dfFindIdsUniqueEuctrRecord
- dfFindIdsUniqueEuctrRecords removed
- installation instructions updated after recently rmongodb was removed
  from CRAN

## ctrdata 0.7 (2016-05-29)

- dbGetVariablesIntoDf changed to concatenate values in array and
  objects
- completed test adaptation for travis
- improving perl regex
- checking helper applications

## ctrdata 0.6.2 (2016-04-20)

- add / update field “record_last_import” for every imported / updated
  record

## ctrdata 0.6.1 (2016-04-02)

- changed to provide vignettes

## ctrdata 0.6 (2016-02-25)

- different update mechanism for EUCTR implemented
- corrected function name from db… to dfFindUniqueEuctrRecord()

## ctrdata 0.5.9 (2016-01-23)

- Corrected bugs
- Started preparation of submission to CRAN
- Preparing to include package unit tests

## ctrdata 0.5 (2015-11-29)

- Published on github
- Improved documentation

## ctrdata 0.4 (2015-10-08)

- Renamed all functions for consistency and ease-of-use

## ctrdata 0.3 (2015-10-06)

- Added functionality to download xml data from CTGOV, which includes
  more data than the csv format

## ctrdata 0.2.8

- Changed and extended how history of queries is included in database.
- New function dbCTRQueryHistory()

## ctrdata 0.2.7

- Added function for merging variables such as from different registers
  and optionally to merge values into new values
- Note that function findCTRkey was renamed to dbFindCTRkey because it
  acts on the database

## ctrdata 0.2.5

- Added function for selecting preferred language versions of trials
  from EUCTR
- Improved use of automatic proxy configuration script

## ctrdata 0.2.2

- Added proxy function and improved installation of cygwin under MS
  Windows

## ctrdata 0.2 (2015-09-19)

- Now also working on MS Windows

## ctrdata 0.1 (2015-09-15)

- First version with basic functionality
- Limited testing
- Works on Mac OS X (10.10.x)
