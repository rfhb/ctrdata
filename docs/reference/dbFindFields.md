# Find names of fields in the database collection

Given a part of the name of fields that are of interest to the user,
this function returns the full field names used in records that were
previously loaded into a collection (using
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)).
Only names of fields that have a value in the collection can be
returned. Set `sample = FALSE` to force screening all records in the
collection for field names, which can take long. See
[ctrShowOneTrial](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
to interactively find fields.

## Usage

``` r
dbFindFields(namepart = ".*", con, sample = TRUE, verbose = FALSE)
```

## Arguments

- namepart:

  A character string (can be a regular expression, including Perl-style)
  to be searched among all field names (keys) in the collection,
  case-insensitive. The default \`".\*"\` lists all fields.

- con:

  A database connection object, created with `nodbi`. See section \`1 -
  Database connection\` in
  [ctrdata](https://rfhb.github.io/ctrdata/reference/ctrdata.md).

- sample:

  If `TRUE` (default), uses a sample of only 5 trial records per
  register to identify fields, to rapidly return a possibly incomplete
  set of field names. If `FALSE`, uses all trial records in the
  collection, which will take more time with more trials but ensures to
  returns all names of all fields in the collection.

- verbose:

  If `TRUE`, prints additional information (default `FALSE`).

## Value

Vector of strings with full names of field(s) found, ordered by register
and alphabet, see examples. Names of the vector are the names of the
register holding the respective fields. The field names can be fed into
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
to extract the data for the field(s) from the collection into a data
frame.

## Details

The full names of child fields are returned in dot notation (e.g.,
`clinical_results.outcome_list.outcome.measure.class_list.class.title`)
In addition, names of parent fields (e.g., `clinical_results`) are
returned. Data in parent fields is typically complex (nested), see
[dfTrials2Long](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
for easily handling it. For field definitions of the registers, see
"Definition" in
[ctrdata-registers](https://rfhb.github.io/ctrdata/reference/ctrdata-registers.md).
Note: When `dbFindFields` is first called after
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md),
it will take a moment.

## Examples

``` r

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

dbFindFields(namepart = "date", con = dbc)[1:5]
#> Finding fields in database collection
#>  (sampling 5 trial records per register) 
#> . 
#> . 
#> . 
#> . 
#> . 
#> 
#> Field names cached for this session.
#>                                                EUCTR 
#>             "n_date_of_competent_authority_decision" 
#>                                                EUCTR 
#>                 "n_date_of_ethics_committee_opinion" 
#>                                                EUCTR 
#>              "p_date_of_the_global_end_of_the_trial" 
#>                                                EUCTR 
#> "trialChanges.globalAmendments.globalAmendment.date" 
#>                                                EUCTR 
#>                 "trialInformation.analysisStageDate" 

# view names of all 3838 fields from all registers:

allFields <- dbFindFields(con = dbc, sample = FALSE)
#> Finding fields in database collection
#>  (may take some time) 
#> . 
#> . 
#> . 
#> . 
#> . 
#> 
#> Field names cached for this session.

if (interactive()) View(data.frame(
  register = names(allFields),
  field = allFields))
```
