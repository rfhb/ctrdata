# Get identifiers of deduplicated trial records

Records for a clinical trial can be loaded from more than one register
into a collection. This function returns deduplicated identifiers for
all trials in the collection, respecting the register(s) preferred by
the user. All registers are recording identifiers also from other
registers, which are used by this function to provide a vector of
identifiers of deduplicated trials.

## Usage

``` r
dbFindIdsUniqueTrials(
  preferregister = c("CTGOV2", "EUCTR", "CTGOV", "ISRCTN", "CTIS"),
  prefermemberstate = "BE",
  include3rdcountrytrials = TRUE,
  con,
  verbose = FALSE
)
```

## Arguments

- preferregister:

  A vector of the order of preference for registers from which to
  generate unique \_id's, default
  `c("CTGOV2", "EUCTR", "CTGOV", "ISRCTN", "CTIS")`

- prefermemberstate:

  Code of single EU Member State for which records should returned. If
  not available, a record for BE or lacking this, any random Member
  State's record for the trial will be returned. For a list of codes of
  EU Member States, please see vector `countriesEUCTR`. Specifying "3RD"
  will return the Third Country record of trials, where available.

- include3rdcountrytrials:

  A logical value if trials should be retained that are conducted
  exclusively in third countries, that is, outside the European Union.
  Ignored if `prefermemberstate` is set to "3RD".

- con:

  A database connection object, created with `nodbi`. See section \`1 -
  Database connection\` in
  [ctrdata](https://rfhb.github.io/ctrdata/reference/ctrdata.md).

- verbose:

  If `TRUE`, prints out the fields of registers used to find
  corresponding trial records

## Value

A named vector with strings of keys (field "\_id") of records in the
collection that represent unique trials, where names correspond to the
register of the record.

## Details

Note that the content of records may differ between registers (and, for
"EUCTR", between records for different Member States). Such differences
are not considered by this function.

Note that for "CTIS", the trial with the latest (highest) resubmission
number (last two digits of \`id\`, clinical trial number) is identified.

Note that the trial concept
[f.isUniqueTrial](https://rfhb.github.io/ctrdata/reference/f.isUniqueTrial.md)
(which uses this function) can be calculated at the time of creating a
data frame with
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md),
which often may be the preferred approach.

## Examples

``` r

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

dbFindIdsUniqueTrials(con = dbc)[1:10]
#> Searching for duplicate trials... 
#> - Getting all trial identifiers...
#> , 31 found in collection
#> - Finding duplicates among registers' and sponsor ids...
#> - 2 EUCTR _id were not preferred EU Member State record for 11 trials
#> - Unique are 8 / 5 / 5 / 3 / 8 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
#> = Returning keys (_id) of 29 records in collection "my_trials"
#>              ISRCTN              ISRCTN              ISRCTN               EUCTR 
#>          "12949496"          "13281214"          "17473621" "2012-003632-23-SE" 
#>               EUCTR               EUCTR                CTIS                CTIS 
#> "2014-002606-20-PT" "2014-003556-31-SE" "2022-500244-37-00" "2022-501142-30-00" 
#>                CTIS                CTIS 
#> "2023-505613-24-00" "2023-508143-51-01" 

# alternative as of ctrdata version 1.21.0,
# using defaults of dbFindIdsUniqueTrials()
df <- dbGetFieldsIntoDf(
  fields = "keyword",
  calculate = "f.isUniqueTrial",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (2 fields)...
#> Calculating f.isUniqueTrial...                            

#> Searching for duplicate trials... 
#> - Getting all trial identifiers...
#> , 31 found in collection
#> - Finding duplicates among registers' and sponsor ids...
#> - 2 EUCTR _id were not preferred EU Member State record for 11 trials
#> - Unique are 8 / 5 / 5 / 3 / 8 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
#> = Returning keys (_id) of 29 records in collection "my_trials"

# using base R
df[df[[".isUniqueTrial"]], ]
#> # A tibble: 29 × 3
#>    `_id`             keyword .isUniqueTrial
#>    <chr>             <chr>   <lgl>         
#>  1 12949496          NA      TRUE          
#>  2 13281214          NA      TRUE          
#>  3 17473621          NA      TRUE          
#>  4 2012-003632-23-SE NA      TRUE          
#>  5 2014-002606-20-PT NA      TRUE          
#>  6 2014-003556-31-SE NA      TRUE          
#>  7 2022-500244-37-00 NA      TRUE          
#>  8 2022-501142-30-00 NA      TRUE          
#>  9 2023-505613-24-00 NA      TRUE          
#> 10 2023-508143-51-01 NA      TRUE          
#> # ℹ 19 more rows

if (FALSE) { # \dontrun{
library(dplyr)
df %>% filter(.isUniqueTrial)
} # }
```
