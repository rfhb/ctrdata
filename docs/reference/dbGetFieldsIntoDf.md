# Create data frame of specified fields or trial concepts from database collection

Fields in the collection are retrieved from all records into a data
frame (or tibble). The function uses the field names to appropriately
type the values that it returns, harmonising original values (e.g.,
"Yes" to \`TRUE\`, "false" to \`FALSE\`, "Information not present in
EudraCT" to \`NA\`, date strings to dates or time differences, number
strings to numbers). Trial concepts are calculated for all records and
included in the return value.

## Usage

``` r
dbGetFieldsIntoDf(fields = "", calculate = "", con, verbose = FALSE)
```

## Arguments

- fields:

  Vector of one or more strings, with names of sought fields. See
  function
  [dbFindFields](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  for how to find names of fields and
  [ctrShowOneTrial](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
  for interactively selecting field names. Dot path notation
  ("field.subfield") without indices is supported. If compatibility with
  [nodbi::src_postgres](https://docs.ropensci.org/nodbi/reference/src_postgres.html)
  is needed, specify fewer than 50 fields, or use parent fields such as
  \`"a.b"\` instead of \`c("a.b.c.d", "a.b.c.e")\` and then access
  sought fields with
  [dfTrials2Long](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
  followed by
  [dfName2Value](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
  or with other R functions.

- calculate:

  Vector of one or more strings, which are names of functions to
  calculate certain trial concepts from fields in the collection, across
  different registers. See
  [ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
  for available functions.

- con:

  A database connection object, created with `nodbi`. See section \`1 -
  Database connection\` in
  [ctrdata](https://rfhb.github.io/ctrdata/reference/ctrdata.md).

- verbose:

  If `TRUE`, prints additional information (default `FALSE`).

## Value

A data frame (or tibble, if `tibble` is loaded) with columns
corresponding to the sought fields. A column with the record \`\_id\`
will always be included. The maximum number of rows of the returned data
frame is equal to the number of trial records in the database
collection, or less if none of the fields has a value in a record.

## Details

Within a given trial record, a field can be hierarchical and structured,
that is, nested. The function simplifies the structure of nested data
and may concatenate multiple strings in a field using " / " (see
example) and may have widened the returned data frame with additional
columns that were recursively expanded from simply nested data (e.g.,
"externalRefs" to columns "externalRefs.doi",
"externalRefs.eudraCTNumber" etc.). For an alternative ways for handling
complex nested data, see
[dfTrials2Long](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
and
[dfName2Value](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
for extracting the sought variable(s).

## Examples

``` r

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

# get fields that are nested within another field
# and can have multiple values with the nested field
dbGetFieldsIntoDf(
  fields = "b1_sponsor.b31_and_b32_status_of_the_sponsor",
  con = dbc)
#> Querying database (1 fields)...
#> # A tibble: 5 × 2
#>   `_id`             b1_sponsor.b31_and_b32_status_of_the_sponsor
#>   <chr>             <chr>                                       
#> 1 2012-003632-23-CZ Commercial                                  
#> 2 2012-003632-23-SE Commercial                                  
#> 3 2014-002606-20-PT Commercial                                  
#> 4 2014-003556-31-GB Commercial                                  
#> 5 2014-003556-31-SE Commercial                                  

# fields that are lists of string values are
# returned by concatenating values with " / "
dbGetFieldsIntoDf(
  fields = "keyword",
  con = dbc)
#> Querying database (1 fields)...
#> # A tibble: 5 × 2
#>   `_id`       keyword                                                           
#>   <chr>       <chr>                                                             
#> 1 NCT03280147 Neonate / Sepsis / Antibiotics / Duration                         
#> 2 NCT03325439 Electroencephalographic neonatal seizures / Brivaracetam / Epilep…
#> 3 NCT03431558 Bovine Lactoferrin, Neonatal infection, Low Birth Weight          
#> 4 NCT04001712 early caffeine preterm                                            
#> 5 NCT04041765 IgM-enriched Intravenous Immunoglobulin                           

# calculate new field(s) from data across trials
df <- dbGetFieldsIntoDf(
  fields = "keyword",
  calculate = c("f.statusRecruitment", "f.isUniqueTrial", "f.startDate"),
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (25 fields)...
#> Calculating f.statusRecruitment...                            

#> Calculating f.isUniqueTrial...                            

#> Searching for duplicate trials... 
#> - Getting all trial identifiers...
#> , 31 found in collection
#> - Finding duplicates among registers' and sponsor ids...
#> - 2 EUCTR _id were not preferred EU Member State record for 11 trials
#> - Unique are 8 / 5 / 5 / 3 / 8 records from CTGOV / CTGOV2 / CTIS / EUCTR / ISRCTN
#> = Returning keys (_id) of 29 records in collection "my_trials"
#> Calculating f.startDate...                            


table(df$.statusRecruitment, exclude = NULL)
#> 
#>     ongoing   completed ended early       other 
#>           8          16           5           2 

if (FALSE) { # \dontrun{
library(dplyr)
library(ggplot2)

df %>%
  filter(.isUniqueTrial) %>%
  count(.statusRecruitment)

df %>%
  filter(.isUniqueTrial) %>%
  ggplot() +
  stat_ecdf(aes(
    x = .startDate,
    colour = .statusRecruitment))
} # }
```
