# Convert data frame with trial records into long format

The function works with procotol- and results- related information. It
converts lists and other values that are in a data frame returned by
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
into individual rows of a long data frame. From the resulting long data
frame, values of interest can be selected using
[dfName2Value](https://rfhb.github.io/ctrdata/reference/dfName2Value.md).
The function is particularly useful for fields with complex content,
such as node field "`clinical_results`" from EUCTR, for which
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
returns as a multiply nested list and for which this function then
converts every observation of every (leaf) field into a row of its own.

## Usage

``` r
dfTrials2Long(df)
```

## Arguments

- df:

  Data frame (or tibble) with columns including the trial identifier
  (`_id`) and one or more variables as obtained from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)

## Value

A data frame (or tibble, if `tibble` is loaded) with the four columns:
\`\_id\`, \`identifier\`, \`name\`, \`value\`

## Examples

``` r

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

dfwide <- dbGetFieldsIntoDf(
  fields = "clinical_results.participant_flow",
  con = dbc)
#> Querying database (1 fields)...
#> , typing fields...

dfTrials2Long(df = dfwide)
#> clinical_results.participant_flow.pre_assignment_details                                                                                                                                                

#> clinical_results.participant_flow.recruitment_details                                                                                                                                                   

#> clinical_results.participant_flow.group_list.group                                                                                                                                                      

#> clinical_results.participant_flow.period_list.period                                                                                                                                                    

#>                                                                                                                                                                                                         

#> . 
#> . 
#> 
#> Total 442 rows, 13 unique names of variables
#> # A tibble: 442 × 4
#>    `_id`       identifier name                                             value
#>    <chr>       <chr>      <chr>                                            <chr>
#>  1 NCT03325556 1          clinical_results.participant_flow.group_list.gr… Pima…
#>  2 NCT03325556 2          clinical_results.participant_flow.group_list.gr… Pima…
#>  3 NCT03325556 3          clinical_results.participant_flow.group_list.gr… Plac…
#>  4 NCT03325556 1          clinical_results.participant_flow.group_list.gr… P1   
#>  5 NCT03325556 2          clinical_results.participant_flow.group_list.gr… P2   
#>  6 NCT03325556 3          clinical_results.participant_flow.group_list.gr… P3   
#>  7 NCT03325556 1          clinical_results.participant_flow.group_list.gr… Pima…
#>  8 NCT03325556 2          clinical_results.participant_flow.group_list.gr… Pima…
#>  9 NCT03325556 3          clinical_results.participant_flow.group_list.gr… Plac…
#> 10 NCT03325556 1.1.1      clinical_results.participant_flow.period_list.p… 70   
#> # ℹ 432 more rows
```
