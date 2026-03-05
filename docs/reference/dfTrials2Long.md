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

dfTrials2Long(df = dfwide)
#> clinical_results.participant_flow.group_list.group                                                                                                                                                      

#> clinical_results.participant_flow.period_list.period                                                                                                                                                    

#>                                                                                                                                                                                                         

#> . 
#> . 
#> 
#> Total 293 rows, 10 unique names of variables
#> # A tibble: 293 × 4
#>    `_id`       identifier name                                             value
#>    <chr>       <chr>      <chr>                                            <chr>
#>  1 NCT00506441 1          clinical_results.participant_flow.group_list.gr… "Ope…
#>  2 NCT00506441 2          clinical_results.participant_flow.group_list.gr… "Dou…
#>  3 NCT00506441 1          clinical_results.participant_flow.group_list.gr… "P1" 
#>  4 NCT00506441 2          clinical_results.participant_flow.group_list.gr… "P2" 
#>  5 NCT00506441 1          clinical_results.participant_flow.group_list.gr… "MCI…
#>  6 NCT00506441 2          clinical_results.participant_flow.group_list.gr… "Pla…
#>  7 NCT00506441 1.1.1      clinical_results.participant_flow.period_list.p… "21" 
#>  8 NCT00506441 1.1.2      clinical_results.participant_flow.period_list.p… "0"  
#>  9 NCT00506441 1.2.1      clinical_results.participant_flow.period_list.p… "13" 
#> 10 NCT00506441 1.2.2      clinical_results.participant_flow.period_list.p… "0"  
#> # ℹ 283 more rows
```
