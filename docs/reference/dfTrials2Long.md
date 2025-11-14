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
#> clinical_results.participant_flow.recruitment_details                                                                                                                                                   
#> clinical_results.participant_flow.pre_assignment_details                                                                                                                                                
#> clinical_results.participant_flow.group_list.group                                                                                                                                                      
#> clinical_results.participant_flow.period_list.period.title                                                                                                                                              
#> clinical_results.participant_flow.period_list.period.milestone_list.milestone                                                                                                                           
#> clinical_results.participant_flow.period_list.period.drop_withdraw_reason_list.drop_withdraw_reason.title                                                                                               
#> clinical_results.participant_flow.period_list.period.drop_withdraw_reason_list.drop_withdraw_reason.participants_list.participants.group_id                                                             
#> clinical_results.participant_flow.period_list.period.drop_withdraw_reason_list.drop_withdraw_reason.participants_list.participants.count                                                                
#>                                                                                                                                                                                                         
#> . 
#> . 
#> 
#> Total 43 rows, 12 unique names of variables
#> # A tibble: 43 × 4
#>    `_id`       identifier name                                             value
#>    <chr>       <chr>      <chr>                                            <chr>
#>  1 NCT02620761 1          clinical_results.participant_flow.group_list.gr… "Inf…
#>  2 NCT02620761 2          clinical_results.participant_flow.group_list.gr… "Inf…
#>  3 NCT02620761 1          clinical_results.participant_flow.group_list.gr… "P1" 
#>  4 NCT02620761 2          clinical_results.participant_flow.group_list.gr… "P2" 
#>  5 NCT02620761 1          clinical_results.participant_flow.group_list.gr… "Con…
#>  6 NCT02620761 2          clinical_results.participant_flow.group_list.gr… "Fen…
#>  7 NCT02620761 1.1        clinical_results.participant_flow.period_list.p… "1"  
#>  8 NCT02620761 1.2        clinical_results.participant_flow.period_list.p… "0"  
#>  9 NCT02620761 2.1        clinical_results.participant_flow.period_list.p… "1"  
#> 10 NCT02620761 2.2        clinical_results.participant_flow.period_list.p… "0"  
#> # ℹ 33 more rows
```
