# Calculate details of a primary endpoint of a study

Trial concept calculated: full description of the primary endpoint,
concatenating with " == " its title, description, time frame of
assessment. The details vary by register. The text description can be
used for identifying trials of interest or for analysing trends in
primary endpoints, which among the set of all endpoints are most often
used for determining the number of participants sought for the study.

## Usage

``` r
f.primaryEndpointDescription(df = NULL)
```

## Arguments

- df:

  data frame such as from
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).
  If \`NULL\`, prints fields needed in \`df\` for calculating this trial
  concept, which can be used with
  [dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md).

## Value

data frame with columns \`\_id\` and \`.primaryEndpointDescription\`,
which is a list (that is, one or more items in one vector per row; the
background is that some trials have several endpoints as primary).

## Examples

``` r
# fields needed
f.primaryEndpointDescription()
#> $euctr
#> [1] "e51_primary_end_points"                         
#> [2] "e511_timepoints_of_evaluation_of_this_end_point"
#> 
#> $ctgov
#> [1] "primary_outcome.measure"     "primary_outcome.description"
#> [3] "primary_outcome.time_frame" 
#> 
#> $ctgov2
#> [1] "protocolSection.outcomesModule.primaryOutcomes.measure"    
#> [2] "protocolSection.outcomesModule.primaryOutcomes.description"
#> [3] "protocolSection.outcomesModule.primaryOutcomes.timeFrame"  
#> 
#> $isrctn
#> [1] "trialDescription.primaryOutcome"
#> 
#> $ctis
#> [1] "authorizedApplication.authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.endPoint"
#> [2] "authorizedPartI.trialDetails.trialInformation.endPoint.primaryEndPoints.endPoint"                      
#> 

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.primaryEndpointDescription",
  con = dbc)
#> To review trial concepts details, call 'help("ctrdata-trial-concepts")'
#> Querying database (11 fields)...
#> , typing fields...
#> Calculating f.primaryEndpointDescription...                            

trialsDf
#> # A tibble: 24 × 2
#>    `_id`             .primaryEndpointDescription
#>    <chr>             <list>                     
#>  1 12949496          <chr [1]>                  
#>  2 13281214          <chr [1]>                  
#>  3 17473621          <chr [1]>                  
#>  4 2016-004489-24-DE <chr [1]>                  
#>  5 2019-002663-10-ES <chr [1]>                  
#>  6 2022-000099-20-DE <chr [1]>                  
#>  7 2022-500244-37-00 <chr [1]>                  
#>  8 2023-505613-24-00 <chr [14]>                 
#>  9 2023-508143-51-01 <chr [1]>                  
#> 10 2024-510663-34-00 <chr [4]>                  
#> # ℹ 14 more rows
```
