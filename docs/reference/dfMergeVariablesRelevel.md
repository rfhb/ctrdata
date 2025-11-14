# Merge variables, keeping type where possible, optionally relevel factors

Merge variables in a data frame such as returned by
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
into a new variable, and optionally also map its values to new levels.
See
[ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
for pre-defined cross-register concepts that are already implemented
based on merging fields from different registers and calculating a new
field.

## Usage

``` r
dfMergeVariablesRelevel(df = NULL, colnames = "", levelslist = NULL)
```

## Arguments

- df:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) with the
  variables (columns) to be merged into one vector.

- colnames:

  A vector of names of columns in \`df\` that hold the variables to be
  merged, or a selection of columns as per
  [`select`](https://dplyr.tidyverse.org/reference/select.html).

- levelslist:

  A names list with one slice each for a new value to be used for a
  vector of old values (optional).

## Value

A vector, with the type of the columns to be merged

## Examples

``` r

dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

df <- dbGetFieldsIntoDf(
  fields = c(
    "ctrname",
    "protocolSection.eligibilityModule.healthyVolunteers",
    "f31_healthy_volunteers",
    "eligibility.healthy_volunteers",
    "participants.participantType",
    paste0(
      "authorizedPartI.trialDetails.trialInformation.",
      "populationOfTrialSubjects.clinicalTrialGroups.name"
    ),
    paste0(
      "authorizedApplication.authorizedPartI.trialDetails.trialInformation.",
      "populationOfTrialSubjects.clinicalTrialGroups.name"
    )
  ),
  con = dbc
)
#> Querying database (7 fields)...

df$isrctnhealthy <- "Healthy volunteer" == df[, 6]
df$ctis1healthy <- "Healthy volunteers" == df[ ,7]
df$ctis2healthy <- "Healthy volunteers" == df[, 8]

table(
  df$ctrname,
  dfMergeVariablesRelevel(
    df = df,
    colnames = 'matches("healthy")'
))
#> Columns identified to be merged: protocolSection.eligibilityModule.healthyVolunteers, f31_healthy_volunteers, eligibility.healthy_volunteers, isrctnhealthy, ctis1healthy, ctis2healthy
#>         
#>          FALSE
#>   CTGOV      6
#>   CTGOV2     5
#>   CTIS       4
#>   EUCTR      5
#>   ISRCTN     8
```
