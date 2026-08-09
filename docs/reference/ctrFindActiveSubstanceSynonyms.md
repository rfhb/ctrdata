# Find synonyms of an active substance

An active substance can be identified by a recommended international
nonproprietary name (INN), a trade or product name, or a company
code(s). To find likely synonyms, the function retrieves from CTGOV2 the
field protocolSection.armsInterventionsModule.interventions. Note this
is mostly manually filled, thus may not be free of errors.

## Usage

``` r
ctrFindActiveSubstanceSynonyms(activesubstance = "", verbose = FALSE)
```

## Arguments

- activesubstance:

  An active substance, in an atomic character vector

- verbose:

  Print number of studies found in CTGOV2 for \`activesubstance\`

## Value

A named character vector of the active substance (input parameter), the
MeSH code(s) and various names used in registered studies, or NULL if
active substance was not found and may be invalid. The active substances
are ordered in decreasing number of occurrence.

## Examples
