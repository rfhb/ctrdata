# Find synonyms of an active substance

An active substance can be identified by a recommended international
nonproprietary name (INN), a trade or product name, or a company
code(s). To find likely synonyms, the function retrieves from CTGOV2 the
field protocolSection.armsInterventionsModule.interventions.otherNames.
Note this does not seem to be based on choices from a dictionary but may
be manually filled, thus is not free of error and needs to be checked.

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

A character vector of the active substance (input parameter) and
synonyms, or NULL if active substance was not found and may be invalid

## Examples

``` r
if (FALSE) { # \dontrun{

ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
#  [1] "imatinib"          "CGP 57148"         "CGP 57148B"
#  [4] "CGP57148B"         "Gleevec"           "GLIVEC"
#  [7] "Imatinib"          "Imatinib Mesylate" "NSC 716051"
# [10] "ST1571"            "STI 571"           "STI571"
} # }
```
