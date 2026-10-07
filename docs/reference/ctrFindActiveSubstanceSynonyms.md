# Find synonyms of an active substance

**\[deprecated\]** An active substance can be identified by a
recommended international nonproprietary name (INN), a trade or product
name, or a company code(s). To find likely synonyms, the function
retrieves from CTGOV2 the field
`protocolSection.armsInterventionsModule.interventions`. Note this is
mostly manually filled, thus may not be free of errors.

## Usage

``` r
ctrFindActiveSubstanceSynonyms(activesubstance = "", verbose = FALSE)
```

## Arguments

- activesubstance:

  An active substance, in an atomic character vector

- verbose:

  Print number of studies found in CTGOV2 for `activesubstance`

## Value

A named character vector of the active substance (input parameter), the
MeSH term(s) and various names (other than the MeSH term) used in
registered studies, or NULL if the active substance was not found and
may be invalid. The active substances are ordered in decreasing number
of occurrence.

## Examples

``` r
if (FALSE) { # \dontrun{

ctrFindActiveSubstanceSynonyms(activesubstance = "imatinib")
# activesubstance                mesh
#      "imatinib" "imatinib mesylate"  "imatinib" "gleevec" "glivec"
#        "STI571"          "CGP57148" "CGP57148B" "NSC716051"
} # }
```
