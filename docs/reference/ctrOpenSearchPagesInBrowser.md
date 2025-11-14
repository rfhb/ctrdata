# Open register to show query results or search page

Open advanced search pages of register(s), or execute search in browser.
For CTIS to accept a search URL and show results, consider installing
the [Tampermonkey browser extension](https://www.tampermonkey.net/),
click on the extension icon, "Create a new script", "Utility" and then
"Import from this URL":
\`https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js\`

## Usage

``` r
ctrOpenSearchPagesInBrowser(url = "", register = "", copyright = FALSE)
```

## Arguments

- url:

  of search results page to show in the browser. To open the browser
  with a previous search, the output of
  [ctrGetQueryUrl](https://rfhb.github.io/ctrdata/reference/ctrGetQueryUrl.md)
  or
  [dbQueryHistory](https://rfhb.github.io/ctrdata/reference/dbQueryHistory.md)
  can be used. Can be left as empty string (default) to open the
  advanced search page of `register`.

- register:

  Register(s) to open, "EUCTR", "CTGOV2", "ISRCTN" or "CTIS". Default is
  empty string, and this opens the advanced search page of the registers
  (including the expert search page in the case of CTGOV).

- copyright:

  (Optional) If set to `TRUE`, opens only the copyright pages of all
  registers.

## Value

(String) Full URL corresponding to the shortened `url` in conjunction
with `register` if any, or invisibly `TRUE` if no `url` is specified.

## Examples

``` r

# Open all and check copyrights before using registers
ctrOpenSearchPagesInBrowser(copyright = TRUE)

# Open specific register advanced search page
ctrOpenSearchPagesInBrowser(register = "CTGOV2")
ctrOpenSearchPagesInBrowser(register = "CTIS")
ctrOpenSearchPagesInBrowser(register = "EUCTR")
ctrOpenSearchPagesInBrowser(register = "ISRCTN")

# Open all queries that were loaded into demo collection
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials",
  flags = RSQLite::SQLITE_RO)

dbh <- dbQueryHistory(con = dbc)

for (r in seq_len(nrow(dbh))) {
    ctrOpenSearchPagesInBrowser(dbh[r, ])
}
#> Since 2024-06-25, the classic CTGOV servers are no longer available. Package ctrdata has translated the classic CTGOV query URL from this call of function ctrLoadQueryIntoDb(queryterm = ...) into a query URL that works with the current CTGOV2. This is printed below and is also part of the return value of this function, ctrLoadQueryIntoDb(...)$url. This URL can be used with ctrdata functions. Note that the fields and data schema of trials differ between CTGOV and CTGOV2. 
#> 
#> Replace this URL:
#> 
#> term=AREA[MaximumAge]+RANGE[0+days,+28+days]&intr=Drugs,+Investigational&strd_s=01/01/2018&strd_e=01/01/2020&type=Intr&phase=2
#> 
#> with this URL:
#> 
#> https://clinicaltrials.gov/search?start=2018-01-01_2020-01-01&term=AREA[MaximumAge] RANGE[0 days, 28 days]&intr=Drugs, Investigational&aggFilters=phase:3,studyType:int
```
