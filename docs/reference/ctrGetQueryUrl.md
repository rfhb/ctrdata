# Get register name and query parameters from search URL

Extracts query parameters and register name from parameter \`url\` or
from the clipboard, into which the URL of a register search was copied.

## Usage

``` r
ctrGetQueryUrl(url = "", register = "")
```

## Arguments

- url:

  URL such as from the browser address bar. If not specified, clipboard
  contents will be checked for a suitable URL. For automatically copying
  the user's query of a register in a web browser to the clipboard, see
  [here](https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser).
  Can also contain a query term such as from
  [dbQueryHistory](https://rfhb.github.io/ctrdata/reference/dbQueryHistory.md)()\["query-term"\].
  Can also be an identifier of a trial, which based on its format will
  indicate to which register it relates.

- register:

  Optional name of register (one of "EUCTR", "CTGOV2" "ISRCTN" or
  "CTIS") in case \`url\` is a query term but not a full URL

## Value

A data frame (or tibble, if `tibble` is loaded) with column names
\`query-term\` and \`query-register\`. The data frame (or tibble) can be
passed as such as parameter \`queryterm\` to
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
and as parameter \`url\` to
[ctrOpenSearchPagesInBrowser](https://rfhb.github.io/ctrdata/reference/ctrOpenSearchPagesInBrowser.md).

## Details

To obtain the search query from CTIS, consider installing the
[Tampermonkey browser extension](https://www.tampermonkey.net/), click
on the extension icon, "Create a new script", "Utility" and then "Import
from this URL":
\`https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js\`
This will copy query URLs from all registers into the clipboard; see
[ctrOpenSearchPagesInBrowser](https://rfhb.github.io/ctrdata/reference/ctrOpenSearchPagesInBrowser.md)
for additional uses.

## Examples

``` r

# user copied into the clipboard the URL from
# the address bar of the browser that shows results
# from a query in one of the trial registers
if (interactive()) try(ctrGetQueryUrl(), silent = TRUE)

# extract query parameters from search result URL
# (URL was cut for the purpose of formatting only)
ctrGetQueryUrl(
    url = paste0(
        "https://classic.clinicaltrials.gov/ct2/results?",
        "cond=&term=AREA%5BMaximumAge%5D+RANGE%5B0+days%2C+28+days%5D",
        "&type=Intr&rslt=&age_v=&gndr=&intr=Drugs%2C+Investigational",
        "&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=",
        "&locn=&phase=2&rsub=&strd_s=01%2F01%2F2015&strd_e=01%2F01%2F2016",
        "&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&rfpd_s=&rfpd_e=&lupd_s=&lupd_e=&sort="
    )
)
#> Since 2024-06-25, the classic CTGOV servers are no longer available. Package ctrdata has translated the classic CTGOV query URL from this call of function ctrLoadQueryIntoDb(queryterm = ...) into a query URL that works with the current CTGOV2. This is printed below and is also part of the return value of this function, ctrLoadQueryIntoDb(...)$url. This URL can be used with ctrdata functions. Note that the fields and data schema of trials differ between CTGOV and CTGOV2. 
#> 
#> Replace this URL:
#> 
#> https://classic.clinicaltrials.gov/ct2/results?term=AREA%5BMaximumAge%5D+RANGE%5B0+days%2C+28+days%5D&type=Intr&intr=Drugs%2C+Investigational&phase=2&strd_s=01%2F01%2F2015&strd_e=01%2F01%2F2016
#> 
#> with this URL:
#> 
#> https://clinicaltrials.gov/search?start=2015-01-01_2016-01-01&term=AREA[MaximumAge] RANGE[0 days, 28 days]&intr=Drugs, Investigational&aggFilters=phase:3,studyType:int
#> * Found search query from CTGOV2: start=2015-01-01_2016-01-01&term=AREA[MaximumAge] RANGE[0 days, 28 days]&intr=Drugs, Investigational&aggFilters=phase:3,studyType:int
#> # A tibble: 1 × 2
#>   `query-term`                                                  `query-register`
#>   <chr>                                                         <chr>           
#> 1 start=2015-01-01_2016-01-01&term=AREA[MaximumAge] RANGE[0 da… CTGOV2          

# other examples
ctrGetQueryUrl("https://www.clinicaltrialsregister.eu/ctr-search/trial/2007-000371-42/results")
#> * Found search query from EUCTR: query=2007-000371-42
#> # A tibble: 1 × 2
#>   `query-term`         `query-register`
#>   <chr>                <chr>           
#> 1 query=2007-000371-42 EUCTR           
ctrGetQueryUrl("https://euclinicaltrials.eu/ctis-public/view/2022-500041-24-00")
#> * Found search query from CTIS: searchCriteria={"number":"2022-500041-24-00"}
#> # A tibble: 1 × 2
#>   `query-term`                                        `query-register`
#>   <chr>                                               <chr>           
#> 1 "searchCriteria={\"number\":\"2022-500041-24-00\"}" CTIS            
ctrGetQueryUrl("https://classic.clinicaltrials.gov/ct2/show/NCT01492673?cond=neuroblastoma")
#> * Note: 'url' shows a single trial (and is returned by the function) but also had search parameters: If interested in search results, click 'Return to List' in browser and use this as 'url'.
#> Since 2024-06-25, the classic CTGOV servers are no longer available. Package ctrdata has translated the classic CTGOV query URL from this call of function ctrLoadQueryIntoDb(queryterm = ...) into a query URL that works with the current CTGOV2. This is printed below and is also part of the return value of this function, ctrLoadQueryIntoDb(...)$url. This URL can be used with ctrdata functions. Note that the fields and data schema of trials differ between CTGOV and CTGOV2. 
#> 
#> Replace this URL:
#> 
#> https://classic.clinicaltrials.gov/ct2/results?term=NCT01492673
#> 
#> with this URL:
#> 
#> https://clinicaltrials.gov/search?term=NCT01492673
#> * Found search query from CTGOV2: term=NCT01492673
#> # A tibble: 1 × 2
#>   `query-term`     `query-register`
#>   <chr>            <chr>           
#> 1 term=NCT01492673 CTGOV2          
ctrGetQueryUrl("https://clinicaltrials.gov/ct2/show/NCT01492673?cond=neuroblastoma")
#> * Note: 'url' shows a single trial (and is returned by the function) but also had search parameters: If interested in search results, click 'Return to List' in browser and use this as 'url'.
#> Since 2024-06-25, the classic CTGOV servers are no longer available. Package ctrdata has translated the classic CTGOV query URL from this call of function ctrLoadQueryIntoDb(queryterm = ...) into a query URL that works with the current CTGOV2. This is printed below and is also part of the return value of this function, ctrLoadQueryIntoDb(...)$url. This URL can be used with ctrdata functions. Note that the fields and data schema of trials differ between CTGOV and CTGOV2. 
#> 
#> Replace this URL:
#> 
#> https://classic.clinicaltrials.gov/ct2/results?term=NCT01492673
#> 
#> with this URL:
#> 
#> https://clinicaltrials.gov/search?term=NCT01492673
#> * Found search query from CTGOV2: term=NCT01492673
#> # A tibble: 1 × 2
#>   `query-term`     `query-register`
#>   <chr>            <chr>           
#> 1 term=NCT01492673 CTGOV2          
ctrGetQueryUrl("https://clinicaltrials.gov/study/NCT01467986?aggFilters=ages:child")
#> * Note: 'url' shows a single trial (and is returned by the function) but also had search parameters: If interested in search results, click on 'Search Results' in browser and use this as 'url'.
#> * Found search query from CTGOV2: id=NCT01467986
#> # A tibble: 1 × 2
#>   `query-term`   `query-register`
#>   <chr>          <chr>           
#> 1 id=NCT01467986 CTGOV2          
ctrGetQueryUrl("https://www.isrctn.com/ISRCTN70039829")
#> * Found search query from ISRCTN: q=ISRCTN70039829
#> # A tibble: 1 × 2
#>   `query-term`     `query-register`
#>   <chr>            <chr>           
#> 1 q=ISRCTN70039829 ISRCTN          

# using identifiers of single trials
ctrGetQueryUrl("70039829")
#> * Found search query from ISRCTN: q=70039829
#> # A tibble: 1 × 2
#>   `query-term` `query-register`
#>   <chr>        <chr>           
#> 1 q=70039829   ISRCTN          
ctrGetQueryUrl("ISRCTN70039829")
#> * Found search query from ISRCTN: q=ISRCTN70039829
#> # A tibble: 1 × 2
#>   `query-term`     `query-register`
#>   <chr>            <chr>           
#> 1 q=ISRCTN70039829 ISRCTN          
ctrGetQueryUrl("NCT00617929")
#> * Found search query from CTGOV2: term=NCT00617929
#> # A tibble: 1 × 2
#>   `query-term`     `query-register`
#>   <chr>            <chr>           
#> 1 term=NCT00617929 CTGOV2          
ctrGetQueryUrl("2022-501142-30-00")
#> * Found search query from CTIS: searchCriteria={"number":"2022-501142-30-00"}
#> # A tibble: 1 × 2
#>   `query-term`                                        `query-register`
#>   <chr>                                               <chr>           
#> 1 "searchCriteria={\"number\":\"2022-501142-30-00\"}" CTIS            
ctrGetQueryUrl("2012-003632-23")
#> * Found search query from EUCTR: query=2012-003632-23
#> # A tibble: 1 × 2
#>   `query-term`         `query-register`
#>   <chr>                <chr>           
#> 1 query=2012-003632-23 EUCTR           
```
