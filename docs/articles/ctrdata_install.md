# Install R package ctrdata

## Install package `ctrdata` on an R system

The R Project website (<https://www.r-project.org/>) provides installers
for the R system. It can be used with software products and graphical
user interfaces such as [R
Studio](https://posit.co/products/open-source/rstudio/), or from [Visual
Studio Code](https://github.com/REditorSupport/vscode-R).

General information on the `ctrdata` package is available here:
<https://rfhb.github.io/ctrdata/>.

In R, execute:

``` r

install.packages("ctrdata")
```

For using the development version of package `ctrdata`, install from
GitHub:

``` r

# install package under development
install.packages(c("remotes"))
remotes::install_github("rfhb/ctrdata", dependencies = TRUE, build_vignettes = TRUE)
```

Either of the above should install package `ctrdata` into the user’s
library.

## Databases to use

At this time, a PostgreSQL, DuckDB, an SQLite or a MongoDB (local or
remote) database can be used with the package `ctrdata`. A full SQLite
database is provided in the R package
[`RSQLite`](https://rsqlite.r-dbi.org/). Suggested installation
instructions for PostgreSQL are
[here](https://www.postgresql.org/download/) and for a local MongoDB
server are
[here](https://www.mongodb.com/docs/manual/administration/install-community/);
a remote MongoDB database server is accessible
[here](https://www.mongodb.com/cloud/atlas). See
[here](https://github.com/ropensci/nodbi#benchmark) for a speed
comparison of the databases; recommended: DuckDB, PostgreSQL or MongoDB
local server.

| Purpose | Function call |
|----|----|
| Create **SQLite** database connection | `dbc <- nodbi::src_sqlite(dbname = "name_of_my_database", collection = "name_of_my_collection")` |
| Create **DuckDB** database connection | `dbc <- nodbi::src_duckdb(dbname = "name_of_my_database", collection = "name_of_my_collection")` |
| Create **MongoDB** database connection | `dbc <- nodbi::src_mongo(db = "name_of_my_database", collection = "name_of_my_collection")` |
| Create **PostgreSQL** database connection | `dbc <- nodbi::src_postgres(dbname = "name_of_my_database"); dbc[["collection"]] <- "name_of_my_collection"` |
| Use connection with `ctrdata` functions | `ctrdata::{ctrLoadQueryIntoDb, dbQueryHistory, dbFindIdsUniqueTrials, dbFindFields, dbGetFieldsIntoDf}(con = dbc, ...)` |

## Attach package `ctrdata`

After completing the installation, attach the package from the user’s
library:

``` r

library(ctrdata)
```

## Credit to clinical trial registers

Remember to respect the registers’ terms and conditions (see
`ctrOpenSearchPagesInBrowser(copyright = TRUE)`).

## Quote or reference `ctrdata`

In any publication, please cite this package as follows:

Herold R (2025). “Aggregating and analysing clinical trials data from
multiple public registers using R package ctrdata.” *Research Synthesis
Methods*, 1–33. <doi:10.1017/rsm.2025.10061>
<https://doi.org/10.1017/rsm.2025.10061>. or  
Herold R (2025). *ctrdata: Retrieve and Analyze Clinical Trials Data
from Public Registers*. R package version 1.25.1.9000,
<https://cran.r-project.org/package=ctrdata>.
