## ----setup, include=FALSE-----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----install_ctrdata, eval=FALSE----------------------------------------------------------------------------------------
# install.packages("ctrdata")

## ----eval=FALSE---------------------------------------------------------------------------------------------------------
# # install package under development
# install.packages(c("remotes"))
# remotes::install_github("rfhb/ctrdata", dependencies = TRUE, build_vignettes = TRUE)

## -----------------------------------------------------------------------------------------------------------------------
# # user to specify their directory of choice;
# # remember to set option for each new R session
# options(duckdb.extension_directory = "~/.duckdb_extensions")
# 
# # load and store in above-mentioned
# # directory; only once to be executed
# DBI::dbExecute(duckdb::dbConnect(duckdb::duckdb()), 'INSTALL json;')

## ----attach_ctrdata-----------------------------------------------------------------------------------------------------
# library(ctrdata)

## ----cite_ctrdata, eval=TRUE, results='asis', echo=c(-1)----------------------------------------------------------------
cat(rev(format(citation("ctrdata"), style = "text")), sep = " or <br/>")

