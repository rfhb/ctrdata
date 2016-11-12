#' ctrdata: Overview on functions.
#'
#' The ctrdata package provides three categories of important functions,
#' in sequence of their use in a workflow:
#'
#' @section Operations on a clinical trial register:
#'
#' \link{ctrOpenSearchPagesInBrowser},
#' \link{ctrGetQueryUrlFromBrowser},
#' \link{ctrLoadQueryIntoDb}
#'
#' @section Using the database that holds downloaded information:
#'
#' \link{dbFindVariable},
#' \link{ctrQueryHistoryInDb}
#'
#' @section Getting R dataframes from clinical trial information in the database:
#'
#' \link{dbGetVariablesIntoDf},
#' \link{dfMergeTwoVariablesRelevel}.
#'
#' @section Deduplication:
#'
#' From the database, a vector of de-duplicated identifiers of
#' clinical trial records can be obtained with
#' \link{dbFindIdsUniqueTrials} and this can be used to select
#' subsets of interest from R dataframes.
#'
#' @docType package
#' @name ctrdata
NULL
