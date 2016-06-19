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
#' In the database, a list of de-duplicated identifiers of
#' clinical trial records can be obtained with
#' \link{dbFindIdsUniqueTrials}.
#'
#' In a dataframe, the preferred of duplicate records of
#' a trial from EUCTR can be obtained with
#' \link{dfFindUniqueEuctrRecord}.
#'
#' @docType package
#' @name ctrdata
NULL
