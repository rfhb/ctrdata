#' Trial concepts implemented across registers
#'
#' `ctrdata` includes (since version 1.21.0) functions that implement selected
#' trial concepts. Concepts of clinical trials, such as their start or status of
#' recruitment, require to analyse several fields against various
#' pre-defined values. In addition, the structure and value sets of data
#' differ between all \link{ctrdata-registers}.
#' The included trial concepts are intended to simplify and accelerate a
#' user's workflow as well as to provide consistency of analyses.
#'
#' The implementation of trial concepts in `ctrdata` is based on current
#' understanding, publication of data models and scientific papers as relevant.
#' Call \link{dfCalculate} for one or more such functions to review the
#' explanation and the fields needed. As with other `R` functions,
#' the code is printed when entering as command just the name of the function,
#' e.g. `.startDate`.
#' Raise an issue \href{https://github.com/rfhb/ctrdata/issues}{here}
#' to ask about or improve a concept.
#'
#' The following trial concepts can be used by referencing their name when
#' calling \link{dbGetFieldIntoDf} (parameter `calculate`) and
#' \link{dfCalculate} (parameter `name`).
#' Concepts will continue to be refined and added;
#' last updated 2025-02-01.
#'
#' - **.isMedIntervTrial** (logical) is the trial interventional and does
#' it have one or more medicines (drugs or biological) as investigational
#' (experimental) intervention? (irrespective of status of authorisation and
#' of study design)
#'
#' - **.isUniqueTrial** (logical) is the trial record unique in the data frame
#' of trial, based on the default parameters of \link{dbFindIdsUniqueTrials}?
#'
#' - **.numTestArmsSubstances** (integer) how many arms or groups have
#' medicines that are investigational? (cannot be calculated for ISRCTN)
#'
#' - **.isPlatformTrial** (logical) is the trial possibly a (research) platform
#' trial? (based on title, .numTestArmsSubstances, number of periods)
#'
#' - **.startDate** (date) the planned, authorised or documented date
#' of start of recruitment
#'
#' - **.statusRecruitment** (ordered factor) a status that is simplified to
#' ongoing (includes temporarily halted), completed and other (includes
#' planned, stopped, terminated)
#'
#' @name ctrdata-trial-concepts
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
