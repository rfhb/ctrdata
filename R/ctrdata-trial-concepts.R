#' Trial concepts implemented across registers
#'
#' `ctrdata` includes (since version 1.21.0) functions that implement selected
#' trial concepts. Concepts of clinical trials, such as their start or status of
#' recruitment, require to analyse several fields against various
#' pre-defined values. The structure and value sets of fields differ between
#' all \link{ctrdata-registers}. In this situation, the implemented trial
#' concepts simplify and accelerate a user's analysis workflow and also increase
#' analysis consistency.
#'
#' The implementation of trial concepts in `ctrdata` has not been validated
#' with any formal approach, but has been checked for plausibility and
#' against expectations. The implementation is based on current
#' understanding, on public data models and on scientific papers, as relevant.
#' As with other `R` functions, call \code{help("f.startDate")} or print its
#' implementation code by entering the name of the function as command,
#' e.g. `f.startDate`.
#' Please raise an issue \href{https://github.com/rfhb/ctrdata/issues}{here}
#' to ask about or improve a trial concept.
#'
#' The following trial concepts can be used by referencing their name when
#' calling \link{dbGetFieldsIntoDf} (parameter `calculate`).
#' Concepts will continue to be refined and added;
#' last updated 2025-08-21.
#'
#' - \link{f.assignmentType} (factor) was the assignment to treatment based on
#' randomisation or not? ("R" or "NR")
#'
#' - \link{f.controlType} (factor) which type of internal or concurrent control
#' is used in the trial? ("none", "no-treatment", "placebo", "active",
#' "placebo+active" or "other")
#'
#' - \link{f.externalLinks} (character) provides links to publications or
#' other external references
#'
#' - \link{f.hasResults} (logical) are any types of results recorded, e.g.,
#' structured data, reports or publications
#'
#' - \link{f.isMedIntervTrial} (logical) is the trial interventional and does
#' it have one or more medicines (drugs or biological) as investigational
#' (experimental) intervention? (irrespective of status of authorisation and
#' of study design)
#'
#' - \link{f.isUniqueTrial} (logical) is the trial record unique in the data
#' frame of trial records, based on default parameters of
#' \link{dbFindIdsUniqueTrials}?
#'
#' - \link{f.likelyPlatformTrial} (logical, list of likely related trials, and
#' list of maybe related trials) is the trial possibly a (research)
#' platform trial, and what are related trials? (based on trial title,
#' `f.numTestArmsSubstances`, number of periods; identifiers of related trials;
#' similarity of terms in parts of trial titles)
#'
#' - \link{f.numSites} (integer) how many sites does the trial have?
#'
#' - \link{f.numTestArmsSubstances} (integer) how many arms or groups in the
#' trial have medicines that are investigational? (cannot be calculated for
#' ISRCTN or for phase 1 trials)
#'
#' - \link{f.primaryEndpointDescription} (list of character) string containing
#' protocol definition, details and time frames, concatenated with " == "
#'
#' - \link{f.primaryEndpointResults} (columns of number, character, integer)
#' returning the statistical testing p value and method as well as the
#' number of subjects included in the test, each in one new column, for the
#' first primary endpoint only
#'
#' - \link{f.resultsDate} (date) the planned or achieved date of results availability
#'
#' - \link{f.startDate} (date) the planned, authorised or documented date
#' of start of recruitment
#'
#' - \link{f.sampleSize} (integer) the planned or achieved number of subjects
#' or participants recruited
#'
#' - \link{f.sponsorType} (factor) a type or class of sponsor(s) that
#' is simplified to "not for profit", "for profit", "mixed" or "other"
#'
#' - \link{f.statusRecruitment} (factor) a status that is simplified to
#' "ongoing" (includes temporarily halted), "completed", "ended early" (includes
#' terminated or ended prematurely) and "other" (includes planned, not yet
#' recruiting, stopped, withdrawn)
#'
#' - \link{f.trialObjectives} (string) identifies with letters those objectives
#' that could be identified in text fragments, e.g. "E S PD D", with "E"
#' (efficacy), "S" (safety), "D" (dose-finding)
#'
#' - \link{f.trialPhase} (ordered factor) the phase(s) of medicine development
#' with which a trial is associated ("phase 1", "phase 1+2" etc.)
#'
#' - \link{f.trialPopulation} (columns of factor, string and string) age groups
#' (e.g., "P" for paediatric participants, "A" for adults, "E" for older than
#' 65 years, or "P+A"), inclusion and exclusion criteria texts
#'
#' - \link{f.trialTitle} (string) full or scientific title of the study
#'
#' @name ctrdata-trial-concepts
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
