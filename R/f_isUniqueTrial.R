#### history ####
# 2025-01-27 first version

#' Calculate if record is unique for a study
#'
#' Trial concept calculated: Applies function dbFindIdsUniqueTrials() with
#' its defaults.
#'
#' @param df data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
#' prints fields needed in `df` for calculating this trial concept, which can
#' be used with \link{dbGetFieldsIntoDf}.
#'
#' @return data frame with columns `_id` and `.isUniqueTrial`, a logical.
#'
#' @export
#'
#' @examples
#' # fields needed
#' f.isUniqueTrial()
#'
#' \dontrun{
#'
#' # apply trial concept when creating data frame
#' dbc <- nodbi::src_sqlite(
#'   dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
#'   collection = "my_trials", flags = RSQLite::SQLITE_RO)
#' trialsDf <- dbGetFieldsIntoDf(
#'   calculate = "f.isUniqueTrial",
#'   con = dbc)
#' }
#'
f.isUniqueTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  #### fields ####
  # fldsNeeded <- c(
  #   "_id",
  #   "ctrname",
  #   # euctr
  #   "a2_eudract_number",
  #   "a52_us_nct_clinicaltrialsgov_registry_number",
  #   "trialInformation.usctnIdentifier",
  #   "a52_us_nct_clinicaltrialsgov_registry_number",
  #   "trialInformation.usctnIdentifier",
  #   "a51_isrctn_international_standard_randomised_controlled_trial_number",
  #   "trialInformation.isrctnIdentifier",
  #   "a41_sponsors_protocol_code_number",
  #   # ctgov
  #   "id_info.secondary_id",
  #   "id_info.org_study_id",
  #   "id_info.nct_id",
  #   "id_info.nct_id",
  #   "id_info.nct_alias",
  #   "id_info.secondary_id",
  #   "id_info.secondary_id",
  #   "id_info.org_study_id",
  #   # isrctn
  #   "externalRefs.eudraCTNumber",
  #   "externalRefs.clinicalTrialsGovNumber",
  #   "externalRefs.clinicalTrialsGovNumber",
  #   "isrctn",
  #   "externalRefs.protocolSerialNumber",
  #   # ctis
  #   "ctNumber",
  #   "eudraCtInfo.eudraCtCode",
  #   "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
  #   "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
  #   # ctgov2
  #   "protocolSection.identificationModule.nctId",
  #   "protocolSection.identificationModule.nctId",
  #   "protocolSection.identificationModule.secondaryIdInfos.id",
  #   "protocolSection.identificationModule.secondaryIdInfos.id",
  #   "protocolSection.identificationModule.nctIdAliases",
  #   "protocolSection.identificationModule.orgStudyIdInfo.id"
  # )
  # # manually copied from dfFindIdsUniqueTrials.R, line 220
  # fldsNeeded <- unique(fldsNeeded)

  # need at least one field
  fldsNeeded <- "ctrname"

  #### describe ####
  if (is.null(df)) {

    # generic, do not edit
    return(fldsNeeded)

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # apply function, access object con in calling environment
  vct <- dbFindIdsUniqueTrials(con = parent.frame()$con)

  # calculate result
  df[[".isUniqueTrial"]] <- df[["_id"]] %in% vct

  # keep only outcome columns
  df <- df[, c("_id", ".isUniqueTrial"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".isUniqueTrial"]], "logical"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end f.isUniqueTrial
