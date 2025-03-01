# function definition for dfCalculate

#### history ####
# 2025-01-27 first version


#' @noRd
#' @export
.isUniqueTrial <- function(df = NULL) {

  # check generic, do not edit
  stopifnot(is.data.frame(df) || is.null(df))

  #### fields ####
  fldsNeeded <- c(
    "_id",
    "ctrname",
    # euctr
    "a2_eudract_number",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a52_us_nct_clinicaltrialsgov_registry_number",
    "trialInformation.usctnIdentifier",
    "a51_isrctn_international_standard_randomised_controlled_trial_number",
    "trialInformation.isrctnIdentifier",
    "a41_sponsors_protocol_code_number",
    # ctgov
    "id_info.secondary_id",
    "id_info.org_study_id",
    "id_info.nct_id",
    "id_info.nct_id",
    "id_info.nct_alias",
    "id_info.secondary_id",
    "id_info.secondary_id",
    "id_info.org_study_id",
    # isrctn
    "externalRefs.eudraCTNumber",
    "externalRefs.clinicalTrialsGovNumber",
    "externalRefs.clinicalTrialsGovNumber",
    "isrctn",
    "externalRefs.protocolSerialNumber",
    # ctis
    "ctNumber",
    "eudraCtInfo.eudraCtCode",
    "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
    "authorizedPartI.trialDetails.clinicalTrialIdentifiers.secondaryIdentifyingNumbers.nctNumber.number",
    # ctgov2
    "protocolSection.identificationModule.nctId",
    "protocolSection.identificationModule.nctId",
    "protocolSection.identificationModule.secondaryIdInfos.id",
    "protocolSection.identificationModule.secondaryIdInfos.id",
    "protocolSection.identificationModule.nctIdAliases",
    "protocolSection.identificationModule.orgStudyIdInfo.id"
  )
  # manually copied from dfFindIdsUniqueTrials.R, line 220
  fldsNeeded <- unique(fldsNeeded)

  #### describe ####
  if (is.null(df)) {

    txt <- '
Applies function dbFindIdsUniqueTrials() with its defaults.
Returns a logical.
    '

    # generic, do not edit
    fctDescribe(match.call()[[1]], txt, fldsNeeded)
    return(invisible(fldsNeeded))

  } # end describe


  #### calculate ####

  # check generic, do not edit
  fctChkFlds(names(df), fldsNeeded)

  # apply function
  vct <- .dbFindIdsUniqueTrials(listofIds = df)
  vct <- df[["_id"]] %in% vct
  df[[".isUniqueTrial"]] <- vct

  # keep only outcome columns
  df <- df[, c("_id", ".isUniqueTrial"), drop = FALSE]


  #### checks ####
  stopifnot(inherits(df[[".isUniqueTrial"]], "logical"))
  stopifnot(ncol(df) == 2L)

  # return
  return(df)

} # end .isUniqueTrial
