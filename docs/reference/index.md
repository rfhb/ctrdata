# Package index

- [`ctrdata-registers`](https://rfhb.github.io/ctrdata/reference/ctrdata-registers.md)
  : Information on clinical trial registers
- [`ctrdata-trial-concepts`](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
  : Trial concepts implemented across registers
- [`ctrdata`](https://rfhb.github.io/ctrdata/reference/ctrdata.md) :
  Getting started, database connection, function overview

## Load information from clinical trial registers

- [`ctrFindActiveSubstanceSynonyms()`](https://rfhb.github.io/ctrdata/reference/ctrFindActiveSubstanceSynonyms.md)
  : Find synonyms of an active substance
- [`ctrGenerateQueries()`](https://rfhb.github.io/ctrdata/reference/ctrGenerateQueries.md)
  : Generates queries that work across registers
- [`ctrGetQueryUrl()`](https://rfhb.github.io/ctrdata/reference/ctrGetQueryUrl.md)
  : Get register name and query parameters from search URL
- [`ctrLoadQueryIntoDb()`](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
  : Load and store register trial information
- [`ctrOpenSearchPagesInBrowser()`](https://rfhb.github.io/ctrdata/reference/ctrOpenSearchPagesInBrowser.md)
  : Open register to show query results or search page
- [`ctrShowOneTrial()`](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
  : Show full structure and all data of a trial

## Use database with downloaded trial information

- [`dbFindFields()`](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
  : Find names of fields in the database collection
- [`dbFindIdsUniqueTrials()`](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)
  : Get identifiers of deduplicated trial records
- [`dbGetFieldsIntoDf()`](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
  : Create data frame of specified fields or trial concepts from
  database collection
- [`dbQueryHistory()`](https://rfhb.github.io/ctrdata/reference/dbQueryHistory.md)
  : Show history of queries loaded into collection

## Operate on data frame from dbGetFieldsIntoDf

- [`dfMergeVariablesRelevel()`](https://rfhb.github.io/ctrdata/reference/dfMergeVariablesRelevel.md)
  : Merge variables, keeping type where possible, optionally relevel
  factors
- [`dfName2Value()`](https://rfhb.github.io/ctrdata/reference/dfName2Value.md)
  : Get value for variable of interest
- [`dfTrials2Long()`](https://rfhb.github.io/ctrdata/reference/dfTrials2Long.md)
  : Convert data frame with trial records into long format

## Calculate trial concepts

- [`f.assignmentType()`](https://rfhb.github.io/ctrdata/reference/f.assignmentType.md)
  : Calculate type of assignment to intervention in a study
- [`f.controlType()`](https://rfhb.github.io/ctrdata/reference/f.controlType.md)
  : Calculate type of control data collected in a study
- [`f.externalLinks()`](https://rfhb.github.io/ctrdata/reference/f.externalLinks.md)
  : Calculate the external references from a study's register record
- [`f.hasResults()`](https://rfhb.github.io/ctrdata/reference/f.hasResults.md)
  : Calculate if a study's results are available
- [`f.isMedIntervTrial()`](https://rfhb.github.io/ctrdata/reference/f.isMedIntervTrial.md)
  : Calculate if study is a medicine-interventional study
- [`f.isUniqueTrial()`](https://rfhb.github.io/ctrdata/reference/f.isUniqueTrial.md)
  : Calculate if record is unique for a study
- [`f.likelyPlatformTrial()`](https://rfhb.github.io/ctrdata/reference/f.likelyPlatformTrial.md)
  : Calculate if study is likely a platform trial or not
- [`f.numSites()`](https://rfhb.github.io/ctrdata/reference/f.numSites.md)
  : Calculate number of sites of a study
- [`f.numTestArmsSubstances()`](https://rfhb.github.io/ctrdata/reference/f.numTestArmsSubstances.md)
  : Calculate number of arms or groups with investigational medicines in
  a study
- [`f.primaryEndpointDescription()`](https://rfhb.github.io/ctrdata/reference/f.primaryEndpointDescription.md)
  : Calculate details of a primary endpoint of a study
- [`f.primaryEndpointResults()`](https://rfhb.github.io/ctrdata/reference/f.primaryEndpointResults.md)
  : Calculate details of a study's primary endpoint analysis and testing
- [`f.resultsDate()`](https://rfhb.github.io/ctrdata/reference/f.resultsDate.md)
  : Calculate date of results of a study
- [`f.sampleSize()`](https://rfhb.github.io/ctrdata/reference/f.sampleSize.md)
  : Calculate sample size of a study
- [`f.sponsorType()`](https://rfhb.github.io/ctrdata/reference/f.sponsorType.md)
  : Calculate type of sponsor of a study
- [`f.startDate()`](https://rfhb.github.io/ctrdata/reference/f.startDate.md)
  : Calculate start date of a study
- [`f.statusRecruitment()`](https://rfhb.github.io/ctrdata/reference/f.statusRecruitment.md)
  : Calculate status of recruitment of a study
- [`f.trialObjectives()`](https://rfhb.github.io/ctrdata/reference/f.trialObjectives.md)
  : Calculate objectives of a study
- [`f.trialPhase()`](https://rfhb.github.io/ctrdata/reference/f.trialPhase.md)
  : Calculate phase of a clinical trial
- [`f.trialPopulation()`](https://rfhb.github.io/ctrdata/reference/f.trialPopulation.md)
  : Calculate in- and exclusion criteria and age groups
- [`f.trialTitle()`](https://rfhb.github.io/ctrdata/reference/f.trialTitle.md)
  : Calculate the title of a study
