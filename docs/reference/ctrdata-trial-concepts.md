# Trial concepts implemented across registers

`ctrdata` includes (since version 1.21.0) functions that implement
selected trial concepts. Concepts of clinical trials, such as their
start or status of recruitment, require to analyse several fields
against various pre-defined values. The structure and value sets of
fields differ between all
[ctrdata-registers](https://rfhb.github.io/ctrdata/reference/ctrdata-registers.md).
In this situation, the implemented trial concepts simplify and
accelerate a user's analysis workflow and also increase analysis
consistency.

## Details

The implementation of trial concepts in `ctrdata` has not been validated
with any formal approach, but has been checked for plausibility and
against expectations. The implementation is based on current
understanding, on public data models and on scientific papers, as
relevant. As with other `R` functions, call
[`help("f.startDate")`](https://rfhb.github.io/ctrdata/reference/f.startDate.md)
or print its implementation code by entering the name of the function as
command, e.g. `f.startDate`. Please raise an issue
[here](https://github.com/rfhb/ctrdata/issues) to ask about or improve a
trial concept.

The following trial concepts can be used by referencing their name when
calling
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md)
(parameter `calculate`). Concepts will continue to be refined and added;
last updated 2025-11-14.

- [f.assignmentType](https://rfhb.github.io/ctrdata/reference/f.assignmentType.md)
  (factor) was the assignment to treatment based on randomisation or
  not? ("R" or "NR")

- [f.controlType](https://rfhb.github.io/ctrdata/reference/f.controlType.md)
  (factor) which type of internal or concurrent control is used in the
  trial? ("none", "no-treatment", "placebo", "active", "placebo+active"
  or "other")

- [f.externalLinks](https://rfhb.github.io/ctrdata/reference/f.externalLinks.md)
  (character) provides links to publications or other external
  references

- [f.hasResults](https://rfhb.github.io/ctrdata/reference/f.hasResults.md)
  (logical) are any types of results recorded, e.g., structured data,
  reports or publications

- [f.isMedIntervTrial](https://rfhb.github.io/ctrdata/reference/f.isMedIntervTrial.md)
  (logical) is the trial interventional and does it have one or more
  medicines (drugs or biological) as investigational (experimental)
  intervention? (irrespective of status of authorisation and of study
  design)

- [f.isUniqueTrial](https://rfhb.github.io/ctrdata/reference/f.isUniqueTrial.md)
  (logical) is the trial record unique in the data frame of trial
  records, based on default parameters of
  [dbFindIdsUniqueTrials](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)?

- [f.likelyPlatformTrial](https://rfhb.github.io/ctrdata/reference/f.likelyPlatformTrial.md)
  (logical, list of likely related trials, and list of maybe related
  trials) is the trial possibly a (research) platform trial, and what
  are related trials? (based on trial title, `f.numTestArmsSubstances`,
  number of periods; identifiers of related trials; similarity of terms
  in parts of trial titles)

- [f.numSites](https://rfhb.github.io/ctrdata/reference/f.numSites.md)
  (integer) how many sites does the trial have?

- [f.numTestArmsSubstances](https://rfhb.github.io/ctrdata/reference/f.numTestArmsSubstances.md)
  (integer) how many arms or groups in the trial have medicines that are
  investigational? (cannot be calculated for ISRCTN or for phase 1
  trials)

- [f.primaryEndpointDescription](https://rfhb.github.io/ctrdata/reference/f.primaryEndpointDescription.md)
  (list of character) string containing protocol definition, details and
  time frames, concatenated with " == "

- [f.primaryEndpointResults](https://rfhb.github.io/ctrdata/reference/f.primaryEndpointResults.md)
  (columns of number, character, integer) returning the statistical
  testing p value and method as well as the number of subjects included
  in the test, each in one new column, for the first primary endpoint
  only

- [f.resultsDate](https://rfhb.github.io/ctrdata/reference/f.resultsDate.md)
  (date) the planned or achieved date of results availability

- [f.startDate](https://rfhb.github.io/ctrdata/reference/f.startDate.md)
  (date) the planned, authorised or documented date of start of
  recruitment

- [f.sampleSize](https://rfhb.github.io/ctrdata/reference/f.sampleSize.md)
  (integer) the planned or achieved number of subjects or participants
  recruited

- [f.sponsorType](https://rfhb.github.io/ctrdata/reference/f.sponsorType.md)
  (factor) a type or class of sponsor(s) that is simplified to "not for
  profit", "for profit", "mixed" or "other"

- [f.statusRecruitment](https://rfhb.github.io/ctrdata/reference/f.statusRecruitment.md)
  (factor) a status that is simplified to "ongoing" (includes
  temporarily halted), "completed", "ended early" (includes terminated
  or ended prematurely) and "other" (includes planned, not yet
  recruiting, stopped, withdrawn)

- [f.trialObjectives](https://rfhb.github.io/ctrdata/reference/f.trialObjectives.md)
  (string) identifies with letters those objectives that could be
  identified in text fragments, e.g. "E S PD D", with "E" (efficacy),
  "S" (safety), "D" (dose-finding)

- [f.trialPhase](https://rfhb.github.io/ctrdata/reference/f.trialPhase.md)
  (ordered factor) the phase(s) of medicine development with which a
  trial is associated ("phase 1", "phase 1+2" etc.)

- [f.trialPopulation](https://rfhb.github.io/ctrdata/reference/f.trialPopulation.md)
  (columns of factor, string and string) age groups (e.g., "P" for
  paediatric participants, "A" for adults, "E" for older than 65 years,
  or "P+A"), inclusion and exclusion criteria texts

- [f.trialTitle](https://rfhb.github.io/ctrdata/reference/f.trialTitle.md)
  (string) full or scientific title of the study

## Author

Ralf Herold <ralf.herold@mailbox.org>
