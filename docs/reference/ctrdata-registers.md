# Information on clinical trial registers

Registers of the four clinical trial registers from which package
[ctrdata](https://rfhb.github.io/ctrdata/reference/ctrdata.md) can
retrieve, aggregate and analyse protocol- and result-related information
as well as documents, last updated 2026-10-04.

## 1 - Overview

- **EUCTR**: The EU Clinical Trials Register is complete with more than
  44,400 clinical trials (at least one investigational medicinal
  product, IMP; in the European Union and beyond), including more than
  26,200 trials with results, which continue to be added.

- **CTIS**: The EU Clinical Trials Information System, launched in 2023,
  holds more than 12,500 publicly accessible clinical trials, including
  more than 1,550 with results or a report. (To automatically get CTIS
  search query URLs, see
  [here](https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser))

- **CTGOV2**: ClinicalTrials.gov holds more than 605,000 interventional
  and observational studies, including more than 37,400 applicable
  clinical trials (ACT, e.g. interventional studies with an
  Investigational Drug or Device, see
  [here](https://clinicaltrials.gov/expert-search?term=AREA%5BStudyType%5D%20Interventional%20AND%20AREA%5BStartDate%5D%20RANGE%5B01%2F18%2F2017,%20MAX%5D%20AND%20(AREA%5BIsFDARegulatedDrug%5D%20Yes%20OR%20AREA%5BIsFDARegulatedDevice%5D%20Yes)%20AND%20NOT%20AREA%5BPhase%5D%20(%22Early%20Phase%201%22%20OR%20%22Phase%201%22)%20AND%20NOT%20AREA%5BDesignPrimaryPurpose%5D%20%22Device%20Feasibility%22%20AND%20(AREA%5BLocationCountry%5D%20(%22United%20States%22%20OR%20%22Puerto%20Rico%22%20OR%20%22Guam%22%20OR%20%22American%20Samoa%22%20OR%20%22Northern%20Mariana%20Islands%22%20OR%20%22U.S.%20Virgin%20Islands%22)%20OR%20AREA%5BIsUSExport%5D%20Yes))
  ), of which more than 14,400 with results.

- **ISRCTN**: The ISRCTN Registry holds more than 28,400 interventional
  and observational health studies.

[TABLE]

## 2 - Notable changes

- EUCTR removed search parameter `status=` as of February 2025 (see
  transitioned trials in
  [dbFindIdsUniqueTrials](https://rfhb.github.io/ctrdata/reference/dbFindIdsUniqueTrials.md)).

- CTIS can be used with `ctrdata` since 2023-03-25. CTIS was relaunched
  on 2024-06-17, changing the data structure and search syntax, to which
  `ctrdata` was updated.

- CTGOV "classic" was retired on 2024-06-25; `ctrdata` subsequently
  translates CTGOV queries to CTGOV2 queries. The new website ("CTGOV2")
  can be used with `ctrdata` since 2023-08-27. Database collections
  created with CTGOV queries can still be used since functions in
  `ctrdata` continue to support them.

More information on changes:
[here](https://rfhb.github.io/ctrdata/news/index.html).

## 3 - References

|  |  |  |  |  |
|----|----|----|----|----|
| **Material** | **EUCTR** | **CTIS** | **CTGOV2** | **ISRCTN** |
| About | [link](https://www.clinicaltrialsregister.eu/about.html) | [link](https://euclinicaltrials.eu/about-this-website/) | [link](https://clinicaltrials.gov/about-site/about-ctg) | [link](https://www.isrctn.com/page/about) |
| Terms & conditions, disclaimer | [link](https://www.clinicaltrialsregister.eu/disclaimer.html) | [link](https://euclinicaltrials.eu/guidance-and-q-as/) | [link](https://clinicaltrials.gov/about-site/terms-conditions) | [link](https://www.isrctn.com/page/faqs#using-the-isrctn) |
| How to search | [link](https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf) | [link](https://euclinicaltrials.eu/search-tips-and-guidance/) | [link](https://clinicaltrials.gov/find-studies/how-to-search) | [link](https://www.isrctn.com/page/search-tips) |
| Search interface | [link](https://www.clinicaltrialsregister.eu/ctr-search/search) | [link](https://euclinicaltrials.eu/search-for-clinical-trials/) | [link](https://clinicaltrials.gov/) | [link](https://www.isrctn.com/) |
| Expert / advanced search | [link](https://www.clinicaltrialsregister.eu/ctr-search/search) | [link](https://euclinicaltrials.eu/ctis-public/search) | [link](https://clinicaltrials.gov/expert-search) | [link](https://www.isrctn.com/editAdvancedSearch) |
| Glossary / related information | [link](https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf) | [link](https://accelerating-clinical-trials.europa.eu/) | [link](https://clinicaltrials.gov/study-basics/glossary) | [link](https://www.who.int/clinical-trials-registry-platform/network/who-data-set) |
| FAQ / caveats / examples | [link](https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf) | [link](https://euclinicaltrials.eu/website-outages-and-system-releases/) | [link](https://clinicaltrials.gov/policy/faq), [link](https://clinicaltrials.gov/about-site/selected-publications), [link](https://clinicaltrials.gov/submit-studies/prs-help/support-training-materials#example-studies) | [link](https://www.isrctn.com/page/faqs) |
| Data dictionaries / structure | [link](https://eudract.ema.europa.eu/result.html), [link](https://eudract.ema.europa.eu/docs/technical/EudraCT%20protocol%20related%20data%20dictionary.xls), [link](https://eudract.ema.europa.eu/docs/technical/V7_V8_Country_List_20210804.xlsx) | [XLSX files](https://www.ema.europa.eu/en/human-regulatory-overview/research-development/clinical-trials-human-medicines/clinical-trials-information-system-ctis-training-support) | [link](https://cdn.clinicaltrials.gov/documents/xsd/public.xsd), [link](https://clinicaltrials.gov/data-about-studies/study-data-structure), [link](https://cdn.clinicaltrials.gov/documents/tutorial/content/index.html) | [link](https://www.isrctn.com/page/definitions) |

Some registers are expanding entered search terms using dictionaries
([example](https://clinicaltrials.gov/data-api/about-api/search-areas)).

## 4 - Example and ctrdata motivation

The example is an expert search for interventional clinical trials
primarily with neonates, investigating medicines for infectious
conditions. It shows that searches in the web interface of most
registers are not sufficient to identify and analyse the trials of
interest:

- EUCTR has a search box to retrieve trials with neonates, but not only
  those conducted exclusively in neonates
  ([link](https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants)).

- CTIS retrieves trials based on their information mentioning the words
  neonates and infection
  ([link](https://euclinicaltrials.eu/ctis-public/search#searchCriteria=%7B%22containAll%22:%22infection%22,%22containAny%22:%22neonates%22%7D);
  to show CTIS search results, see
  [here](https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser)).

- CTGOV2 can retrieve trials with neonates only if a specific age
  definition is used
  ([link](https://clinicaltrials.gov/search?ageRange=0M_1M&cond=Infections&aggFilters=studyType:int&distance=50&intr=Investigational+Agent)).

- ISRCTN retrieves studies only with neonates, but not only of medicines
  ([link](https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search)).

To address these issues, trials can first be retrieved using the links
above with
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md).
In a second step, all trial or a subset can be analysed.

`ctrdata` supports users with pre-defined
[ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
that can be calculated across registers when creating a dataframe for
analysis, such as
[f.isMedIntervTrial](https://rfhb.github.io/ctrdata/reference/f.isMedIntervTrial.md)
which identifies the subset of investigational medicine trials.

In addition, all fields made public by the registers can be searched
with
[dbFindFields](https://rfhb.github.io/ctrdata/reference/dbFindFields.md);
the trial's structure and the fields' values can be reviewed with
[ctrShowOneTrial](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md).

For the example, the following fields can be included when creating a
dataframe using
[dbGetFieldsIntoDf](https://rfhb.github.io/ctrdata/reference/dbGetFieldsIntoDf.md),
to select or analyse trials:

- EUCTR: `f113_newborns_027_days`,
  `f1131_number_of_subjects_for_this_age_range`

- CTIS:
  `authorizedApplication.authorizedPartI.medicalConditions.medicalCondition`

- CTGOV2: `protocolSection.eligibilityModule.maximumAge`

- ISRCTN: `participants.upperAgeLimit`

See vignette
[ctrdata_summarise](https://rfhb.github.io/ctrdata/doc/ctrdata_summarise.md)
for several other examples.

## Author

Ralf Herold <ralf.herold@mailbox.org>
