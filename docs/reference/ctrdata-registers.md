# Information on clinical trial registers

Registers of the four clinical trial registers from which package
[ctrdata](https://rfhb.github.io/ctrdata/reference/ctrdata.md) can
retrieve, aggregate and analyse protocol- and result-related information
as well as documents, last updated 2025-12-31.

## 1 - Overview

- **EUCTR**: The EU Clinical Trials Register is complete with 44,366
  clinical trials (at least one investigational medicinal product, IMP;
  in the European Union and beyond), including more than 25,800 trials
  with results, which continue to be added.

- **CTIS**: The EU Clinical Trials Information System, launched in 2023,
  holds more than 10,650 publicly accessible clinical trials, including
  more than 520 with results or a report. (To automatically get CTIS
  search query URLs, see
  [here](https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser))

- **CTGOV2**: ClinicalTrials.gov holds almost 564,000 interventional and
  observational studies, including more than 17,000 interventional
  studies with results.

- **ISRCTN**: The ISRCTN Registry holds almost 27,500 interventional and
  observational health studies, including almost 15,000 studies with
  results.

[TABLE]

## 2 - Notable changes

CTGOV "classic" was retired on 2024-06-25; `ctrdata` subsequently
translates CTGOV queries to CTGOV2 queries. The new website ("CTGOV2")
can be used with `ctrdata` since 2023-08-27. Database collections
created with CTGOV queries can still be used since functions in
`ctrdata` continue to support them. CTIS was relaunched on 2024-06-17,
changing the data structure and search syntax, to which `ctrdata` was
updated. CTIS can be used with `ctrdata` since 2023-03-25. EUCTR removed
search parameter `status=` as of February 2025. More information on
changes: [here](https://rfhb.github.io/ctrdata/news/index.html).

## 3 - References

|  |  |  |  |  |
|----|----|----|----|----|
| **Material** | **EUCTR** | **CTGOV2** | **ISRCTN** | **CTIS** |
| About | [link](https://www.clinicaltrialsregister.eu/about.html) | [link](https://clinicaltrials.gov/about-site/about-ctg) | [link](https://www.isrctn.com/page/about) | [link](https://euclinicaltrials.eu/about-this-website/) |
| Terms & conditions, disclaimer | [link](https://www.clinicaltrialsregister.eu/disclaimer.html) | [link](https://clinicaltrials.gov/about-site/terms-conditions) | [link](https://www.isrctn.com/page/faqs#using-the-isrctn) | [link](https://euclinicaltrials.eu/guidance-and-q-as/) |
| How to search | [link](https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf) | [link](https://clinicaltrials.gov/find-studies/how-to-search) | [link](https://www.isrctn.com/page/search-tips) | [link](https://euclinicaltrials.eu/search-tips-and-guidance/) |
| Search interface | [link](https://www.clinicaltrialsregister.eu/ctr-search/search) | [link](https://clinicaltrials.gov/) | [link](https://www.isrctn.com/) | [link](https://euclinicaltrials.eu/search-for-clinical-trials/) |
| Expert / advanced search | [link](https://www.clinicaltrialsregister.eu/ctr-search/search) | [link](https://clinicaltrials.gov/expert-search) | [link](https://www.isrctn.com/editAdvancedSearch) | [link](https://euclinicaltrials.eu/ctis-public/search) |
| Glossary / related information | [link](https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf) | [link](https://clinicaltrials.gov/study-basics/glossary) | [link](https://www.who.int/clinical-trials-registry-platform/network/who-data-set) | [link](https://accelerating-clinical-trials.europa.eu/) |
| FAQ / caveats / examples | [link](https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf) | [link](https://clinicaltrials.gov/policy/faq), [link](https://clinicaltrials.gov/about-site/selected-publications), [link](https://clinicaltrials.gov/submit-studies/prs-help/support-training-materials#example-studies) | [link](https://www.isrctn.com/page/faqs) | [link](https://euclinicaltrials.eu/website-outages-and-system-releases/) |
| Data dictionaries / structure | [link](https://eudract.ema.europa.eu/result.html), [link](https://eudract.ema.europa.eu/docs/technical/EudraCT%20protocol%20related%20data%20dictionary.xls), [link](https://eudract.ema.europa.eu/docs/technical/V7_V8_Country_List_20210804.xlsx) | [link](https://cdn.clinicaltrials.gov/documents/xsd/public.xsd), [link](https://clinicaltrials.gov/data-about-studies/study-data-structure), [link](https://cdn.clinicaltrials.gov/documents/tutorial/content/index.html) | [link](https://www.isrctn.com/page/definitions) | [link](https://www.ema.europa.eu/en/human-regulatory-overview/research-development/clinical-trials-human-medicines/clinical-trials-information-system-ctis-training-support) (see XLSX files) |
| Example\* (see below) | [link](https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants) | [link](https://clinicaltrials.gov/search?ageRange=0M_1M&cond=Infections&aggFilters=studyType:int&distance=50&intr=Investigational+Agent) | [link](https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search) | [link](https://euclinicaltrials.eu/ctis-public/search#searchCriteria=%7B%22containAll%22:%22infection%22,%22containAny%22:%22neonates%22%7D) |

Some registers are expanding entered search terms using dictionaries
([example](https://clinicaltrials.gov/data-api/about-api/search-areas)).

## 4 - Example and ctrdata motivation

See
[`vignette("ctrdata_summarise")`](https://rfhb.github.io/ctrdata/articles/ctrdata_summarise.md)
for several other examples.

\*This example is an expert search for interventional trials primarily
with neonates, investigating treatments for infectious conditions. It
shows that searches in the web interface of most registers are not
sufficient to identify the trials of interest:

- EUCTR retrieves trials with neonates, but not only those exclusively
  in neonates.

- ISRCTN retrieves studies with interventions other than medicines.

- CTIS retrieves trials that mention the words neonates and infection.
  (To show CTIS search results, see
  [here](https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser))

To address this issue, trials can be retrieved with
[ctrLoadQueryIntoDb](https://rfhb.github.io/ctrdata/reference/ctrLoadQueryIntoDb.md)
into a database collection and in a second step trials of interest can
be selected based on values of relevant fields, for example:

- EUCTR field `f115_children_211years` and other age group criteria

- ISRCTN field `interventions.intervention.interventionType` for type of
  study

- CTIS fields `ageGroup` and
  `authorizedApplication.authorizedPartI.medicalConditions.medicalCondition`

`ctrdata` supports users with pre-defined
[ctrdata-trial-concepts](https://rfhb.github.io/ctrdata/reference/ctrdata-trial-concepts.md)
and these cover the example above, and with functions
[dbFindFields](https://rfhb.github.io/ctrdata/reference/dbFindFields.md)
and
[ctrShowOneTrial](https://rfhb.github.io/ctrdata/reference/ctrShowOneTrial.md)
for finding fields of interest and reviewing data structure,
respectively.

## Author

Ralf Herold <ralf.herold@mailbox.org>
