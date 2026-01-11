#' Information on clinical trial registers
#'
#' Registers of the four clinical trial registers from which package
#' \link{ctrdata} can retrieve, aggregate and analyse protocol- and
#' result-related information as well as documents, last updated 2026-01-11.
#'
#' @section 1 - Overview:
#'
#' - **EUCTR**: The EU Clinical Trials Register is complete with 44,366
#' clinical trials (at least one investigational medicinal product, IMP; in
#' the European Union and beyond), including more than 25,800 trials with
#' results, which continue to be added.
#'
#' - **CTIS**: The EU Clinical Trials Information System, launched in 2023,
#' holds more than 10,680 publicly accessible clinical trials, including
#' more than 520 with results or a report.
#' (To automatically get CTIS search query URLs, see
#' \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/\#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}})
#'
#' - **CTGOV2**: ClinicalTrials.gov holds more than 565,000 interventional and
#' observational studies, including more than 71,000 interventional studies
#' with results.
#'
#' - **ISRCTN**: The ISRCTN Registry holds more than 27,500 interventional and
#' observational health studies, including almost 15,000 studies with
#' results.
#'
#' | | **CTGOV2**| **CTIS** | **EUCTR** | **ISRCTN** |
#' | -- | :--: | :--: | :--: | :--: |
#' | Protocol-related information | Structured | Structured | Structured | Structured |
#' | Result-related information | Structured,<br>publication links | Documents | Structured,<br>documents | Publication links |
#' | Documents | Primarily<br>protocol, SAP | Protocol, info sheets,<br>SAP, results | Reports | Protocol, info sheets,<br>SAP, results |
#'
#' @section 2 - Notable changes:
#'
#' CTGOV "classic" was retired on 2024-06-25; `ctrdata` subsequently translates
#' CTGOV queries to CTGOV2 queries. The new website ("CTGOV2") can be used with
#' `ctrdata` since 2023-08-27. Database collections created with CTGOV queries
#' can still be used since functions in `ctrdata` continue to support them.
#' CTIS was relaunched on 2024-06-17, changing the data structure and search
#' syntax, to which `ctrdata` was updated.
#' CTIS can be used with `ctrdata` since 2023-03-25.
#' EUCTR removed search parameter `status=` as of February 2025.
#' More information on changes:
#' \href{https://rfhb.github.io/ctrdata/news/index.html}{here}.
#'
#' @section 3 - References:
#'
#' | **Material** | **EUCTR** | **CTGOV2**| **ISRCTN** | **CTIS** |
#' | -------------- | :--------------: | :--------------: | :--------------: | :--------------: |
#' | About | \href{https://www.clinicaltrialsregister.eu/about.html}{link} | \href{https://clinicaltrials.gov/about-site/about-ctg}{link} | \href{https://www.isrctn.com/page/about}{link} | \href{https://euclinicaltrials.eu/about-this-website/}{link} |
#' | Terms & conditions, disclaimer | \href{https://www.clinicaltrialsregister.eu/disclaimer.html}{link} | \href{https://clinicaltrials.gov/about-site/terms-conditions}{link} | \ifelse{latex}{\out{\href{https://www.isrctn.com/page/faqs\#using-the-isrctn}{link}}}{\href{https://www.isrctn.com/page/faqs#using-the-isrctn}{link}} | \href{https://euclinicaltrials.eu/guidance-and-q-as/}{link} |
#' | How to search | \href{https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf}{link} | \href{https://clinicaltrials.gov/find-studies/how-to-search}{link} | \href{https://www.isrctn.com/page/search-tips}{link} | \href{https://euclinicaltrials.eu/search-tips-and-guidance/}{link} |
#' | Search interface | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} | \href{https://euclinicaltrials.eu/search-for-clinical-trials/}{link} |
#' | Expert / advanced search | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://clinicaltrials.gov/expert-search}{link} | \href{https://www.isrctn.com/editAdvancedSearch}{link} | \href{https://euclinicaltrials.eu/ctis-public/search}{link} |
#' | Glossary / related information | \href{https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf}{link} | \href{https://clinicaltrials.gov/study-basics/glossary}{link} | \href{https://www.who.int/clinical-trials-registry-platform/network/who-data-set}{link} | \href{https://accelerating-clinical-trials.europa.eu/}{link} |
#' | FAQ / caveats / examples | \href{https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf}{link} | \href{https://clinicaltrials.gov/policy/faq}{link}, \href{https://clinicaltrials.gov/about-site/selected-publications}{link}, \href{https://clinicaltrials.gov/submit-studies/prs-help/support-training-materials#example-studies}{link} | \href{https://www.isrctn.com/page/faqs}{link} | \href{https://euclinicaltrials.eu/website-outages-and-system-releases/}{link} |
#' | Data dictionaries / structure | \href{https://eudract.ema.europa.eu/result.html}{link}, \href{https://eudract.ema.europa.eu/docs/technical/EudraCT%20protocol%20related%20data%20dictionary.xls}{link}, \href{https://eudract.ema.europa.eu/docs/technical/V7_V8_Country_List_20210804.xlsx}{link} | \href{https://cdn.clinicaltrials.gov/documents/xsd/public.xsd}{link}, \href{https://clinicaltrials.gov/data-about-studies/study-data-structure}{link}, \href{https://cdn.clinicaltrials.gov/documents/tutorial/content/index.html}{link} | \href{https://www.isrctn.com/page/definitions}{link} | \href{https://www.ema.europa.eu/en/human-regulatory-overview/research-development/clinical-trials-human-medicines/clinical-trials-information-system-ctis-training-support}{link} (see XLSX files) |
#' | Example* (see below) | \ifelse{latex}{\out{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections\&age=newborn\&age=preterm-new-born-infants}{link}}}{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants}{link}} | \ifelse{latex}{\out{\href{https://clinicaltrials.gov/search?ageRange=0M_1M\&cond=Infections\&aggFilters=studyType:int\&distance=50\&intr=Investigational+Agent}{link}}}{\href{https://clinicaltrials.gov/search?ageRange=0M_1M&cond=Infections&aggFilters=studyType:int&distance=50&intr=Investigational+Agent}{link}} | \ifelse{latex}{\out{\href{https://www.isrctn.com/search?q=\&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations\&searchType=advanced-search}{link}}}{\href{https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search}{link}} | \ifelse{latex}{\out{\href{https://euclinicaltrials.eu/ctis-public/search\#searchCriteria={"containAll":"infection","containAny":"neonates","containNot":""}}{link}}}{\href{https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"infection","containAny":"neonates"}}{link}} |
#'
#' Some registers are expanding entered search terms using dictionaries
#' (\href{https://clinicaltrials.gov/data-api/about-api/search-areas}{example}).
#'
#' @section 4 - Example and ctrdata motivation:
#'
#' See vignette \href{../doc/ctrdata_summarise.html}{ctrdata_summarise} for
#' several other examples.
#'
#' *This example is an expert search for interventional trials primarily with
#' neonates, investigating treatments for infectious conditions. It shows that
#' searches in the web interface of most registers are not sufficient to
#' identify the trials of interest:
#'
#' - EUCTR retrieves trials with neonates, but not only those exclusively in neonates.
#' - ISRCTN retrieves studies with interventions other than medicines.
#' - CTIS retrieves trials that mention the words neonates and infection. (To
#' show CTIS search results, see \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/\#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}})
#'
#' To address this issue, trials can be retrieved with \link{ctrLoadQueryIntoDb}
#' into a database collection and in a second step trials of interest can be
#' selected based on values of relevant fields, for example:
#'
#' - EUCTR field \code{f115_children_211years} and other age group criteria
#' - ISRCTN field \code{interventions.intervention.interventionType} for type of study
#' - CTIS fields \code{ageGroup} and \code{authorizedApplication.authorizedPartI.medicalConditions.medicalCondition}
#'
#' `ctrdata` supports users with pre-defined \link{ctrdata-trial-concepts} and
#' these cover the example above, and with functions \link{dbFindFields} and
#' \link{ctrShowOneTrial} for finding fields of interest and reviewing data
#' structure, respectively.
#'
#' @name ctrdata-registers
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
