#' Information on clinical trial registers
#'
#' Registers of the four clinical trial registers from which package
#' \link{ctrdata} can retrieve, aggregate and analyse protocol- and
#' result-related information as well as documents, last updated 2026-10-04.
#'
#' @section 1 - Overview:
#'
#' - **EUCTR**: The EU Clinical Trials Register is complete with more than 44,400
#' clinical trials (at least one investigational medicinal product, IMP; in
#' the European Union and beyond), including more than 26,200 trials with
#' results, which continue to be added.
#'
#' - **CTIS**: The EU Clinical Trials Information System, launched in 2023,
#' holds more than 12,500 publicly accessible clinical trials, including
#' more than 1,550 with results or a report.
#' (To automatically get CTIS search query URLs, see
#' \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/\#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}})
#'
#' - **CTGOV2**: ClinicalTrials.gov holds more than 605,000 interventional and
#' observational studies, including more than 37,400 applicable clinical trials
#' (ACT, e.g. interventional studies with an Investigational Drug or Device, see
#' \href{https://clinicaltrials.gov/expert-search?term=AREA%5BStudyType%5D%20Interventional%20AND%20AREA%5BStartDate%5D%20RANGE%5B01%2F18%2F2017,%20MAX%5D%20AND%20(AREA%5BIsFDARegulatedDrug%5D%20Yes%20OR%20AREA%5BIsFDARegulatedDevice%5D%20Yes)%20AND%20NOT%20AREA%5BPhase%5D%20(%22Early%20Phase%201%22%20OR%20%22Phase%201%22)%20AND%20NOT%20AREA%5BDesignPrimaryPurpose%5D%20%22Device%20Feasibility%22%20AND%20(AREA%5BLocationCountry%5D%20(%22United%20States%22%20OR%20%22Puerto%20Rico%22%20OR%20%22Guam%22%20OR%20%22American%20Samoa%22%20OR%20%22Northern%20Mariana%20Islands%22%20OR%20%22U.S.%20Virgin%20Islands%22)%20OR%20AREA%5BIsUSExport%5D%20Yes)}{here}
#' ), of which more than 14,400 with results.
#'
#' - **ISRCTN**: The ISRCTN Registry holds more than 28,400 interventional and
#' observational health studies.
#'
#' | | **EUCTR** |  **CTIS** | **CTGOV2** | **ISRCTN** |
#' | -- | :--: | :--: | :--: | :--: |
#' | Protocol-related information | Structured | Structured | Structured | Structured |
#' | Result-related information | Structured,<br>documents | Documents | Structured,<br>publication links | Publication links |
#' | Documents | Reports | Protocol, info sheets,<br>SAP, results, other | Primarily<br>protocol, SAP | Protocol, info sheets,<br>SAP, results, other |
#'
#' @section 2 - Notable changes:
#'
#' - EUCTR removed search parameter `status=` as of February 2025 (see
#' transitioned trials in \link{dbFindIdsUniqueTrials}).
#' - CTIS can be used with `ctrdata` since 2023-03-25.
#' CTIS was relaunched on 2024-06-17, changing the data structure and search
#' syntax, to which `ctrdata` was updated.
#' - CTGOV "classic" was retired on 2024-06-25; `ctrdata` subsequently translates
#' CTGOV queries to CTGOV2 queries. The new website ("CTGOV2") can be used with
#' `ctrdata` since 2023-08-27. Database collections created with CTGOV queries
#' can still be used since functions in `ctrdata` continue to support them.
#'
#' More information on changes:
#' \href{https://rfhb.github.io/ctrdata/news/index.html}{here}.
#'
#' @section 3 - References:
#'
#' | **Material** | **EUCTR** | **CTIS** | **CTGOV2**| **ISRCTN** |
#' | -------------- | :--------------: | :--------------: | :--------------: | :--------------: |
#' | About | \href{https://www.clinicaltrialsregister.eu/about.html}{link} | \href{https://euclinicaltrials.eu/about-this-website/}{link} | \href{https://clinicaltrials.gov/about-site/about-ctg}{link} | \href{https://www.isrctn.com/page/about}{link} |
#' | Terms & conditions, disclaimer | \href{https://www.clinicaltrialsregister.eu/disclaimer.html}{link} | \href{https://euclinicaltrials.eu/guidance-and-q-as/}{link} |\href{https://clinicaltrials.gov/about-site/terms-conditions}{link} | \ifelse{latex}{\out{\href{https://www.isrctn.com/page/faqs\#using-the-isrctn}{link}}}{\href{https://www.isrctn.com/page/faqs#using-the-isrctn}{link}} |
#' | How to search | \href{https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf}{link} | \href{https://euclinicaltrials.eu/search-tips-and-guidance/}{link} |\href{https://clinicaltrials.gov/find-studies/how-to-search}{link} | \href{https://www.isrctn.com/page/search-tips}{link} |
#' | Search interface | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://euclinicaltrials.eu/search-for-clinical-trials/}{link} |\href{https://clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} |
#' | Expert / advanced search | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://euclinicaltrials.eu/ctis-public/search}{link} | \href{https://clinicaltrials.gov/expert-search}{link} | \href{https://www.isrctn.com/editAdvancedSearch}{link} |
#' | Glossary / related information | \href{https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf}{link} | \href{https://accelerating-clinical-trials.europa.eu/}{link} | \href{https://clinicaltrials.gov/study-basics/glossary}{link} | \href{https://www.who.int/clinical-trials-registry-platform/network/who-data-set}{link} |
#' | FAQ / caveats / examples | \href{https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf}{link} | \href{https://euclinicaltrials.eu/website-outages-and-system-releases/}{link} | \href{https://clinicaltrials.gov/policy/faq}{link}, \href{https://clinicaltrials.gov/about-site/selected-publications}{link}, \href{https://clinicaltrials.gov/submit-studies/prs-help/support-training-materials#example-studies}{link} | \href{https://www.isrctn.com/page/faqs}{link} |
#' | Data dictionaries / structure | \href{https://eudract.ema.europa.eu/result.html}{link}, \href{https://eudract.ema.europa.eu/docs/technical/EudraCT%20protocol%20related%20data%20dictionary.xls}{link}, \href{https://eudract.ema.europa.eu/docs/technical/V7_V8_Country_List_20210804.xlsx}{link} | \href{https://www.ema.europa.eu/en/human-regulatory-overview/research-development/clinical-trials-human-medicines/clinical-trials-information-system-ctis-training-support}{XLSX files} | \href{https://cdn.clinicaltrials.gov/documents/xsd/public.xsd}{link}, \href{https://clinicaltrials.gov/data-about-studies/study-data-structure}{link}, \href{https://cdn.clinicaltrials.gov/documents/tutorial/content/index.html}{link} | \href{https://www.isrctn.com/page/definitions}{link} |
#'
#' Some registers are expanding entered search terms using dictionaries
#' (\href{https://clinicaltrials.gov/data-api/about-api/search-areas}{example}).
#'
#' @section 4 - Example and ctrdata motivation:
#'
#' The example is an expert search for interventional clinical trials
#' primarily with neonates, investigating medicines for infectious conditions.
#' It shows that searches in the web interface of most registers are not
#' sufficient to identify and analyse the trials of interest:
#'
#' - EUCTR has a search box to retrieve trials with neonates, but not only
#' those conducted exclusively in neonates
#' (\ifelse{latex}{\out{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections\&age=newborn\&age=preterm-new-born-infants}{link}}}{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants}{link}}).
#' - CTIS retrieves trials based on their information mentioning the words neonates and infection
#' (\ifelse{latex}{\out{\href{https://euclinicaltrials.eu/ctis-public/search\#searchCriteria={"containAll":"infection","containAny":"neonates","containNot":""}}{link}}}{\href{https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"infection","containAny":"neonates"}}{link}};
#' to show CTIS search results, see
#' \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/\#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}).
#' - CTGOV2 can retrieve trials with neonates only if a specific age definition is used
#' (\ifelse{latex}{\out{\href{https://clinicaltrials.gov/search?ageRange=0M_1M\&cond=Infections\&aggFilters=studyType:int\&distance=50\&intr=Investigational+Agent}{link}}}{\href{https://clinicaltrials.gov/search?ageRange=0M_1M&cond=Infections&aggFilters=studyType:int&distance=50&intr=Investigational+Agent}{link}}).
#' - ISRCTN retrieves studies only with neonates, but not only of medicines
#' (\ifelse{latex}{\out{\href{https://www.isrctn.com/search?q=\&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations\&searchType=advanced-search}{link}}}{\href{https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search}{link}}).
#'
#' To address these issues, trials can first be retrieved using the links above
#' with \link{ctrLoadQueryIntoDb}. In a second step, all trial or a subset
#' can be analysed.
#'
#' `ctrdata` supports users with pre-defined \link{ctrdata-trial-concepts} that
#' can be calculated across registers when creating a dataframe for analysis,
#' such as \link{f.isMedIntervTrial} which identifies the subset of
#' investigational medicine trials.
#'
#' In addition, all fields made public by the registers can be searched with
#' \link{dbFindFields}; the trial's structure and the fields' values can be
#' reviewed with \link{ctrShowOneTrial}.
#'
#' For the example, the following fields can be included when creating a
#' dataframe using \link{dbGetFieldsIntoDf}, to select or analyse trials:
#'
#' - EUCTR: \code{f113_newborns_027_days}, \code{f1131_number_of_subjects_for_this_age_range}
#' - CTIS: \code{authorizedApplication.authorizedPartI.medicalConditions.medicalCondition}
#' - CTGOV2: \code{protocolSection.eligibilityModule.maximumAge}
#' - ISRCTN: \code{participants.upperAgeLimit}
#'
#' See vignette \href{../doc/ctrdata_summarise.html}{ctrdata_summarise} for
#' several other examples.
#'
#' @name ctrdata-registers
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
