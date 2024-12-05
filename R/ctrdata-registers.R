#' ctrdata: information on clinical trial registers
#'
#' Registers of clinical trials from which protocol- and result-related information
#' can be retrieved and analysed with package \link{ctrdata}, last updated
#' 2024-12-06.
#'
#' @section 1 - Overview:
#'
#' - **EUCTR**: The EU Clinical Trials Register contains more than 44,200 clinical
#' trials (at least one investigational medicinal product, IMP; in the European
#' Union and beyond; no new trials, and results of trials continue to be added)
#' - **CTIS**: The EU Clinical Trials Information System started in January 2023
#' for new clinical trials. It includes more than 7,900 publicly accessible trials.
#' How to automatically get the CTIS search query URL: \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/\#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}
#' - **CTGOV2**: ClinicalTrials.gov includes more than 518,000 interventional and
#' observational studies
#' - **ISRCTN**: The ISRCTN Registry includes more than 25,700 interventional and
#' observational health studies
#'
#' @section 2 - Notable changes:
#'
#' CTGOV was retired on 2024-06-25; `ctrdata` subsequently translates CTGOV
#' queries to CTGOV2 queries. The new website (CTGOV2) can be used with `ctrdata`
#' since 2023-08-27. CTIS was relaunched on 2024-06-17, changing the data
#' structure and search syntax, to which `ctrdata` was updated. CTIS can be used
#' with `ctrdata` since 2023-03-25. More information on changes:
#' \href{https://rfhb.github.io/ctrdata/news/index.html}{here}.
#'
#' @section 3 - References:
#'
#' | **Material** | **EUCTR** | **CTGOV2**| **ISRCTN** | **CTIS** |
#' | -------------- | :--------------: | :--------------: | :--------------: | :--------------: |
#' | Home page | \href{https://www.clinicaltrialsregister.eu/}{link} | \href{https://clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} | \href{https://euclinicaltrials.eu/}{link} |
#' | About | \href{https://www.clinicaltrialsregister.eu/about.html}{link} | \href{https://clinicaltrials.gov/about-site/about-ctg}{link} | \href{https://www.isrctn.com/page/about}{link} | \href{https://euclinicaltrials.eu/about-this-website/}{link} |
#' | Terms & conditions, disclaimer | \href{https://www.clinicaltrialsregister.eu/disclaimer.html}{link} | \ifelse{latex}{\out{\href{https://clinicaltrials.gov/about-site/terms-conditions\#usage}{link}}}{\href{https://clinicaltrials.gov/about-site/terms-conditions#usage}{link}} | \ifelse{latex}{\out{\href{https://www.isrctn.com/page/faqs\#using-the-isrctn}{link}}}{\href{https://www.isrctn.com/page/faqs#using-the-isrctn}{link}} | \href{https://euclinicaltrials.eu/guidance-and-q-as/}{link} |
#' | How to search | \href{https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf}{link} | \href{https://clinicaltrials.gov/find-studies/how-to-search}{link} | \href{https://www.isrctn.com/page/search-tips}{link} | \href{https://euclinicaltrials.eu/search-tips-and-guidance/}{link} |
#' | Search interface | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} | \href{https://euclinicaltrials.eu/search-for-clinical-trials/}{link} |
#' | Expert / advanced search | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://clinicaltrials.gov/expert-search}{link} | \href{https://www.isrctn.com/editAdvancedSearch}{link} | \href{https://euclinicaltrials.eu/ctis-public/search}{link} |
#' | Glossary | \href{https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf}{link} | \href{https://clinicaltrials.gov/study-basics/glossary}{link} | \href{https://www.who.int/clinical-trials-registry-platform/network/who-data-set}{link} | |
#' | FAQ, caveats, issues | \href{https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf}{link} | \href{https://clinicaltrials.gov/about-site/selected-publications}{link} | \href{https://www.isrctn.com/page/faqs}{link} | \href{https://euclinicaltrials.eu/website-outages-and-system-releases/}{link} |
#' | Definitions of fields | \href{https://eudract.ema.europa.eu/result.html}{link} | \href{https://clinicaltrials.gov/data-about-studies/study-data-structure}{link}, \href{https://cdn.clinicaltrials.gov/documents/tutorial/content/index.html}{link} | \href{https://www.isrctn.com/page/definitions}{link} | \href{https://www.ema.europa.eu/en/human-regulatory-overview/research-development/clinical-trials-human-medicines/clinical-trials-information-system-ctis-training-support}{link} |
#' | Example* | \ifelse{latex}{\out{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections\&age=newborn\&age=preterm-new-born-infants}{link}}}{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants}{link}} | \ifelse{latex}{\out{\href{https://clinicaltrials.gov/search?ageRange=0M_1M\&cond=Infections\&aggFilters=studyType:int\&distance=50\&intr=Investigational+Agent}{link}}}{\href{https://clinicaltrials.gov/search?ageRange=0M_1M&cond=Infections&aggFilters=studyType:int&distance=50&intr=Investigational+Agent}{link}} | \ifelse{latex}{\out{\href{https://www.isrctn.com/search?q=\&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations\&searchType=advanced-search}{link}}}{\href{https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search}{link}} | \ifelse{latex}{\out{\href{https://euclinicaltrials.eu/ctis-public/search\#searchCriteria={"containAll":"infection","containAny":"neonates","containNot":""}}{link}}}{\href{https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"infection","containAny":"neonates"}}{link}} |
#'
#' @section 4 - Example and ctrdata motivation:
#'
#' *The example is an expert search for interventional trials primarily with neonates, investigating infectious conditions.
#' It shows that searches in registers may not be sufficient to identify the sought trials:
#'
#' - The CTGOV2 search retrieves trials conducted exclusively with neonates.
#' - EUCTR retrieves trials with neonates, but not only those exclusively in neonates.
#' - ISRCTN retrieves studies with interventions other than medicines.
#' - CTIS retrieves trials that mention the words neonates and infection. To
#' show search results, see \ifelse{latex}{\out{\href{https://rfhb.github.io/ctrdata/\#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://rfhb.github.io/ctrdata/#id_2-script-to-automatically-copy-users-query-from-web-browser}{here}}
#'
#' To address this, trials can be retrieved with \link{ctrLoadQueryIntoDb}
#' into a database collection and in a second step can be selected,
#' based on values of relevant fields of all retrieved trial information,
#' for example:
#'
#' - EUCTR field \code{f115_children_211years} for age criteria
#' - ISRCTN field \code{interventions.intervention.interventionType} for type of study
#' - CTIS fields \code{ageGroup} and \code{authorizedApplication.authorizedPartI.medicalConditions.medicalCondition}
#'
#' `ctrdata` helps identifying fields with function \link{dbGetFieldsIntoDf}.
#'
#' @name ctrdata-registers
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
