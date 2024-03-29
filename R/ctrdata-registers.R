#' ctrdata: information on clinical trial registers
#'
#' Registers of clinical trials that can be accessed with
#' package \link{ctrdata} as of September 2023.
#'
#' - **EUCTR**: The European Union Clinical Trials Register contains
#' more than 43,500 clinical trials (using one or more medicines as
#' investigational medicinal product, IMP; in the European Union and beyond)
#' - **CTGOV** and **CTGOV2**: ClinicalTrials.gov includes more than 465,000
#' interventional and observational studies (both the current and
#' the classic website are supported)
#' - **ISRCTN**: The ISRCTN Registry includes almost 24,000
#' interventional or observational health studies
#' - **CTIS**: The EU Clinical Trials Information System was
#' started in January 2023, including more than 500 publicly
#' accessible trials on 2024-02-10. How to automatically
#' get a query URL: \ifelse{latex}{\out{\href{https://github.com/rfhb/ctrdata\#3-script-to-automatically-copy-users-query-from-web-browser}{here}}}{\href{https://github.com/rfhb/ctrdata#3-script-to-automatically-copy-users-query-from-web-browser}{here}}
#'
#'
#' | **Material** | **EUCTR** | **CTGOV** | **CTGOV2**| **ISRCTN** | **CTIS** |
#' | -------------- | :--------------: | :--------------: | :--------------: | :--------------: | :--------------: |
#' | Home page | \href{https://www.clinicaltrialsregister.eu/}{link} | \href{https://classic.clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} | \href{https://euclinicaltrials.eu/}{link} |
#' | About | \href{https://www.clinicaltrialsregister.eu/about.html}{link} | \href{https://classic.clinicaltrials.gov/ct2/about-site/background}{link} | \href{https://www.clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/page/about}{link} | \href{https://euclinicaltrials.eu/about-this-website/}{link} |
#' | Terms and conditions, disclaimer | \href{https://www.clinicaltrialsregister.eu/disclaimer.html}{link} | \ifelse{latex}{\out{\href{https://https://classic.clinicaltrials.gov/ct2/results/details?term=clinicaltrials.gov/ct2/about-site/terms-conditions\#Use}{link}}}{\href{https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use}{link}} | \href{https://www.clinicaltrials.gov/about-site/terms-conditions}{link} | \ifelse{latex}{\out{\href{https://www.isrctn.com/page/faqs\#usingISRCTN}{link}}}{\href{https://www.isrctn.com/page/faqs#usingISRCTN}{link}} | \href{https://euclinicaltrials.eu/guidance-and-q-as/}{link} |
#' | How to search | \href{https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf}{link} | \href{https://classic.clinicaltrials.gov/ct2/help/how-find/advanced}{link} | \href{https://www.clinicaltrials.gov/find-studies/how-to-search}{link} | \href{https://www.isrctn.com/page/search-tips}{link} | \href{https://euclinicaltrials.eu/search-tips-and-guidance/}{link} |
#' | Search interface | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://classic.clinicaltrials.gov/ct2/search/advanced}{link} | \href{https://www.clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} | \href{https://euclinicaltrials.eu/search-for-clinical-trials/}{link} |
#' | Glossary | \href{https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf}{link} | \href{https://classic.clinicaltrials.gov/ct2/about-studies/glossary}{link} | \href{https://www.clinicaltrials.gov/study-basics/glossary}{link} | \href{https://www.who.int/clinical-trials-registry-platform/network/who-data-set}{link} | |
#' | FAQ | \href{https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf}{link} | \href{https://www.clinicaltrials.gov/about-site/selected-publications}{link} | \href{https://www.clinicaltrials.gov/about-site/about-ctg}{link} | \href{https://www.isrctn.com/page/faqs}{link} | |
#' | Expert / advanced search | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://classic.clinicaltrials.gov/ct2/results/refine?show_xprt=Y}{link} | | \href{https://www.isrctn.com/editAdvancedSearch}{link} | \ifelse{latex}{\out{\href{https://euclinicaltrials.eu/app/\#/search}{link}}}{\href{https://euclinicaltrials.eu/app/#/search}{link}} |
#' | Example* | \ifelse{latex}{\out{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections\&age=newborn\&age=preterm-new-born-infants}{link}}}{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants}{link}} | \ifelse{latex}{\out{\href{https://classic.clinicaltrials.gov/ct2/results/refine?term=AREA[MaximumAge]+RANGE[0+days,1+months]\&type=Intr\&cond=Infections\&intr=Investigational+Agent\&show_xprt=Y}{link}}}{\href{https://classic.clinicaltrials.gov/ct2/results/refine?term=AREA%5BMaximumAge%5D+RANGE%5B0+days,1+months%5D&type=Intr&cond=Infections&intr=Investigational+Agent&show_xprt=Y}{link}} | \ifelse{latex}{\out{\href{https://www.clinicaltrials.gov/search?ageRange=0M_1M\&cond=Infections\&aggFilters=studyType:int\&distance=50\&intr=Investigational+Agent}{link}}}{\href{https://www.clinicaltrials.gov/search?ageRange=0M_1M&cond=Infections&aggFilters=studyType:int&distance=50&intr=Investigational+Agent}{link}} | \ifelse{latex}{\out{\href{https://www.isrctn.com/search?q=\&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations\&searchType=advanced-search}{link}}}{\href{https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search}{link}} | \ifelse{latex}{\out{\href{https://euclinicaltrials.eu/app/\#/search?basicSearchInputAND=infections\&ageGroupCode=2}{link}}}{\href{https://euclinicaltrials.eu/app/#/search?basicSearchInputAND=infections&ageGroupCode=2}{link}} |
#' | Definitions | \href{https://eudract.ema.europa.eu/result.html}{link} | \href{https://prsinfo.clinicaltrials.gov/prs-xml-schemas.html}{link} | \href{https://www.clinicaltrials.gov/data-about-studies/study-data-structure}{link} | \href{https://www.isrctn.com/page/definitions}{link} | \href{https://www.ema.europa.eu/en/human-regulatory-overview/research-and-development/clinical-trials-human-medicines/clinical-trials-information-system-training-support}{link} |
#'
#' *The example is an expert search that retrieves interventional
#' trials with neonates, investigating infectious conditions:
#' The CTGOV expert search retrieves trials conducted exclusively with neonates.
#' It is not yet clear why different sets of trials are shown in CTGOV (65) and
#' CTGOV2 (14) for the same query parameters.
#' EUCTR retrieves trials with neonates, but not only those exclusively in neonates.
#' ISRCTN retrieves studies with interventions other than medicines.
#' Thus, after loading trials with \link{ctrLoadQueryIntoDb}
#' into a database collection, trials of interest have to be selected
#' in a second step, based on values of fields of interest
#' (e.g., using \code{f115_children_211years} from EUCTR for age criteria,
#' \code{interventions.intervention.interventionType} from ISRCTN for type of study),
#' which can be obtained from the collection using \link{dbGetFieldsIntoDf}.
#'
#' @name ctrdata-registers
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
