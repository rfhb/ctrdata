#' ctrdata: detailed information on clinical trial registers
#'
#' Registers of clinical trials that can be accessed with
#' package \link{ctrdata-package} as of end 2021
#'
#' - **EUCTR**: The European Union Clinical Trials Register contains more
#' than 40,000 clinical trials (using one or more medicines as
#' investigational medicinal product, IMP; in Europe and beyond)
#' - **CTGOV**: ClinicalTrials.gov includes almost 400,000
#' interventional and observational studies
#' - **ISRCTN**: The ISRCTN Registry includes more than 21,000
#' interventional or observational health studies
#'
#' | **Material** | **EUCTR** | **CTGOV** | **ISRCTN** |
#' | -------------- | :-------: | :------: | :------: |
#' | Home page | \href{https://www.clinicaltrialsregister.eu/}{link} | \href{https://clinicaltrials.gov/}{link} | \href{https://www.isrctn.com/}{link} |
#' | About | \href{https://www.clinicaltrialsregister.eu/about.html}{link} | \href{https://clinicaltrials.gov/ct2/about-site/background}{link} | \href{https://www.isrctn.com/page/about}{link} |
#' | Terms and conditions, disclaimer | \href{https://www.clinicaltrialsregister.eu/disclaimer.html}{link} | \ifelse{latex}{\out{\href{https://clinicaltrials.gov/ct2/about-site/terms-conditions\#Use}{link}}}{\href{https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use}{link}} | \ifelse{latex}{\out{\href{https://www.isrctn.com/page/faqs\#usingISRCTN}{link}}}{\href{https://www.isrctn.com/page/faqs#usingISRCTN}{link}} |
#' | How to search | \href{https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf}{link} | \href{https://clinicaltrials.gov/ct2/help/how-find/advanced}{link} | \href{https://www.isrctn.com/page/search-tips}{link} |
#' | Search interface | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://clinicaltrials.gov/ct2/search/advanced}{link} | \href{https://www.isrctn.com/}{link} |
#' | Glossary | \href{https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf}{link} | \href{https://clinicaltrials.gov/ct2/about-studies/glossary}{link} | \href{https://www.who.int/clinical-trials-registry-platform/network/who-data-set}{link} |
#' | FAQ | \href{https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf}{link} | \href{https://clinicaltrials.gov/ct2/resources/pubs}{link} | \href{https://www.isrctn.com/page/faqs}{link} |
#' | Expert / advanced search | \href{https://www.clinicaltrialsregister.eu/ctr-search/search}{link} | \href{https://clinicaltrials.gov/ct2/results/refine?show_xprt=Y}{link} | \href{https://www.isrctn.com/editAdvancedSearch}{link} |
#' | Example* | \ifelse{latex}{\out{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections\&age=newborn\&age=preterm-new-born-infants}{link}}}{\href{https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants}{link}} | \ifelse{latex}{\out{\href{https://clinicaltrials.gov/ct2/results/refine?term=AREA[MaximumAge]+RANGE[0+days,1+months]\&type=Intr\&cond=Infections\&intr=Investigational+Agent\&show_xprt=Y}{link}}}{\href{https://clinicaltrials.gov/ct2/results/refine?term=AREA%5BMaximumAge%5D+RANGE%5B0+days,1+months%5D&type=Intr&cond=Infections&intr=Investigational+Agent&show_xprt=Y}{link}} | \ifelse{latex}{\out{\href{https://www.isrctn.com/search?q=\&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations\&searchType=advanced-search}{link}}}{\href{https://www.isrctn.com/search?q=&filters=ageRange:Neonate,conditionCategory:Infections+and+Infestations&searchType=advanced-search}{link}} |
#' | Definitions | \href{https://eudract.ema.europa.eu/result.html}{link} | \href{https://prsinfo.clinicaltrials.gov/definitions.html}{Protocol}, \href{https://prsinfo.clinicaltrials.gov/results_definitions.html}{results}, \href{https://clinicaltrials.gov/api/gui/ref/crosswalks}{names}, \href{https://clinicaltrials.gov/api/gui/ref/syntax}{syntax} | \href{https://www.isrctn.com/page/definitions}{link} |
#'
#' *The example expert search retrieves interventional trials with neonates,
#' investigating infectious conditions:
#' EUCTR retrieves trials with neonates, but not exclusively.
#' The CTGOV expert search retrieves trials exclusively in neonates.
#' ISRCTN retrieves a small number of studies.
#' Thus, after loading trials with \link{ctrLoadQueryIntoDb}
#' into a database collection, corresponding sets of trials need to be defined,
#' based on values of fields of interest
#' (e.g., \code{eligibility.maximum_age} from CTGOV and
#' \code{f115_children_211years} from EUCTR),
#' which can be obtained from the collection using \link{dbGetFieldsIntoDf}.
#'
#' @name ctrdata-registers
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
#'
NULL
#> NULL
