#' ctrdata: detailed information on clinical trial registers
#'
#' Registers of clinical trials that can be accessed with
#' package \link{ctrdata-package} as of end 2021
#'
#' - **EUCTR**: The European Union Clinical Trials Register contains more
#' than 40,000 clinical trials (using one or more medicines as
#' investigational medicinal product, IMPD; in Europe and beyond)
#' - **CTGOV**: ClinicalTrials.gov includes almost 400,000
#' interventional and observational studies
#' - **ISCRTN**: The ISRCTN Registry includes more than 21,000
#' interventional or observational health studies
#'
#'
#' | **Material** | **EUCTR** | **CTGOV** | **ISRCTN** |
#' | -------------- | :-------: | :------: | :------: |
#' | Home page | [link](https://www.clinicaltrialsregister.eu/) | [link](https://clinicaltrials.gov/) | [link](https://www.isrctn.com/) |
#' | About | [link](https://www.clinicaltrialsregister.eu/about.html) | [link](https://clinicaltrials.gov/ct2/about-site/background) | [link](https://www.isrctn.com/page/about) |
#' | How to search | [link](https://www.clinicaltrialsregister.eu/doc/How_to_Search_EU_CTR.pdf) | [link](https://clinicaltrials.gov/ct2/help/how-find/advanced) | [link](https://www.isrctn.com/page/search-tips) |
#' | Search | [link](https://www.clinicaltrialsregister.eu/ctr-search/search) | [link](https://clinicaltrials.gov/ct2/search/advanced) | [link]
#' | Glossary | [link](https://www.clinicaltrialsregister.eu/doc/EU_Clinical_Trials_Register_Glossary.pdf)  | [link](https://clinicaltrials.gov/ct2/about-studies/glossary) | [link](https://www.who.int/clinical-trials-registry-platform/network/who-data-set) |
#' | FAQ | [link](https://www.clinicaltrialsregister.eu/doc/EU_CTR_FAQ.pdf) | [link](https://clinicaltrials.gov/ct2/resources/pubs) | [link](https://www.isrctn.com/page/faqs) |
#' | Expert / advanced search | [link](https://www.clinicaltrialsregister.eu/ctr-search/search) | [link](https://clinicaltrials.gov/ct2/results/refine?show_xprt=Y) | [link](https://www.isrctn.com/editAdvancedSearch) |
#' | Example* | [link](https://www.clinicaltrialsregister.eu/ctr-search/search?query=Infections&age=newborn&age=preterm-new-born-infants) | [link](https://clinicaltrials.gov/ct2/results/refine?term=AREA%5BMaximumAge%5D+RANGE%5B0+days%2C+1+months%5D&type=Intr&cond=Infections&intr=Investigational+Agent&show_xprt=Y) | [link](https://www.isrctn.com/search?q=&filters=ageRange%3ANeonate%2CconditionCategory%3AInfections+and+Infestations&searchType=advanced-search) |
#' | Definitions | [link](https://eudract.ema.europa.eu/result.html) | [link](https://clinicaltrials.gov/api/gui/ref/syntax), [link](https://clinicaltrials.gov/api/info/study_fields_list) | [link](https://www.isrctn.com/page/definitions) |
#' | Terms & conditions, disclaimer | [link](https://www.clinicaltrialsregister.eu/disclaimer.html) | [link](https://clinicaltrials.gov/ct2/about-site/terms-conditions#Use) | [link](https://www.isrctn.com/page/faqs#usingISRCTN) |
#'
#' *The example intends to retrieve interventional trials with neonates, investigating
#' infectious conditions: EUCTR retrieves trials with neonates but not exclusively.
#' The CTGOV expert search retrieves trials exclusively in neonates. ISRCTN retrieves
#' a small number of studies. Thus, after loading trials with \link{ctrLoadQueryIntoDb}
#' into a database collection, corresponding sets of trials need to be selected
#' using fields of interest obtained from the collection using \link{dbGetFieldsIntoDf}.
#'
#' @name ctrdata-registers
#' @docType data
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords data
#' @md
NULL
#> NULL
