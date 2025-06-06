% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_likelyPlatformTrial.R
\name{f.likelyPlatformTrial}
\alias{f.likelyPlatformTrial}
\title{Calculate if study is likely a platform trial or not}
\usage{
f.likelyPlatformTrial(df = NULL)
}
\arguments{
\item{df}{data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
prints fields needed in `df` for calculating this trial concept, which can
be used with \link{dbGetFieldsIntoDf}.}
}
\value{
data frame with columns `_id` and `.likelyPlatformTrial`, a logical,
and two complementary columns, each with lists of identifiers:
`.likelyRelatedTrials` (based on other identifiers provided in the trial
record, including `associatedClinicalTrials` from CTIS; listing identifiers
whether or not the trial with the other identifier is in the database
collection) and `.maybeRelatedTrials` (based on similar short terms in the
first set of brackets or before a colon in the trial title; only listing
identifiers from the database collection).
}
\description{
Trial concept calculated: platform trial, research platform.
As operational definition, at least one of these criteria is true:
a. trial has "platform", "basket", "umbrella", "multi.?arm", "multi.?stage"
or "master protocol" in its title or description (for ISRCTN, this is the
only criterion; some trials in EUCTR lack data in English),
b. trial has more than 2 active arms with different investigational medicines,
after excluding comparator, auxiliary and placebo medicines (calculated with
\link{f.numTestArmsSubstances}; not used for ISRCTN because it cannot be
calculated precisely),
c. trial has more than 2 periods, after excluding safety run-in, screening,
enrolling, extension and follow-up periods (for CTGOV and CTGOV2, this
criterion requires results-related data).
}
\details{
For EUCTR, requires that results have been included in the collection, using
`ctrLoadQueryIntoDb(queryterm = ..., euctrresults = TRUE, con = ...)`.
Requires packages dplyr and stringdist to be installed; stringdist is used
for evaluating terms in brackets in the trial title, where trials may be
related if the term similarity is 0.77 or higher.

Publication references considered:
\href{https://web.archive.org/web/20230314024441/https://eu-pearl.eu/wp-content/uploads/2020/06/EU-PEARL_D2.1_Report-on-Terminology-and-Scenarios-for-Platform-Trials-and-Masterprotocols.pdf}{EU-PEARL WP2 2020}
and Williams RJ et al. 2022, \doi{10.1136/bmj-2021-067745}
}
\examples{
# fields needed
f.likelyPlatformTrial()

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.likelyPlatformTrial",
  con = dbc)
trialsDf

}
