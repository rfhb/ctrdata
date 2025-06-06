% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_statusRecruitment.R
\name{f.statusRecruitment}
\alias{f.statusRecruitment}
\title{Calculate status of recruitment of a study}
\usage{
f.statusRecruitment(df = NULL)
}
\arguments{
\item{df}{data frame such as from \link{dbGetFieldsIntoDf}. If `NULL`,
prints fields needed in `df` for calculating this trial concept, which can
be used with \link{dbGetFieldsIntoDf}.}
}
\value{
data frame with columns `_id` and `.statusRecruitment`, which is
a factor with levels `ongoing` (includes active, not yet recruiting;
temporarily halted; suspended; authorised, not started and similar),
`completed` (includes ended; ongoing, recruitment ended),
`ended early` (includes prematurely ended, terminated early) and
`other` (includes revoked, withdrawn, planned, stopped).
}
\description{
Trial concept calculated: status of recruitment at the time of loading
the trial records. Maps the categories that are in fields which specify
the state of recruitment. Simplifies the status into three categories.
}
\examples{
# fields needed
f.statusRecruitment()

# apply trial concept when creating data frame
dbc <- nodbi::src_sqlite(
  dbname = system.file("extdata", "demo.sqlite", package = "ctrdata"),
  collection = "my_trials", flags = RSQLite::SQLITE_RO)
trialsDf <- dbGetFieldsIntoDf(
  calculate = "f.statusRecruitment",
  con = dbc)
trialsDf

}
