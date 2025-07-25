% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctrLoadQueryIntoDb.R
\name{ctrLoadQueryIntoDb}
\alias{ctrLoadQueryIntoDb}
\title{Load and store register trial information}
\usage{
ctrLoadQueryIntoDb(
  queryterm = NULL,
  register = "",
  querytoupdate = NULL,
  forcetoupdate = FALSE,
  euctrresults = FALSE,
  euctrresultshistory = FALSE,
  euctrprotocolsall = TRUE,
  ctgov2history = FALSE,
  ctishistory = FALSE,
  documents.path = NULL,
  documents.regexp = "prot|sample|statist|sap_|p1ar|p2ars|icf|ctalett|lay|^[0-9]+ ",
  annotation.text = "",
  annotation.mode = "append",
  only.count = FALSE,
  con = NULL,
  verbose = FALSE
)
}
\arguments{
\item{queryterm}{Either a single string with the full URL of a search
query in a register, e.g. one of the strings returned by
\link{ctrGenerateQueries}, or a single-row data frame returned by
\link{ctrGetQueryUrl} or \link{dbQueryHistory},
or an `_id` in the format of one of the trial registers (e.g., "NCT..."),
or, together with \code{register}, a string with query
elements of a search URL. The query details are recorded in the
\code{collection} for later use, e.g. to update records.
For "CTIS", \code{queryterm} can be an empty string to obtain
all trial records. For automatically copying the user's
query of a register in a web browser to the clipboard, see the
information on a browser extension script in \link{ctrGetQueryUrl}.}

\item{register}{String with abbreviation of register to query,
either "EUCTR", "CTGOV2", "ISRCTN" or "CTIS". Not needed
if \code{queryterm} has a full URL to query results,
or has a single trial identifier, or comes from
\link{ctrGetQueryUrl} or \link{dbQueryHistory}.}

\item{querytoupdate}{Either the word "last", or the row number of
a query in the data frame returned by \link{dbQueryHistory} that
should be run to retrieve any new or update trial records since
this query was run the last time.
This parameter takes precedence over \code{queryterm}.
For "EUCTR" and "CTIS", updates are available only for the last seven days;
the query is run again if more time has passed since it was run last.}

\item{forcetoupdate}{If \code{TRUE}, run again the query
given in \code{querytoupdate}, irrespective of when it was
run last. Default is \code{FALSE}.}

\item{euctrresults}{If \code{TRUE}, also load available
results when retrieving and loading trials from EUCTR. This
slows down this function. (For "CTGOV2" and "CTIS",
available results are always retrieved and loaded into the
collection.)}

\item{euctrresultshistory}{If \code{TRUE}, load results and also
the available history of results publication in "EUCTR."
This somewhat time-consuming. Default is \code{FALSE}.}

\item{euctrprotocolsall}{If \code{TRUE}, load all available records of
protocol-related data (that is, versions from all EU Member States and any
third country where the trial is conducted); if \code{FALSE}, only a single
record per trial is loaded, to accelerate loading. Default is \code{TRUE},
but only for backwards consistency; for new collections, \code{FALSE} is
the recommended setting, unless there are questions about differences
between Member States' protocol versions of a trial such as dates or outcomes
of an authorisation decision or an ethics opinion, global status and end.}

\item{ctgov2history}{For trials from CTGOV2, retrieve historic
versions of the record. Default is \code{FALSE}, because this
is a time-consuming operation. Use
\code{n} for n from all versions (recommended),
\code{1} for the first (original) version,
\code{-1} for the last-but-one version,
\code{"n:m"} for the nth to the mth versions, or
\code{TRUE} for all versions
of the trial record to be retrieved. Note that for register
CTIS, historic versions were available in the `applications`
field only before the register's relaunch on 2024-06-17.}

\item{ctishistory}{If \code{TRUE}, and only when using \code{querytoupdate},
move the current CTIS record into an array \code{history} with the record
which holds one or more historic versions, before updating the rest of the
record from CTIS. Default is \code{FALSE}, because this is a time-consuming
operation. See "Historic versions" in \code{vignette("ctrdata_summarise")}.}

\item{documents.path}{If this is a relative or absolute
path to a directory that exists or can be created,
save any documents into it that are directly available from
the register ("EUCTR", "CTGOV2", "ISRCTN", "CTIS")
such as PDFs on results, analysis plans, spreadsheets,
patient information sheets, assessments or product information.
Default is \code{NULL}, which disables saving documents.
For "EUCTR", sets \code{euctrresults = TRUE} since documents
are available only with results.}

\item{documents.regexp}{Regular expression, case insensitive,
to select documents by filename, if saving documents is requested
(see \code{documents.path}).
If set to \code{NULL}, empty placeholder files are saved for
every document that could be saved, which is useful to get an overview
on the number and types of documents available for download.
Default is
\code{"prot|sample|statist|sap_|p1ar|p2ars|icf|ctalett|lay|^[0-9]+ "}.
Used with "CTGOV2", "ISRCTN" and "CTIS" (for "EUCTR", all documents
are downloaded since they are few and have non-canonical filenames.)}

\item{annotation.text}{Text to be including into the field
"annotation" in the records retrieved with the query
that is to be loaded into the collection.
The contents of the field "annotation" for a trial record
are preserved e.g. when running this function again and
loading a record of a with an annotation, see parameter
\code{annotation.mode}.}

\item{annotation.mode}{One of "append" (default), "prepend"
or "replace" for new annotation.text with respect to any
existing annotation for the records retrieved with the query
that is to be loaded into the collection.}

\item{only.count}{Set to \code{TRUE} to return only the
number of trial records found in the register for the query.
Does not load trial information into the database.
Default is \code{FALSE}.}

\item{con}{A database connection object, created with
\code{nodbi}. See section `1 - Database connection` in
\link{ctrdata}.}

\item{verbose}{If \code{TRUE}, prints additional information
(default \code{FALSE}).}
}
\value{
A list with elements
`n` (number of trial records newly imported or updated),
`success` (a vector of _id's of successfully loaded records),
`failed` (a vector of identifiers of records that failed to load)
and `queryterm` (the query term used).
The returned list has several attributes (including database and
collection name, as well as the query history of this database
collection) to facilitate documentation.
}
\description{
Retrieves information on clinical trials from registers
and stores it in a collection in a database. Main function
of \link{ctrdata} for accessing registers.
A collection can store trial information from different queries
or different registers. Query details are stored in the
collection and can be accessed using \link{dbQueryHistory}.
A previous query can be re-run, which replaces or adds trial
records while keeping any user annotations of trial records.
}
\examples{
\dontrun{

# Count ongoing interventional cancer trials involving children
# Note this query is a classical CTGOV query and is translated
# to a corresponding query for the current CTGOV2 webinterface
ctrLoadQueryIntoDb(
  queryterm = "cond=cancer&recr=Open&type=Intr&age=0",
  register = "CTGOV",
  only.count = TRUE
)

dbc <- nodbi::src_sqlite(collection = "my_collection")

# Retrieve protocol- and results-related information
# on two specific trials identified by their EU number,
# including protocol versions from all Member States
ctrLoadQueryIntoDb(
  queryterm = "2005-001267-63+OR+2008-003606-33",
  register = "EUCTR",
  euctrresults = TRUE,
  euctrprotocolsall = TRUE,
  con = dbc
)

# Retrieve all information on more than 40 trials
# that are labelled as phase 3 and that mention
# either neuroblastoma or lymphoma from ISRCTN,
# into the same collection as used before
ctrLoadQueryIntoDb(
  queryterm = paste0(
    "https://www.isrctn.com/search?",
    "q=neuroblastoma+OR+lymphoma&filters=phase\%3APhase+III"),
  con = dbc
)

# Retrieve information trials in CTIS mentioning neonates
ctrLoadQueryIntoDb(
  queryterm = paste0("https://euclinicaltrials.eu/ctis-public/",
  "search#searchCriteria={\%22containAll\%22:\%22\%22,",
  "\%22containAny\%22:\%22neonates\%22,\%22containNot\%22:\%22\%22}"),
  con = dbc
)
}

}
