# ctrdata: Retrieve and Analyze Clinical Trials Data from Public Registers

A system for querying, retrieving and analyzing protocol- and
results-related information on clinical trials from four public
registers, the 'European Union Clinical Trials Register' ('EUCTR',
<https://www.clinicaltrialsregister.eu/>), 'ClinicalTrials.gov'
(<https://clinicaltrials.gov/> and also translating queries the retired
classic interface), the 'ISRCTN' (<http://www.isrctn.com/>) and the
'European Union Clinical Trials Information System' ('CTIS',
<https://euclinicaltrials.eu/>). Trial information is downloaded,
converted and stored in a database ('PostgreSQL', 'SQLite', 'DuckDB' or
'MongoDB'; via package 'nodbi'). Protocols, statistical analysis plans,
informed consent sheets and other documents in registers associated with
trials can also be downloaded. Other functions implement trial concepts
canonically across registers, identify deduplicated records, easily find
and extract variables (fields) of interest even from complex nested data
as used by the registers, merge variables and update queries. The
package can be used for monitoring, meta- and trend-analysis of the
design and conduct as well as of the results of clinical trials across
registers. See overview in Herold, R. (2025)
[doi:10.1017/rsm.2025.10061](https://doi.org/10.1017/rsm.2025.10061)

## See also

Useful links:

- <https://cran.r-project.org/package=ctrdata>

- <https://rfhb.github.io/ctrdata/>

- Report bugs at <https://github.com/rfhb/ctrdata/issues>

## Author

**Maintainer**: Ralf Herold <ralf.herold@mailbox.org>
([ORCID](https://orcid.org/0000-0002-8148-6748))

Other contributors:

- Marek Kubica (node-xml2js library) \[copyright holder\]

- Ivan Bozhanov (jstree library) \[copyright holder\]
