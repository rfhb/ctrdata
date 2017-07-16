### ctrdata package
### functions for euctr results

## 2017-04-02 started exploration
## 2017-07-15 first working functions

# for manual debugging
if(FALSE) {

  q <- "https://www.clinicaltrialsregister.eu/ctr-search/trial/2014-001076-58/results"

  eudract_number <- "2007-000371-42"
  eudract_number <- "2014-001076-58"

  tmp <- getEuctrResultsTrial(eudract_number = eudract_number)

  # css path: html body div#content.container_12 div.detail div#resultContent table#trialInformationSection.sectionTable
  # css selector: #trialInformationSection
  # embedded table:
  # #subjectDispositionSection > tbody:nth-child(1) > tr:nth-child(52) > td:nth-child(1) > div:nth-child(1) > table:nth-child(1)
  # div#resultContent table#subjectDispositionSection.sectionTable tbody tr td.embeddedTableContainer div table.embeddedTable

  i <- "#trialInformationSection"
  i <- "#endPointsSection"
  i <- "#endPoint999Section"
  i <- "#moreInformationSection"

}


# general definitions
results.tablenames <- c("#trialInformationSection",
                        "#subjectDispositionSection",
                        "#subjectDispositionSection .embeddedTable",
                        "#baselineCharacteristicsSection",
                        "#baselineCharacteristicsSection .embeddedTable",
                        "#endPointsSection",
                        "#endPoint-Section",
                        "#endPoint-Section .embeddedTable",
                        "#adverseEventsSection",
                        "#adverseEventsSection .embeddedTable",
                        "#moreInformationSection")



#' strnorm
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
strnorm <- function (s) {
  s <- tolower(s)
  s <- gsub(" +", "_", s)
  s <- gsub("[#.]", "",  s)
  s <- gsub("section", "", s)
  s <- gsub("embeddedtable", "details", s)
  return(s)
}



#' format_euctr_results
#'
#' @param df
#' @param tabletype
#'
#' @return
#' @export
#'
#' @examples
format_euctr_results <- function (df, tabletype) {

  # check if df is data.frame
  if(class(df) != "data.frame") stop("df: Not a data frame")

  # row deletion
  df <- df[df[, 1] != "", ]
  df <- df[!grepl("top of page", df[, 1], ignore.case = TRUE), ]

  # distinct handling
  if (tabletype == 0) {

    # embedded tables have two or more columns
    if(ncol(df) > 2) {

      # find where embedded table starts
      embedstart <- which(df[, 3] != "")

      # if any information found, there
      # seems a subtable, and in this case
      # retain only rows before embedded table
      if(length(embedstart) > 0) df <- df[seq_len(min(embedstart) - 1), ]

    }

    # main tables only have two columns
    df <- df [, 1:2]

  }

  # add information from rows used as headers into subsequent cells
  rowinfo <- ""
  rowinfo_was_used <- TRUE
  for (r in seq_len(nrow(df))) {

    if(df[r, 2] == "") {
      # save potential header info

      # # -1- with accumulation of rows
      # if(rowinfo_was_used) {
      #   # record new rowinfo
      #   rowinfo <- df[r, 1]
      #   rowinfo_was_used <- FALSE
      # } else {
      #   # accumulate rowinfo
      #   rowinfo <- paste0(rowinfo, " - ", df[r, 1])
      # }

      # -2- without accumulation of rows
      rowinfo <- df[r, 1]

    } else {
      # df[r, 2] != ""

      if(rowinfo != "") {
        df[r, 1] <- paste0(rowinfo, ": ", df[r, 1])
        rowinfo_was_used <- TRUE
      }
    }
  }

  # row deletion
  df <- df[df[, 2] != "", ]

  # add name of table
  tablename <- df[1, 1]
  attr(df, "tablename") <- tablename
  df <- df[ -1, ]

  # return cleaned and formatted data frame
  return (df)
}




#' ctrLoadQueryIntoDbEuctrResults
#'
#' Takes query and database parameters to find
#' all trials with results, for each of which
#' then the results are retrieved and loaded
#' into the data base
#'
#' @inheritParams ctrLoadQueryIntoDb
#'
#' @keywords internal
#'
ctrLoadQueryIntoDbEuctrResults <- function(queryterm, register = "EUCTR", querytoupdate,
                                           details, parallelretrievals, debug,
                                           collection, db, url,
                                           username, password, verbose,
                                           queryupdateterm) {

  # call for each trial getEuctrResultsTrial

  # load results into data base

}




#' getEuctrResultsTrial
#'
#' Obtain the results published for a single trial in the EUCTR.
#'
#' @param fullurleuctrtrialresults A full URL to the web pages with the results
#' of a specific trial in the EUCTR, example
#' https://www.clinicaltrialsregister.eu/ctr-search/trial/2007-000371-42/results
#'
#' @param eudract_number A single EudraCT number as string. If specified, takes
#' precedence over specifying fullurleuctrtrialresults.
#'
#' @return A list with named data frames representing the results.
#'
#' @importFrom xml2  read_html
#' @importFrom rvest html_nodes html_table
#'
getEuctrResultsTrial <- function(fullurleuctrtrialresults = "", eudract_number = ""){

  # check parameters
  if ((fullurleuctrtrialresults == "" & eudract_number == "") |
      (fullurleuctrtrialresults != "" & eudract_number != ""))
    stop("Specify either 'fullurleuctrtrialresults' or 'eudract_number'.")
  if (!grepl("[0-9]{4}-[0-9]{6}-[0-9]{2}", eudract_number))
    stop("Invalid 'eudract_number': ", eudract_number, ".")

  # from eudract_number construct fullurleuctrtrialresults
  fullurleuctrtrialresults <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/trial/",
                                     eudract_number, "/results")

  # download single web page with results for one trial
  webpage <- xml2::read_html(fullurleuctrtrialresults)

  # initialise variables needed for processing
  ii <- 0
  tables.results <- list()

  for (i in results.tablenames) {

    while(TRUE) {

      # construct table ids to be extracted
      if(grepl("-", i)){
        ii <- ii + 1
        isub <- sub("-", ii, i)
      } else {
        isub <- i
      }

      # extract from webpage
      webpage %>%
        html_nodes(isub) %>%
        html_table(fill = TRUE) ->
        table.list

      # for debugging:
      # df <- table.list[[1]]

      # if no data could be extracted,
      # leave while(TRUE) loop and iterate
      # to next i in results.tablenames
      if (length(table.list) == 0) {
        ii <- 0
        break
      }

      # convert each element in list into table
      tables.tmp <- lapply(table.list, format_euctr_results, tabletype = ii)

      # for debugging:
      # df <- tables.tmp[[1]]

      # iterate over tables
      for(iiii in tables.tmp) {

        # get table name
        tmpname <- paste(isub, attr(iiii, "tablename"))

        # normalise name
        tmpname <- strnorm(tmpname)

        # append named table as list element to output
        tables.results <- append(tables.results, setNames(list(iiii), tmpname))

      }

      # if there are no embedded tables,
      # leave while(TRUE) loop and iterate
      # to next i in results.tablenames
      if (ii == 0) {
        # inform user on successful last table
        message(isub)
        break
      }

      # inform user on successful last table
      message(isub)

    } # while(TRUE)

  } # for (i in results.tablenames)

  # compose return
  return(tables.results)

} # getEuctrResultsTrial




