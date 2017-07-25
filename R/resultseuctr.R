### ctrdata package
### functions for euctr results

## 2017-04-02 started exploration
## 2017-07-15 first working functions


# for manual debugging
if(FALSE) {

  q <- "https://www.clinicaltrialsregister.eu/ctr-search/trial/2014-001076-58/results"

  eudract_number <- "2007-000371-42"
  eudract_number <- "2014-001076-58"

  collection <- "resultsTest"

  ctrLoadQueryIntoDb(eudract_number, register = "EUCTR", collection = collection)

  tmp <- getEuctrResultsTrial(eudract_number = eudract_number)
  tmp <- jsonlite::toJSON(tmp, auto_unbox = TRUE, pretty = TRUE)

  cat(tmp, file = paste0("./private/results/", eudract_number, ".json"))

  # -1- to do:
  # "id_info.secondary_id": ["-", "-"],
  # "id_info.nct_id": "NCT00106353",

  # -2- do not make named sublists


  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection)[["ctr"]]
  # update database
  mongo$update(query  = paste0('{"x1_eudract_number":{"$eq":"', eudract_number, '"}}'),
               update = paste0('{ "$set" :', tmp, "}"),
               upsert = TRUE)

}










## general definitions

# names of tables in euctr html results page, see following examples:
# css path: div#content.container_12 div.detail div#resultContent table#trialInformationSection.sectionTable
# css selector: #trialInformationSection
# embedded: table#subjectDispositionSection.sectionTable tbody tr td.embeddedTableContainer div table.embeddedTable
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

# mapping of euctr and ctgov variable / key names
euctrctgovdict <- c(

  "Additional study identifiers: US NCT number", "id_info.nct_id",
  "Additional study identifiers: ISRCTN number", "id_info.secondary_id",
  "Additional study identifiers: WHO universal trial number (UTN)", "id_info.secondary_id",

  "results_analysis_stage_global_end_of_trial_date", "last_follow_up_date", # ctgov deprecated end_date

  "First version publication date", "firstreceived_results_date",
  "This version publication date", "lastchanged_date",

  "number_of_subjects_enrolled_per_country_worldwide_total_number_of_subjects", "enrollment",

  # no correspondence found in ctgov
  "number_of_subjects_enrolled_per_country_eea_total_number_of_subjects", NA,
  "number_of_subjects_enrolled_per_age_group_in_utero", NA,
  "number_of_subjects_enrolled_per_age_group_preterm_newborn_-_gestational_age_<_37_wk", NA,
  "number_of_subjects_enrolled_per_age_group_newborns_(0-27_days)", NA,
  "number_of_subjects_enrolled_per_age_group_infants_and_toddlers_(28_days-23_months)", NA,
  "number_of_subjects_enrolled_per_age_group_children_(2-11_years)", NA,
  "number_of_subjects_enrolled_per_age_group_adolescents_(12-17_years)", NA,
  "number_of_subjects_enrolled_per_age_group_adults_(18-64_years)", NA,
  "number_of_subjects_enrolled_per_age_group_from_65_to_84_years", NA,
  "number_of_subjects_enrolled_per_age_group_85_years_and_over", NA

)
# convert into data frame for looking up
euctrctgovdict.select <- seq.int(1L, length(euctrctgovdict), 2L)
euctrctgovdict <- data.frame("euctr" = euctrctgovdict[euctrctgovdict.select    ],
                             "ctgov" = euctrctgovdict[euctrctgovdict.select + 1],
                             stringsAsFactors = FALSE)




#' euctr2ctgov
#'
#' This function looks up the name of the result variable
#' as retrieved from EUCTR and attempts to find and return
#' the corresponding CTGOV variable name.
#'
#' @param x A single string
#'
#' @return A CTGOV variable name
#'
euctr2ctgov <- function (x) {

  # for debugging
  #message(x, " -> ", appendLF = FALSE)

  # check paramater
  if(class(x)  != "character") stop("x is not a string.")
  if(length(x) != 1)           stop("x has to be of length 1.")
  if(!is.atomic(x))            stop("x has to be single string.")

  # attempt look up in dictionary
  s <- euctrctgovdict$ctgov[euctrctgovdict$euctr == x]

  # for debugging
  #message(s, appendLF = TRUE)

  # fallback to normalised input string
  if(!length(s) || is.na(s)) s <- strnorm(x)

  # return converted string
  return (s)
}




#' strnorm
#'
#' This function normalises a single string
#' according to some rules developed from
#' processing EUCTR result field names
#'
#' @param s A string to be normalised
#'
#' @return Normalised string
#'
strnorm <- function (s) {
  s <- tolower(s)
  s <- gsub(" +", "_", s)
  s <- gsub("_$", "", s)
  s <- gsub("/", "_", s)
  s <- gsub(":", "", s) # TODO
  s <- gsub("[#.]", "",  s)
  s <- gsub("section", "", s)
  s <- gsub("embeddedtable", "details", s)
  return(s)
}



#' format_euctr_results
#'
#' Function cleans a table from EUCTR results,
#' in particular by removing extraneous cells
#' and interpreting the visual / positional
#' markup used by EUCTR
#'
#' @param df A data frame from a table node by rvest
#'
#' @param tabletype Indicator if simple table (0)
#' or if it includes a subtable (higher than 0)
#'
#' @return A clean data frame
#'
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

  # turn df into named list to
  # facilitate toJSON conversion
  #
  outdf <- lapply(df[ ,2], c)
  #
  # for name, search for ctgov from euctr key
  names(outdf) <- unlist(lapply(df[ ,1], euctr2ctgov))

  # there will be duplicate keys, hence it is necessary
  # to concatenate elements by key
  keys  <- unique(names(outdf))
  outdf <- lapply(keys, FUN = function (x) paste0(outdf[ names(outdf) == x ]))
  names(outdf) <- keys

  # return cleaned and formatted list
  return (outdf)

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
#' @importFrom stats setNames
#'
getEuctrResultsTrial <- function(fullurleuctrtrialresults = "", eudract_number = ""){

  # check parameters
  if ((fullurleuctrtrialresults == "" & eudract_number == "") |
      (fullurleuctrtrialresults != "" & eudract_number != ""))
    stop("Specify either 'fullurleuctrtrialresults' or 'eudract_number'.")
  if (!grepl("[0-9]{4}-[0-9]{6}-[0-9]{2}", eudract_number))
    stop("Invalid 'eudract_number': ", eudract_number, ".")

  # from eudract_number construct full url of euctr trial's results
  fullurleuctrtrialresults <- paste0("https://www.clinicaltrialsregister.eu/ctr-search/trial/",
                                     eudract_number, "/results")

  # download single web page with results for one trial
  webpage <- xml2::read_html(fullurleuctrtrialresults)

  # extract special information prepended to table

  # <td class="header labelColumn">
  #   <div>    Results information</div>
  #   </td>
  #   <tr>
  #   <td class="labelColumn"><div>    This version publication date</div></td>
  #   <td class="valueColumn"><div>
  #   01 Apr 2017
  # </div>
  #   </td>
  #   </tr>
  #   <tr>
  #   <td class="labelColumn"><div>    First version publication date</div></td>
  #   <td class="valueColumn"><div>
  #   01 Apr 2017
  # </div>
  tables.results <- list(
    sub( ".*This version publication date.*?([0-9]{2} [A-Z][a-z]{2} [0-9]{4}).*", "\\1", webpage),
    sub(".*First version publication date.*?([0-9]{2} [A-Z][a-z]{2} [0-9]{4}).*", "\\1", webpage)
    )
  names(tables.results) <- c(euctr2ctgov( "This version publication date"),
                             euctr2ctgov("First version publication date"))

  # initialise variables needed for further processing
  ii <- 0

  # iterate over pre-defined tables to extract tabled information
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
      table.list <- rvest::html_table(rvest::html_nodes(webpage, isub), fill = TRUE)

      # for debugging:
      # df <- table.list[[1]]

      # if no data could be extracted,
      # leave while(TRUE) loop and iterate
      # to next i in results.tablenames
      if (length(table.list) == 0) {
        ii <- 0
        break
      }

      # convert each element in list into formatted data frame
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
        tables.results <- append(tables.results, stats::setNames(list(iiii), tmpname))

      }

      # if there are no embedded tables,
      # leave while(TRUE) loop and iterate
      # to next i in results.tablenames
      if (ii == 0) {
        # inform user on successful last table
        #message(isub)
        break
      }

      # inform user on successful last table
      #message(isub)

    } # while(TRUE)

  } # for (i in results.tablenames)

  # compose return
  return(tables.results)

} # getEuctrResultsTrial






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
ctrLoadQueryIntoDbEuctrResults <- function(queryterm, register, querytoupdate,
                                           include.euctr.results,
                                           details, parallelretrievals, debug,
                                           collection, db, url,
                                           username, password, verbose,
                                           queryupdateterm,
                                           eudractnumbers) {

  ## adjust query

  # change query so that only


  ## get results

  # call for each trial getEuctrResultsTrial
  tmp <- getEuctrResultsTrial(eudract_number = eudract_number)

  # transform into json
  tmp <- jsonlite::toJSON(tmp, auto_unbox = TRUE) # , pretty = TRUE


  ## load results into data base

  # get a working mongo connection, select trial record collection
  mongo <- ctrMongo(collection = collection, db = db, url = url,
                    username = username, password = password, verbose = TRUE)[["ctr"]]

  # update database
  mongo$update(query  = paste0('{"x1_eudract_number":{"$eq":"', eudract_number, '"}}'),
               update = paste0('{ "$set" :', tmp, "}"),
               upsert = TRUE)




} # ctrLoadQueryIntoDbEuctrResults


