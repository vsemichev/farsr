library(dplyr)
library(maps)
library(readr)
library(tidyr)

#' Read \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Analysis Reporting System (FARS)} report file
#'
#' This function reads a FARS report file in CSV format and returns a data frame tibble
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename Name of the report file to read. An error will be thrown if the \code{filename} does not exist
#'
#' @return This function returns a data frame tibble containing the information from the report file
#'
#' @examples
#' \dontrun{
#' df <- fars_read("../data/accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}
#' Generate a FARS report file name based on the year provided as an argument
#'
#' This function will return a character vector containing a file name that follows the FARS naming standards
#'
#' @param year Year for which to generate the file name. It is expected to be an integer number. If the \code{year} argument
#'        cannot be converted to an integer a warning will be reported.
#'
#' @return A character vector containing the generated file name.
#'
#' @examples
#' \dontrun{
#' nm <- make_filename(2017)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' Read multiple report files for the list of years provided
#'
#' This function reads the data for multiple years provided as a list and returns a list of data frame tibbles
#' Each tibble will have two columns (variables)
#' \itemize{
#'    item MONTH
#'    item year
#' }
#' These columns conorrespond to month and year of each measurement
#'
#' @param years Vector or list containing years for which to read the data from the FARS reports
#'
#' @return List of tibbles. Each tibble contains information about \code{MONTH} and \code{year} for each processed measurement
#'
#' @note \code{fars_read_years} uses \code{\link{make_filename}} function to generate the list of files to process.
#'       \code{fars_read_years} expexts to find the report files in the current working directory
#'       If one of the report files does not exist \code{fars_read_years} will throw a warning.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{
#' mon_year <- fars_read_years(c(2013,2014,2015))
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select_(~MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}
#' Generate summary report showing number of observations per months for each year
#'
#' This function reads FARS report files for the list of years provided and produces a data frame tibble that summarizes
#' data for each month of each year. Information for each year is consolidated in a separate column. Each row represents
#' information for the corresponding month.
#'
#' @inheritParams fars_read_years
#'
#' @return data frame tibble in which each column represents a year and each row represent a month
#'         Values in this tibble represent number of observations for the corresponding month-year combination
#'
#' @note \code{fars_summarize_years} uses \code{fars_read_years} to read the reports.
#'       It expects to find the report files in the current working directory
#'
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' smry <- fars_summarize_years(list(2013,2014))
#' smry <- fars_summarize_years(c(2014,2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~year, ~MONTH) %>%
                dplyr::summarize_(n = ~n()) %>%
                tidyr::spread_(~year, ~n)
}
#' Draw a state map with dots showing location of each accident
#'
#' This function generates a picture of the state map with dots showing location of each accident
#' for the given year and state
#'
#' @param state.num Numeric state number. If \code{state.num} cannot be converted to an integer or there is no
#'        state corresponding to this number an error will be generated
#' @param year Numeric year for which to draw a map
#'
#' @return NULL
#'
#' @note This function has no return value. It is used only for its side effect of drawing the picture.
#'       If there are no recorded accidents for a given state-year combination a message will be reported indicating this.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(6,2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
