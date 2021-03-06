% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Generate summary report showing number of observations per months for each year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{Vector or list containing years for which to read the data from the FARS reports}
}
\value{
data frame tibble in which each column represents a year and each row represent a month
        Values in this tibble represent number of observations for the corresponding month-year combination
}
\description{
This function reads FARS report files for the list of years provided and produces a data frame tibble that summarizes
data for each month of each year. Information for each year is consolidated in a separate column. Each row represents
information for the corresponding month.
}
\note{
\code{fars_summarize_years} uses \code{fars_read_years} to read the reports.
      It expects to find the report files in the current working directory
}
\examples{
\dontrun{
smry <- fars_summarize_years(list(2013,2014))
smry <- fars_summarize_years(c(2014,2015))
}

}
