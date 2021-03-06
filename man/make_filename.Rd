% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Generate a FARS report file name based on the year provided as an argument}
\usage{
make_filename(year)
}
\arguments{
\item{year}{Year for which to generate the file name. It is expected to be an integer number. If the \code{year} argument
cannot be converted to an integer a warning will be reported.}
}
\value{
A character vector containing the generated file name.
}
\description{
This function will return a character vector containing a file name that follows the FARS naming standards
}
\examples{
\dontrun{
nm <- make_filename(2017)
}

}
