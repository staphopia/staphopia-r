% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{get_metadata}
\alias{get_metadata}
\title{get_metadata}
\usage{
get_metadata(sample_id)
}
\arguments{
\item{sample_id}{An integer sample ID, or vector of sample IDs}
}
\value{
Parsed JSON response.
}
\description{
Retrieve metadata associated with a sample.
}
\examples{
get_metadata(500)
get_metadata(c(500, 501, 1000))
}
