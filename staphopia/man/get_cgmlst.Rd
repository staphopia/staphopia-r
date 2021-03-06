% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlst.R
\name{get_cgmlst}
\alias{get_cgmlst}
\title{get_cgmlst}
\usage{
get_cgmlst(sample_id)
}
\arguments{
\item{sample_id}{An integer sample ID}
}
\value{
Parsed JSON response.
}
\description{
Retrieve cgMLST results based on Mentalist for a given sample(s).
}
\examples{
get_cgmlst(500)
get_cgmlst(c(500,501))
}
