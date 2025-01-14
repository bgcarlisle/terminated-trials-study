% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_reported_events_euctr.R
\name{adverse_events}
\alias{adverse_events}
\title{adverse_events}
\usage{
adverse_events(trn)
}
\arguments{
\item{trn}{A character vector containing a EUCTR trial registry
number with result section}
}
\value{
Adverse event table of the trial. Note will not fetch if
result section is an attachment or summary report link
}
\description{
Retrieves adverse events section for a EUCTR entry
with result section
}
\examples{

adverse_events("2008-006649-18")
}
