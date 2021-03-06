% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/PoliticalDatasets.R
\name{merge_by_date_interval}
\alias{merge_by_date_interval}
\title{Merging datsets by date intervals}
\usage{
merge_by_date_interval(data1, data2, key_col = "country_name", startdate1,
  enddate1, startdate2, enddate2, group_vars = NULL)
}
\arguments{
\item{data1}{}

\item{data2}{}

\item{key_col}{}

\item{startdate1}{}

\item{enddate1}{}

\item{startdate2}{}

\item{enddate2}{}

\item{group_vars}{}
}
\value{
A dataset merging \code{data1} and \code{data2} by the \code{key_col}
  with each row of \code{data1} split according to which part of it falls
  within the intervals in \code{data2}
}
\description{
Merging datsets by date intervals
}
\examples{
library(dplyr)
data1 <- archigos \%>\% select(country_name,obsid,leader,startdate,enddate) \%>\% filter(country_name == "Cuba")
data2 <- polity_cases \%>\% select(country_name, polity, polity_startdate, polity_enddate) \%>\% filter(country_name == "Cuba")
results <- merge_by_date_interval(data1, data2, "country_name", "startdate", "enddate", "polity_startdate", "polity_enddate", "obsid")
results
}

