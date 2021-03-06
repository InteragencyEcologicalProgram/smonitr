% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tools.R
\name{smr_x_axis}
\alias{smr_x_axis}
\title{Custom x-axis for IEP Seasonal Monitoring Report figures}
\usage{
smr_x_axis(
  report_year,
  type = c("all", "recent"),
  season = c("annual", "winter", "spring", "summer", "fall"),
  interval,
  start_year
)
}
\arguments{
\item{report_year}{The user-defined report year for the Seasonal
Monitoring Report. Must be an integer.}

\item{type}{The scale type to use. \code{type = "all"} defines the
minimum x-axis limit as 1966, which corresponds to the earliest
survey shown in the fall season report (Fall Midwater Trawl).
\code{type = "recent"} defines the minimum x-axis limit as 2004, which
corresponds to the Pelagic Organism Decline.}

\item{season}{The season for the Seasonal Monitoring Report.
Must be one of the following: \code{"winter"}, \code{"spring"},
\code{"summer"}, \code{"fall"}, or \code{"annual"}.}

\item{interval}{(optional) override the x-axis break interval.
By default, \code{type = "all"} results in a break interval of 10 years
while \code{type = "recent"} results in a break interval of 5 years.}

\item{start_year}{(optional) override the x-axis lower limit.}
}
\value{
A ggplot continuous x-axis scale.
}
\description{
Standardizes the x-axis limits and breaks of a ggplot.
}
