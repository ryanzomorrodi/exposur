% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_anderson.R
\name{calc_anderson_wind}
\alias{calc_anderson_wind}
\title{Calculate Anderson et al. Wind}
\usage{
calc_anderson_wind(hurdat2, geography, geoid = "GEOID")
}
\arguments{
\item{hurdat2}{Output from \code{get_hurdat2()}}

\item{geography}{\code{sf} object for the area of interest}

\item{geoid}{Unique identifier for geography}
}
\value{
Maximum wind and gust speed and duration calculated
using the \code{stormwindmodel}
}
\description{
Leverages the \href{https://github.com/geanders/stormwindmodel}{\code{stormwindmodel}}
to allow for the approximate calculation wind exposure at
individual points.
}
\details{
\subsection{Centroids}{

By default this package will take the mean centroid of whatever
geography provided to it. If you would like to approximate exposure
at the population weighted centroid use \code{centr::mean_center()} on
your geography before providing it to this function.
}
}
\examples{
\dontrun{
harvey_anderson_wind <- calc_anderson_wind(
  harvey_hurdat2,
  TXLA_tracts
)
}
}
