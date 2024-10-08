% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_closest_dist.R
\name{calc_closest_dist}
\alias{calc_closest_dist}
\title{Calculate Closest Distance and Time}
\usage{
calc_closest_dist(hurdat2, geography, geoid = "GEOID")
}
\arguments{
\item{hurdat2}{Output from \code{get_hurdat2()}}

\item{geography}{\code{sf} object for the area of interest}

\item{geoid}{Unique identifier for geography}
}
\value{
Closest distance and time to track for each geography
}
\description{
Utilizes \href{https://github.com/geanders/stormwindmodel}{\code{stormwindmodel}}
to impute the \code{hurdat2} track to 15 minute intervals, calculate
at what time point the storm is closest, and record the distance
from track at that time.
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
harvey_closest_dist <- calc_closest_dist(
  harvey_hurdat2,
  TXLA_tracts
)
}
}
