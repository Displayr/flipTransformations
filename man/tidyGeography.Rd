% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo.R
\name{tidyGeography}
\alias{tidyGeography}
\title{Use Google's geocoding API to return geocoded data from a vector of locations.}
\usage{
tidyGeography(x, api.key)
}
\arguments{
\item{x}{A vector of locations.}

\item{api.key}{Google geocoding API key.}
}
\description{
Use Google's geocoding API to return geocoded data from a vector of locations.
}
\details{
This fuction iterates through a character vector of locations and returns
a data.frame of geocoded variables including country, city, latitude, longitude, etc.
}
\examples{
# vector of US postal codes
x <- c('98406', '40601', '32801', '79835', '61701')
# vector of US cities and states
x <- c('Yorba Linda, CA', 'Boise, ID', 'Tulsa, OK', 'Pittsburgh, PA', 'Birmingham, AL')
#vector of cities around the world
x <- c('Okinawa, Japan', 'Lisbon, Portugal', 'Toronto, Canada', 'Melbourne, Australia')
}
