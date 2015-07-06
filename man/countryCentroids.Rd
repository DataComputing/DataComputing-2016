\name{CountryCentroids}
\alias{CountryCentroids}
\docType{data}
\title{Geographic locations of countries}
\description{Gives the latitude and longitude for many countries.  A single point --- the centroid of the country's boundaries --- is given.  Countries are identified by English name and by the ISO A3 code.}
\usage{data("CountryCentroids")}
\format{
  A data frame with 241 observations on the following 4 variables.
  \describe{
    \item{\code{name}}{country name in English, e.g. \code{Afghanistan} \code{Aland} \code{Albania} \code{Algeria}  \code{Yemen} \code{Zambia} \code{Zimbabwe}}
    \item{\code{iso_a3}}{The ISO A3 code for the country, e.g. s \code{-99} \code{ABW} \code{AFG} \code{AGO} \code{YEM} \code{ZAF} \code{ZMB} \code{ZWE}}
    \item{\code{long}}{Longitude in degrees}
    \item{\code{lat}}{Latitude in degrees}
  }
}
\source{The centroids were derived from country boundary map data as the medians of the boundary points latitude and longitude.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(CountryCentroids)
plot(lat ~ long, data=CountryCentroids)
}
\keyword{datasets}
