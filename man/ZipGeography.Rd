\name{ZipGeography}
\alias{ZipGeography}
\docType{data}
\title{Geographic information by US Zip Codes (Postal Codes)}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ZipGeography")}
\format{
  A data frame with 42741 observations on the following 13 variables.
  \describe{
    \item{\code{State}}{a factor}
    \item{\code{Population}}{a numeric vector}
    \item{\code{HousingUnits}}{a numeric vector}
    \item{\code{LandArea}}{a numeric vector}
    \item{\code{WaterArea}}{a numeric vector}
    \item{\code{CityName}}{a factor with levels}
    \item{\code{AreaCode}}{a numeric vector}
    \item{\code{Timezone}}{a factor with levels \code{ } \code{EST} \code{EST+1} \code{CST} \code{MST} \code{PST} \code{PST-2} \code{PST-3} \code{PST-4} \code{PST-5} \code{PST-6} \code{PST-7} \code{PST-1}}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\code{ZIP}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ZipGeography)
## maybe str(ZipGeography) ; plot(ZipGeography) ...
}
\keyword{datasets}
