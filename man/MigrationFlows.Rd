\name{MigrationFlows}
\alias{MigrationFlows}
\docType{data}
\title{
Human Migration between Countries
}
\description{
The number of people who moved in from one country to another.  Data are provided for 1960, 1970, 1980, 1990, and 2000.  There are separate counts for males and females.}
\usage{data("MigrationFlows")}
\format{
  A data frame with 107184 observations on the following 8 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{Female} and \code{Male}}
    \item{\code{to}}{three letter country codes indicating the destination of the migrants}
    \item{\code{from}}{three letter country codes indicating where the migrants left}
    \item{\code{Y2000}}{Migration in year 2000.  The value is the number of people (males and females separately) who migrated from the origin country to the destination country.}
    \item{\code{Y1990}}{Migration in year 1990}
    \item{\code{Y1980}}{Migration in year 1980}
    \item{\code{Y1970}}{Migration in year 1970}
    \item{\code{Y1960}}{Migration in year 1960}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
The World Bank's Global Bilateral Migration Database: http://data.worldbank.org/data-catalog/global-bilateral-migration-databas
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(MigrationFlows)
str(MigrationFlows)
}
\keyword{datasets}
