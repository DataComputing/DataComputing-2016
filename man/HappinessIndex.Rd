\name{HappinessIndex}
\alias{HappinessIndex}
\docType{data}
\title{
World Happiness Report Data
}
\description{
The United Nations Sustainable Development Solutions Network releases a report giving several indices of "happiness" measured for each of more than 150 countries.  These data are from the 2013 report (\url{http://unsdsn.org/resources/publications/world-happiness-report-2013/}).  Underlying the data are the results of Gallop World Polls --- the country data is presumably the aggregate of data from individual people, although how the aggregate is made is uncertain to this writer.
}
\usage{data("HappinessIndex")}
\format{
  A data frame with 156 observations on the following 13 variables.
  \describe{
    \item{\code{country}}{name of the country}
    \item{\code{region}}{numerical code for a region of the world}
    \item{\code{score}}{The happiness index: a number from 0 to 10}
    \item{\code{socialSupport}}{A measure of having someone to count on}
    \item{\code{freedom}}{Perceived freedom to make life choices}
    \item{\code{corruption}}{Freedom from corruption}
    \item{\code{donation}}{}
    \item{\code{generosity}}{The generosity of people}
    \item{\code{affectPos}}{}
    \item{\code{affectNeg}}{}
    \item{\code{happinessYesterday}}{A presumably subjective measure of how happy the poll respondants were the day before the poll.}
    \item{\code{gdpPerCapita}}{In dollars}
    \item{\code{lifeExpectancy}}{The "healthy life expectancy" in years.}
  }
}
\details{
The healthy life expectancy (HALE) is a World Health Organization measure of how long, on average, a person can be expected to live in good health.  

The data were scraped from a PDF file released by the United Nations.  A few country names were changed to match those in  \code{CountryData}. (Example: Myanmar -> Burma)  Not all of the \code{HappinessIndex} countries are in \code{CountryData}, and vice versa.
}
\source{
The World Happiness Report is at \url{http://unsdsn.org/wp-content/uploads/2014/02/WorldHappinessReport2013_online.pdf}.  The original data used to create \code{HappinessIndex} are at \url{http://unsdsn.org/wp-content/uploads/2013/09/Chapter-2_online-appendix_9-5-13_final.pdf}.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(HappinessIndex)
## maybe str(HappinessIndex) ; plot(HappinessIndex) ...
}
\keyword{datasets}
