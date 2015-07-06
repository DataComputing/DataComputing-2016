\name{CountryGroups}
\alias{CountryGroups}
\docType{data}
\title{
Membership in Country Groups
}
\description{
A data frame listing countries with logical variables indicating whether each country belongs to the G8, the G20, or the GGG.
}
\usage{data("CountryGroups")}
\format{
  \describe{
    \item{\code{G8}}{country belongs to G8}
    \item{\code{G20}}{country belongs to G20}
    \item{\code{GGG}}{country belongs to the GGG}
  }
}
\details{
Each of the variables \code{G8}, \code{G20}, \code{GGG} is a logical variable.  \code{TRUE} means the country belongs to the group.

The country names are standardized to the CIA World Factbook names.
}
\source{
\url{http://en.wikipedia.org/wiki/G8},
\url{https://www.g20.org/about_g20/g20_members},
\url{http://www.mfa.gov.sg/content/mfa/overseasmission/newyork/nyemb_statements/global_governance_group/2012/201209/press_20122809.html}
}

\examples{
data(CountryGroups)
## maybe str(CountryGroups) ; plot(CountryGroups) ...
}
\keyword{datasets}
