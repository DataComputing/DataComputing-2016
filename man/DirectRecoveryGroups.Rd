\name{DirectRecoveryGroups}
\alias{DirectRecoveryGroups}
\docType{data}
\title{
Descriptions of the Direct Recovery Groups (DRGs) in the Medicare data.
}
\description{
Provides the medical meaning of the DRG codes.
}
\usage{data("DirectRecoveryGroups")}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{drg}}{a character string that looks like a number}
    \item{\code{drgDefinition}}{a text description of the DRG}
  }
}
\details{
This data table is related to \code{MedicareCharges} and \code{MedicareProviders}.
}
\source{
Extracted from the highly repetitive table provided by the Centers for Medicare and Medicaid Services.  See \url{http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient.html}.
}
\examples{
head(DirectRecoveryGroups)
}
\keyword{datasets}
