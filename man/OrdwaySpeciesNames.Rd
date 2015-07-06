\name{OrdwaySpeciesNames}
\alias{OrdwaySpeciesNames}
\docType{data}
\title{
Corrected Species Names for the Ordway Birds
}
\description{
This data frame lists all the species name that appear in \code{OrdwayBirds}.  In many cases, the species name was mis-spelled in the original.  As a result, many birds are listed as separate species even though, in reality, they all belong to the same species.  For each potentially mis-spelled species name, this table gives a standardized name.
}
\usage{data("OrdwaySpeciesNames")}
\format{
  A data frame to translate between the various mis-spellings of species in \code{OrdwayBirds} to a consistent set of spellings.
  \describe{
    \item{\code{SpeciesName}}{The original spelling.}
    \item{\code{SpeciesNameCleaned}}{Corrected spelling (or \code{NA} if the original was not identifiable.)}
  }
}
\details{
These data can be used as part of cleaning the \code{OrdwayBirds} data.
}
\source{
See \code{OrdwayBirds}.  The data in \code{SpeciesNameCleaned} was added by crowd-sourcing.  A class of Data and Computing Fundamentals students, in Spring 2013, read through the original names and typed in the corresponding correct spelling.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# To juxtapose the corrected spelling for each bird in OrdwayBirds 
merge( OrdwayBirds, OrdwaySpeciesNames )
}
\keyword{datasets}
