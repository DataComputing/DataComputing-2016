\name{OrdwayBirds}
\alias{OrdwayBirds}
\docType{data}
\title{
Birds captured and released at Ordway, complete and uncleaned
}
\description{
The historical record of birds captured and released at the Katharine Ordway Natural History Study Area, a 278-acre preserve in Inver Grove Heights, Minnesota, owned and managed by Macalester College.
}
\usage{data(OrdwayBirds)}
\format{
  A data frame with 15829 observations on the bird's species, size, date found, and band number.
  
  Timestamp indicates when the data were entered into an electronic record, not anything about the bird being described. 
}
\details{
There are many extraneous levels of variables such as species.  Part of the purpose of this data set is to teach about data cleaning.

A translation table for cleaning the species names is in \code{OrdwaySpecies}.r
}
\source{
Jerald Dosch, Dept. of Biology, Macalester College: the manager of the Study Area.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(OrdwayBirdsOrig)
}
\keyword{datasets}
