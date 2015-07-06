\name{NCI60}
\alias{NCI60}

\docType{data}
\title{Gene expression in cancer.}
\description{The data come from a National Cancer Institute study of gene expression in cell lines drawn from various sorts of cancer.

The expression data, \code{NCI60} is a dataframe of 41078 gene probes (rows) and 60 cell lines (columns).  The first column, \code{Probe} gives the name of the Agilent microarray probe.  Each of the remaining columns is named for a cell line.  The value is the log-2 expression associated with that probe for the cell line.

\code{NCI60cells} gives information about each cell line.
}
\seealso{\code{\link{NCI60cells}}}
\usage{data("NCI60cells")}
\format{
  See the description
}
\details{}
\source{
DRAFT: Get the web address for the NCI60 data
}
\references{
Staunton et al.(http://www.pnas.org/content/98/19/10787.full

D.T. Ross et al. (2000) Nature Genetics, 24(3):227-234 \link{http://discover.nci.nih.gov/host/2000_systematic_abstract.jsp}
}
\examples{
data(nci60cellLine) 
}
\keyword{datasets}
