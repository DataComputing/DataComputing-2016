\name{NCI60cells}
\alias{NCI60cells}
\docType{data}
\title{
Cell Line descriptions in the NCI-60 dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("NCI60cells")}
\format{
  A data frame with 60 observations on the following 12 variables.
  \describe{
    \item{\code{cellLine}}{Name of the cell line}
    \item{\code{tissue}}{The cancer tissue from which the cell line originated \code{Breast} \code{CNS} \code{Colon} \code{Leukemia} \code{Melanoma} \code{Non-Small Cell Lung} \code{Ovarian} \code{Prostate} \code{Renal}}
    \item{\code{age}}{of the donor}
    \item{\code{sex}}{of the donor}
    \item{\code{prior.treatment}}{treatment of the donor}
    \item{\code{epithelial}}{is the cell of epithelial origin}
    \item{\code{histology}}{}
    \item{\code{p53}}{a factor with levels \code{?} \code{MT} \code{WT}}
    \item{\code{mdr}}{a numeric vector}
    \item{\code{doublingtime}}{a numeric vector}
  }
}
\details{
These are the cell-line descriptions corresponding to the columns in the NCI60 expression dataset.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{NCI60}}}
\examples{
data(NCI60cells)
}
\keyword{datasets}
