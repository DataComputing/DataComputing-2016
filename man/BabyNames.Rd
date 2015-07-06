\name{BabyNames}
\alias{BabyNames}
\alias{BabyNamesYearly}
\docType{data}
\title{
Names of children as recorded by the US Social Security Administration.
}
\description{
The US Social Security Administration provides yearly lists of names given to babies.  These data combine the yearly lists.

\code{BabyNames} is the raw data from the SSA. The case is a year-name-sex, for example: Jane F 1922.  The \code{count} is the number of children of that sex given that name in that year.  Names assigned to fewer than five children of one sex in any year are not listed, presumably out of privacy concerns.
}
\usage{data(BabyNames)}
\format{
  \code{BabyNames} consists of 1,792,091 entries, each of which has four variables:
  \describe{
    \item{\code{name}}{The given name (character string)}
    \item{\code{sex}}{F or M (character string)}
    \item{\code{count}}{The number of babies given that name and of that sex. (integer)}
    \item{\code{year}}{Year of birth (integer)}
  }
}
\details{
}
\source{
The data were compiled from the Social Security Administration web site: \code{http://www.ssa.gov/oact/babynames/names.zip}.
}
\seealso{\code{BabyNames}}
\references{}
\examples{
data(BabyNames)
str(BabyNames)
}
\keyword{datasets}
