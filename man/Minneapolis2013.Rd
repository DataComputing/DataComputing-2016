\name{Minneapolis2013}
\alias{Minneapolis2013}
\docType{data}
\title{Ballots in the 2013 Mayoral election in Minneapolis}
\description{The choices marked on each (valid) ballot for the election, which was run using a rank-choice, instant runoff system.}
\usage{data("Minneapolis2013")}
\format{
  A data frame with 80101 observations on the following 5 variables. All are stored as character strings.
  \describe{
    \item{\code{Precinct}}{Precincts are sub-divisions within Wards}
    \item{\code{First}}{The voter's first choice}
    \item{\code{Second}}{The voter's second choice}
    \item{\code{Third}}{The voter's third choice}
    \item{\code{Ward}}{The city is divided spatially into districts or 'wards'.  These are further subdivided into precincts }
  }
}
\details{
Ballot information for the 2013 Minneapolis Mayoral election, which was run as a rank-choice election.  In rank-choice, a voter can indicate first, second, and third choices.  If a voter's first choice is eliminated (by being last in the count across voters), the second choice is promoted to that voter's first choice, and similarly third -> second.  Eliminations are done successively until one candidate has a majority of the first-choice votes.
}
\source{
Ballot data from the Minneapolis city government: http://vote.minneapolismn.gov/www/groups/public/@clerk/documents/webcontent/2013-mayor-cvr.xlsx
}
\references{
Description of ranked-choice voting: http://vote.minneapolismn.gov/rcv/index.htm

A Minnesota Public Radio story about the election ballot tallying process: http://minnesota.publicradio.org/display/web/2013/11/22/politics/ranked-choice-vote-count-programmers

The Wikipedia article about the election: http://en.wikipedia.org/wiki/Minneapolis_mayoral_election,_2013
}
\examples{
data(Minneapolis2013)
str(Minneapolis2013)
}
\keyword{datasets}
