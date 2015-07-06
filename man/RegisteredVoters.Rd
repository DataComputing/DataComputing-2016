\name{RegisteredVoters}
\alias{RegisteredVoters}
\docType{data}
\title{
A sample of the voter registration list for Wake County, North Carolina in Fall 2010.
}
\description{

}
\usage{data(RegisteredVoters)}
\format{
  A data frame with 10000 rows.
  \describe{
    \item{\code{gender}}{M or F}
    \item{\code{party}}{Party affiliation: DEM=Democratic, LIB=Libertarian,REP=Republican,UNA=Unaffiliated}
    \item{Age}{age in years}
    \item{\code{race}}{race: A=Asian, B=black or African American, I=American Indian or Alaska Native, M=More than one race, O=Other, U=Undesignated,W=White}
    \item{\code{voter_reg_num}}{Unique serial number for each voter}
    \item{\code{last_name}}{Voter's last name}
    \item{\code{first_name}}{Voter's first name}
    \item{\code{midl_name}}{Voter's middle name}
    \item{\code{name_sufx}}{Suffix, if any, on the voter's name}
  }
}
\details{
   For additional fields, see the original documentation at \url{http://msweb03.co.wake.nc.us/bordelec/Waves/boedatadescription.pdf}.
}
\source{
The data file is publicly available from the Wake County (NC) Board of Elections.  \url{http://msweb03.co.wake.nc.us/bordelec/Waves/WavesDownload.asp}. It's in a Windows self-extracting format.   Prof. Shilad Sen reformatted the data for the DataComputing package.

The full data set, containing 583,092 records, is available at \url{http://www.mosaic-web.org/go/Repository/DannyKaplan/WakeCountyBOE.Rdata}.  You can access it by downloading the file, then loading it into your R session.  The data frame is \code{WakeCountyBOE}.
}
\references{
}
\examples{
data(RegisteredVoters)
}
\keyword{datasets}
