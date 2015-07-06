\name{NHANES}
\alias{NHANES}
\docType{data}
\title{
Older NHANES data: 1999-2004
}
\description{
These are the data used originally in Data Computing and for the mosaic Modeling book.  They have since been supplanted on CRAN with a new NHANES which has more modern data.

These data were collected as part of the NHANES 1999-2004 study.
}
\usage{data("NHANES")}

\format{A data frame with 31126 observations on the following 32 variables.
\itemize{
\item{\code{seqn} matching code for NHANES mortality data}
\item{\code{sex} \code{"male"} or \code{"female"}}
\item{\code{age} in years}
\item{\code{pregnant} \code{"yes"} or \code{"no"}}
\item{\code{ethnicity} Mexican American,
Other Hispanic, Non-Hispanic White, Non-Hispanic Black, or Other/Multi
}
\item{\code{death} \code{"alive"},  \code{"cardiovascular death"} or \code{"other death"}}
\item{\code{followup} months of follow up (for mortality data)}
\item{\code{smoker}
\code{"yes"} or \code{"no"}}
\item{\code{diabetic}
\code{"yes"} or \code{"no"}}
\item{\code{height} height (meters)}
\item{\code{weight} weight (kilograms)}
\item{\code{waist} waist circumference (meters)}
\item{\code{wci} the proposed body shape index}
\item{\code{bmi} body mass index}
\item{\code{ptfat} percent trunk fat}
\item{\code{tfat} mass of trunk fat}
\item{\code{lfat} limb fat}
\item{\code{llean} limb lean tissue}
\item{\code{lean_bmi} lean-tissue only BMI}
\item{\code{fat_bmi} fat-only BMI}
\item{\code{bone_bmi} bone BMI}
\item{\code{pct_fat} percent fat}
\item{\code{bmd} bone mineral density}
\item{\code{fmhm_other} Framingham risk score}
\item{\code{hdl} HDL cholesterol}
\item{\code{chol} cholesterol (LDL?)}
\item{\code{bps} systolic blood pressure, mmHg}
\item{\code{bpd} diastolic blood pressure, mmHg}
\item{\code{income} ratio of family income to poverty threshold. 5 is ratio greater than or equal to 5}
\item{\code{pop_weight} population weight, for adding up over the US population }
\item{\code{psu} primary sampling unit}
\item{\code{stratum} sampling stratum}
}}
\source{
These data were assempled from NHANES 1999-2004 sources by Dr. Nir
Krakauer at City College of New York.
}

\examples{
data(NHANES)
## maybe str(NHANES) ; plot(NHANES) ...
}
\keyword{datasets}
