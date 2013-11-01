\name{GellerTCDS}
\alias{GellerTCDS}
\title{Table of color distance scores for quantitative scoring of the Lanthony desaturate D-15s test}
\usage{GellerTCDS}
\description{\code{GellerTCDS} contains the color distance scores for quantitative scoring of the Lanthony desaturate D-15s test, from Geller AM. (2001).
The Lanthony desaturate test (D-15s) is a short test for detecting acquired color vision deficiencies. Geller AM (2001) created a table based on the Commission Internationale de l'Eclairage (International Commission on Illumination, CIE) Space and Color Difference formula, CIE 1976 (L*a*b*) with perceptual distances between pairs of caps. The table is used for the calculation of the Total Color Distance Score (TCDS) which is the sum of the CIELAB space distances between colored caps.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Pilot}{ Distances between colored caps for the pilot cap}
    \item{Cap1}{ Distances between colored caps for the 1st cap}
    \item{Cap2}{ Distances between colored caps for the 2nd cap}
    \item{Cap3}{ Distances between colored caps for the 3rd cap}
    \item{Cap4}{ Distances between colored caps for the 4th cap}
    \item{Cap5}{ Distances between colored caps for the 5th cap}
    \item{Cap6}{ Distances between colored caps for the 6th cap}
    \item{Cap7}{ Distances between colored caps for the 7th cap}
    \item{Cap8}{ Distances between colored caps for the 8th cap}
    \item{Cap9}{ Distances between colored caps for the 9th cap}
    \item{Cap10}{ Distances between colored caps for the 10th cap}
    \item{Cap11}{ Distances between colored caps for the 11th cap}
    \item{Cap12}{ Distances between colored caps for the 12th cap}
    \item{Cap13}{ Distances between colored caps for the 13th cap}
    \item{Cap14}{ Distances between colored caps for the 14th cap}
    \item{Cap15}{ Distances between colored caps for the 15th cap}
  }
}
\source{
Geller AM. A table of color distance scores for quantitative scoring of the Lanthony desaturate color vision test. Neurotoxicol Teratol 2001; 23: 265-267. 
}
\references{
Geller AM. A table of color distance scores for quantitative scoring of the Lanthony desaturate color vision test. Neurotoxicol Teratol 2001; 23: 265-267. 
}
\author{Jose Gama}
\examples{
data(GellerTCDS)
GellerTCDS
}
\keyword{datasets}

