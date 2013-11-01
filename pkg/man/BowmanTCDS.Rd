\name{BowmanTCDS}
\alias{BowmanTCDS}
\title{Table of color distance scores for quantitative scoring of the Farnsworth panel D-15 test}
\usage{BowmanTCDS}
\description{\code{BowmanTCDS} contains the color distance scores for quantitative scoring of the Farnsworth panel D-15 test, from Bowman KJ (1982)
The Farnsworth Dichotomous test (D-15) is a short test for detecting congenital color vision deficiencies. Bowman KJ (1982) created a table based on the Commission Internationale de l'Eclairage (International Commission on Illumination, CIE) Space and Color Difference formula, CIE 1976 (L*a*b*) with perceptual distances between pairs of caps. The table is used for the calculation of the Total Color Distance Score (TCDS) which is the sum of the CIELAB space distances between colored caps.
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
Bowman KJ: A method for quantitative scoring of the Farnsworth panel D-15. Acta Ophthalmol 60:907, 1982.
}
\references{
Bowman KJ: A method for quantitative scoring of the Farnsworth panel D-15. Acta Ophthalmol 60:907, 1982.
}
\author{Jose Gama}
\examples{
data(BowmanTCDS)
BowmanTCDS
}
\keyword{datasets}

