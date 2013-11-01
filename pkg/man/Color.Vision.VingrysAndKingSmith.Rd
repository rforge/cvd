\name{Color.Vision.VingrysAndKingSmith}
\alias{Color.Vision.VingrysAndKingSmith}
\title{Scoring the results of the "D-15", "D-15DS" or "FM1OO-Hue" tests}
\description{\code{Color.Vision.VingrysAndKingSmith} takes a vector with cap numbers from the "D-15", "D-15DS" or "FM1OO-Hue" tests and outputs the score by the method from Vingrys and King-Smith.}
\usage{
Color.Vision.VingrysAndKingSmith(capnumbers=NULL,testType='D-15',silent=TRUE)
}
\arguments{
  \item{capnumbers}{ vector with cap numbers}
  \item{testType}{ test type, one of "D-15", "D-15DS" or "FM1OO-Hue"}
  \item{silent}{ logical, if TRUE then the function will send output to the screen, similarly to the original version}
}
\value{
  \item{Angle}{ confusion angle which identifies the type of color defect}
  \item{MajRad}{ major moment of inertia}
  \item{MinRad}{ minor moment of inertia}
  \item{TotErr}{ error score or estimate of the severity of color defect}
  \item{Sindex}{ Selectivity-Index which quantifies the amount of polarity or lack of randomness in a cap arrangement}
  \item{Cindex}{ Confusion-Index which quantifies the degree of color loss relative to a perfect arrangement of caps}
}
\author{Jose Gama}
\references{
Vingrys, A.J. and King-Smith, P.E. (1988).
A quantitative scoring technique for panel tests of color vision.
Investigative Ophthalmology and Visual Science, 29, 50-63.
}
\examples{
Color.Vision.VingrysAndKingSmith(1:15,silent=FALSE)
#result from the original GW Basic version:
#SUMS OF U AND V              41.25999     -4.92
# ANGLE MAJ RAD MIN RAD TOT ERR S-INDEX C-INDEX
#    61.98     9.23     6.71    11.42     1.38     1.00
Color.Vision.VingrysAndKingSmith(1:15,'D-15DS',silent=FALSE)
#result from the original GW Basic version:
#SUMS OF U AND V              26.86001     -38.69
# ANGLE MAJ RAD MIN RAD TOT ERR S-INDEX C-INDEX
#    61.44     5.12     3.60     6.26     1.42     1.00
Color.Vision.VingrysAndKingSmith(1:85, 'FM1OO-Hue',silent=FALSE)
#result from the original GW Basic version:
#SUMS OF U AND V              423.7896      203.7294
# ANGLE MAJ RAD MIN RAD TOT ERR S-INDEX C-INDEX
#    54.15     2.53     1.97     3.20     1.28     1.00
}
\keyword{programming}
