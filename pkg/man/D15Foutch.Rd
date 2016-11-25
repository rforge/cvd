\name{D15Foutch}
\alias{D15Foutch}
\title{Quantitatively analyzes of D15 color panel tests}
\description{\code{D15Foutch} Calculates angle, magnitude and scatter for VK-S 88 and VK-S 93 (Vingrys, A.J. and King-Smith, P.E. (1988, 1993)), LSA 05 (Foutch/Bassi '05), and JMO 11 (Foutch/Stringham/Vengu '11).}
\usage{
D15Foutch(userD15values=NULL, testType = 'D-15', dataVKS = NA)
}
\arguments{
  \item{userD15values}{ position values chosen by tester}
  \item{testType}{ the CVD test to be scored: "D-15", "D-15DS", "Roth28-Hue" or "FM1OO-Hue"}
  \item{dataVKS}{ by default, the original 1976 CIE Luv data from Vingrys and King-Smith}
}
\value{
  \item{outmat}{ data.frame with columns "angle", "magnitude" and "scatter" and rows "LSA05","JMO11","VKS88","VKS93"}
}
\author{Brian K. Foutch}
\references{
A new quantitative technique for grading Farnsworth D-15 color panel tests
Foutch, Brian K.; Stringham, James M.; Lakshminarayanan, Vasuvedan
Journal of Modern Optics, vol. 58, issue 19-20, pp. 1755-1763

Evaluation of the new web-based" Colour Assessment and Diagnosis" test
J Seshadri, J Christensen, V Lakshminarayanan, CJ BASSI
Optometry & Vision Science 82 (10), 882-885

Vingrys, A.J. and King-Smith, P.E. (1988).
A quantitative scoring technique for panel tests of color vision.
Investigative Ophthalmology and Visual Science, 29, 50-63.
}
\examples{
# 2 examples from VK-S
\dontrun{
D15Foutch(userD15values=c(1:7,9,8,10:15))
D15Foutch(userD15values=c(1:7,9,8,10:13,15,14))}
}
\keyword{programming}
