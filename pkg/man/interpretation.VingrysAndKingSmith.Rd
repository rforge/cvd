\name{interpretation.VingrysAndKingSmith}
\alias{interpretation.VingrysAndKingSmith}
\alias{interpretation.Foutch}
\title{Automatic interpretation of test scores}
\description{\code{interpretation.VingrysAndKingSmith} and \code{interpretation.Foutch} perform an interpretation of the test results based on the classification ranges from the authors of the tests.}
\usage{
interpretation.VingrysAndKingSmith(VKS,optMethod=88)
}
\arguments{
  \item{VKS}{ data to be interpreted}
  \item{optMethod}{ CVD test method}
}
\value{
  \item{TCDS}{ Total Color Difference Score (TCDS)}
}
\author{Jose Gama}
\references{
Vingrys, A.J. and King-Smith, P.E. (1988). A quantitative scoring technique for panel tests of color vision.
Investigative Ophthalmology and Visual Science, 29, 50-63.

A new quantitative technique for grading Farnsworth D-15 color panel tests
Foutch, Brian K.; Stringham, James M.; Lakshminarayanan, Vasuvedan
Journal of Modern Optics, vol. 58, issue 19-20, pp. 1755-1763

Evaluation of the new web-based" Colour Assessment and Diagnosis" test
J Seshadri, J Christensen, V Lakshminarayanan, CJ BASSI
Optometry & Vision Science 82 (10), 882-885
}
\examples{
# a "perfect" score
\dontrun{
interpretation.VingrysAndKingSmith(D15Foutch(1:15))}
}
\keyword{programming}
