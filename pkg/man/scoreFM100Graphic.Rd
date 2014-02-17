\name{scoreFM100Graphic}
\alias{scoreFM100Graphic}
\title{Graphical score for the D-15 tests}
\description{\code{scoreFM100Graphic} computes the graphical score for the
 FM-100 test or similar. The input is either a vector of RGB colors or cap positions.}
\usage{
scoreFM100Graphic(userFM100colors=NULL,userFM100values=NULL, titleGraphic=
"Farnsworth Munsell 100-Hue test results", okFM100colors=NULL, Kinnear=FALSE)
}
\arguments{
  \item{userFM100colors}{ RGB colors chosen by tester}
  \item{userFM100values}{ position values chosen by tester}
  \item{titleGraphic}{ title for the graphic}
  \item{okFM100colors}{ vector with RGB colors in the correct sequence}
  \item{Kinnear}{ logical, scoring method TRUE = Farnsworth, FALSE = Kinnear}
}
\value{
  none
}
\author{Jose Gama}
\references{
Dean Farnsworth, 1943
The Farnsworth Munsell 100-hue dichotomous tests for colour vision
Journal of the Optical Society of America, 33 (1943), pp. 568–576
}
\examples{
# an example score
\dontrun{
FM100example<-exampleFM100
userFM100values=cbind(FM100example[1,], FM100example[4,-22],
 FM100example[7,-22], FM100example[10,-22])
userFM100values=as.vector(unlist(userFM100values))
scoreFM100Graphic(userFM100values)
}
}
\keyword{programming}
