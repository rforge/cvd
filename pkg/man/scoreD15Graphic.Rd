\name{scoreD15Graphic}
\alias{scoreD15Graphic}
\title{Graphical score for the D-15 tests}
\description{\code{scoreD15Graphic} computes the graphical score for the D-15 test or similar. The input is either a vector of RGB colors or cap positions.}
\usage{
scoreD15Graphic(userD15colors=NULL,userD15values=NULL, titleGraphic="Farnsworth dichotomous test (D-15) results", okD15colors=NULL)
}
\arguments{
  \item{userD15colors}{ RGB colors chosen by tester}
  \item{userD15values}{ position values chosen by tester}
  \item{titleGraphic}{ title for the graphic}
  \item{okD15colors}{ vector with RGB colors in the correct sequence}
}
\value{
  none
}
\author{Jose Gama}
\references{
Farnsworth D. The Farnsworth Dichotomous Test for Color Blindness Panel D-15 Manual. New York, The Psychological Corp., 1947, pp. 1-8.
}
\examples{
# a "perfect" score
\dontrun{scoreD15Graphic(userD15values=1:15)}
}
\keyword{programming}
