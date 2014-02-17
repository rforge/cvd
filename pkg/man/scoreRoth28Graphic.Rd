\name{scoreRoth28Graphic}
\alias{scoreRoth28Graphic}
\title{Graphical score for the D-15 tests}
\description{\code{scoreRoth28Graphic} computes the graphical score for the 
Roth-28 test or similar. The input is either a vector of RGB colors or cap positions.}
\usage{
scoreRoth28Graphic(userR28colors=NULL,userR28values=NULL, titleGraphic=
"Roth-28 test results", okR28colors=NULL)
}
\arguments{
  \item{userR28colors}{ RGB colors chosen by tester}
  \item{userR28values}{ position values chosen by tester}
  \item{titleGraphic}{ title for the graphic}
  \item{okR28colors}{ vector with RGB colors in the correct sequence}
}
\value{
  none
}
\author{Jose Gama}
\references{
Carl Erb, Martin Adler, Nicole Stübiger, Michael Wohlrab, Eberhart Zrenner, 
Hans-Jürgen Thiel, Colour vision in normal subjects tested by the colour 
arrangement test ‘Roth 28-hue desaturated’, Vision Research, Volume 38, 
Issue 21, November 1998, Pages 3467-3471, ISSN 0042-6989, 
\url{http://dx.doi.org/10.1016/S0042-6989(97)00433-1}.
}
\examples{
# a "perfect" score
\dontrun{scoreRoth28Graphic(userD15values=1:28)}
}
\keyword{programming}
