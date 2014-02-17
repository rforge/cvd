\name{Color.Vision.c2g}
\alias{Color.Vision.c2g}
\alias{RGBtoHSL}
\title{Decolorize an image using the c2g algorithm}
\description{\code{Color.Vision.c2g} decolorizes an image using the c2g algorithm from Martin Faust (2008).
\code{RGBtoHSL} converts from RGB to HSL, used by \code{Color.Vision.c2g}}
\usage{
Color.Vision.c2g(fileIN=NULL, fileOUT=NULL, CorrectBrightness=FALSE)
}
\arguments{
  \item{fileIN}{ PNG input file}
  \item{fileOUT}{ PNG output file}
  \item{CorrectBrightness}{ automatic brightness correction}
}
\value{
  none
}
\author{Jose Gama}
\references{
Martin Faust 2008 http://www.e56.de/c2g.php
}
\examples{
\dontrun{
fname<-paste(system.file(package='CVD'),'/extdata/fruits.png',sep='')
Color.Vision.c2g(fname, 'fruits.c2g.png')
}
}
\keyword{programming}
