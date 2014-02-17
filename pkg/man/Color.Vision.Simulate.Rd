\name{Color.Vision.Simulate}
\alias{Color.Vision.Simulate}
\title{Simulate CVDs on images}
\description{\code{Color.Vision.Simulate} converts images so that the colors look similar to how they are seen by people with CVD.}
\usage{
Color.Vision.Simulate(fileIN=NULL, fileOUT=NULL, myoptions=NULL, amount=1.0)
}
\arguments{
  \item{fileIN}{ PNG input file}
  \item{fileOUT}{ PNG output file}
  \item{myoptions}{ CVD from "Protanope","Deuteranope" or "Tritanope"}
  \item{amount}{ level from 0.0 to 1.0 for "Achromat"}
}
\value{
  none
}
\author{Jose Gama}
\references{
Michael Deal Daltonize.org http://mudcu.be/labs/Color/Vision http://www.daltonize.org/p/about.html "Analysis of Color Blindness" by Onur Fidaner, Poliang Lin and Nevran Ozguven.
"Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats" by Francoise Vienot, Hans Brettel and John D. Mollon http://vision.psychol.cam.ac.uk/jdmollon/papers/colourmaps.pdf
}
\examples{
# a "perfect" score
\dontrun{
fname<-paste(system.file(package='CVD'),'/extdata/fruits.png',sep='')
Color.Vision.Simulate(fname, 'fruits.Simulate.Protanope.png','Protanope')
Color.Vision.Simulate(fname, 'fruits.Simulate.Deuteranope.png','Deuteranope')
Color.Vision.Simulate(fname, 'fruits.Simulate.Tritanope.png','Tritanope')
}
}
\keyword{programming}
