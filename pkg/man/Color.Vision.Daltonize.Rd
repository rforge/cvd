\name{Color.Vision.Daltonize}
\alias{Color.Vision.Daltonize}
\title{Daltonize images}
\description{\code{Color.Vision.Daltonize} converts images so that the most problematic colors are more visible to people with CVD.}
\usage{
Color.Vision.Daltonize(fileIN=NULL, fileOUT=NULL, myoptions=NULL, amount=1.0)
}
\arguments{
  \item{fileIN}{ PNG input file}
  \item{fileOUT}{ PNG output file}
  \item{myoptions}{ CVD from "Protanope","Deuteranope" or "Tritanope"}
  \item{amount}{ UNUSED - level from 0.0 to 1.0 for "Achromat"}
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
Color.Vision.Daltonize(userD15values=1:15)}
}
\keyword{programming}
