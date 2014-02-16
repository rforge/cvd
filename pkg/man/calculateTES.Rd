\name{calculateTES}
\alias{calculateTES}
\title{total error score (TES) using Farnsworth's or Kinnear's method}
\description{\code{calculateTES} computes the total error score (TES) using Farnsworth's or Kinnear's method for the FM-100, D-15, Roth-28 and so forth. The input is a vector of cap positions.}
\usage{
calculateTES(fmData, Kinnear=FALSE)
}
\arguments{
  \item{fmData}{ vector of cap positions}
  \item{Kinnear}{ position values chosen by tester}
}
\value{
  \item{TCDS}{ Total Color Difference Score (TCDS)}
}
\author{Jose Gama}
\references{
Farnsworth D. The Farnsworth-Munsell 100-Hue Test. Baltimore: Munsell Color Company, 1957.
}
\examples{
# a "perfect" score
\dontrun{
calculateTES(userD15values=1:15)}
}
\keyword{programming}
