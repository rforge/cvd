\name{scoreD15TCDS}
\alias{scoreD15TCDS}
\title{Total Color Difference Score (TCDS) for the D-15 tests}
\description{\code{scoreD15TCDS} computes the Total Color Difference Score (TCDS) for the D-15 test, from Bowman's (1982). The input is either a vector of RGB colors or cap positions.}
\usage{
scoreD15TCDS(userD15colors=NULL,userD15values=NULL, distTable = BowmanTCDS, D15colors = FarnsworthD15)
}
\arguments{
  \item{userD15colors}{ RGB colors chosen by tester}
  \item{userD15values}{ position values chosen by tester}
  \item{distTable}{ distance table - matrix with the color distances}
  \item{D15colors}{ RGB colors for the CVD test}
}
\value{
  \item{TCDS}{ Total Color Difference Score (TCDS)}
}
\author{Jose Gama}
\references{
Bowman's (1982) Total Color Difference Score (TCDS) for congenitally defective observers on the D-15 with enlarged tests.
K.J. Bowman, A method for quantitative scoring of the Farnsworth Panel D-15, Acta Ophthalmologica, 60 (1982), pp. 907–916
}
\examples{
# a "perfect" score
\dontrun{
scoreD15TCDS(userD15values=1:15)}
}
\keyword{programming}
