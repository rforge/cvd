\name{createPNGbuttons}
\alias{createPNGbuttons}
\title{Creates PNG files to be used as colored caps (buttons)}
\description{ \code{createPNGbuttons} creates PNG files from a data.frame with RGB values.
}
\usage{
createPNGbuttons(capsData = get("FarnsworthD15", envir = environment()),
 imgLength = 44, imgWidth = 78)
}
\arguments{
  \item{capsData}{ Input file name.}
  \item{imgLength}{ Input file name.}
  \item{imgWidth}{ Input file name.}
}
\value{
  png file object.
}
\author{Jose Gama}
\examples{
\dontrun{
createPNGbuttons(data.frame(R=0,G=0,B=0))
data(FarnsworthD15)
createPNGbuttons(FarnsworthD15)
}
}
\keyword{programming}
