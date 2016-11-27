\name{vectorPNGbuttons}
\alias{vectorPNGbuttons}
\title{Vector of PNG files representing colored caps (buttons)}
\description{\code{vectorPNGbuttons} returns a vector with the filenames of the PNG files representing colored caps (buttons) from a data.frame.
}
\usage{
vectorPNGbuttons(capsData=get("FarnsworthD15", envir = environment()))
}
\arguments{
  \item{capsData}{ data.frame with RGB values of colored caps (buttons).}
}
\value{
  vector with path+filenames of PNG files.
}
\author{Jose Gama}
\examples{
\dontrun{
vectorPNGbuttons(FarnsworthD15)
}
}
\keyword{programming}
