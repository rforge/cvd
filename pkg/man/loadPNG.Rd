\name{loadPNG}
\alias{loadPNG}
\title{Load a PNG file}
\description{ \code{loadPNG} loads a PNG file and displays the image dimensions.
}
\usage{
loadPNG(fileIN=NULL, silent=FALSE)
}
\arguments{
  \item{fileIN}{ Input file name.}
  \item{silent}{ Logic, TRUE=do not display image dimensions.}
}
\value{
  png file object.
}
\author{Jose Gama}
\examples{
\dontrun{
loadPNG(paste(system.file(package='CVD'),'/inst/extdata/fruits.png',sep=''))
}
}
\keyword{programming}
