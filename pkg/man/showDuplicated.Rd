\name{showDuplicated}
\alias{showDuplicated}
\title{Show missing and duplicated cap numbers}
\description{\code{showDuplicated} shows missing and duplicated cap numbers from D-15, D15d, FM-100 and similar tests.
}
\usage{
showDuplicated(cnum)
}
\arguments{
  \item{cnum}{ cap numbers}
}
\value{
  none
}
\author{Jose Gama}
\examples{
# find duplicate values
\dontrun{showDuplicated(1:15)
showDuplicated(c(1:4,8,5:14))
# this is an example of a typo in data from a publication
#Procedures for Testing Color Vision: Report of Working Group 41, 1981, Committee on Vision, National Research Council, pp. 107
#the "monochromat" data has "16" instead of "6"
data(exampleNRC1981)
showDuplicated(exampleNRC1981[,3])
}
}
\keyword{programming}
