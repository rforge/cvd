\name{VKSgraphic}
\alias{VKSgraphic}
\alias{VKSvariantGraphic}
\title{Graphical score for the D-15 tests}
\description{\code{VKSgraphic} computes a graphical score based on the Vingrys and King-Smith method (VKS) for the D-15 test or similar tests.
\code{VKSvariantGraphic} shows the angles with double their value, for a continuous display of the confusion axis.}
\usage{
VKSgraphic(VKSdata, xLimit=5, yLimit=4, VKStitle='', VKSxlabel='',VKSylabel='')
}
\arguments{
  \item{VKSdata}{ data.frame with color vision deficiency name, VKS angle and VKS index}
  \item{xLimit}{ X-axis boundaries}
  \item{yLimit}{ Y-axis boundaries}
  \item{VKStitle}{ title for the plot}
  \item{VKSxlabel}{ text for the x label}
  \item{VKSylabel}{ text for the y label}
}
\value{
  none
}
\source{
VKSvariantGraphic - original idea by David Bimler
Atchison DA, Bowman KJ, Vingrys AJ
Quantitave scoring methods for D15 panel tests in the diagnosis of congenital colour-vision deficiencies.
Optometry and Vision Science 1991, 68:41-48.
}
\references{
Atchison DA, Bowman KJ, Vingrys AJ
Quantitave scoring methods for D15 panel tests in the diagnosis of congenital colour-vision deficiencies.
Optometry and Vision Science 1991, 68:41-48.
}
\author{Jose Gama}
\examples{
# Creating similar graphics to "A Quantitative Scoring Technique For Panel Tests of Color Vision" Algis J. Vingrys and P. Ewen King-Smith
\dontrun{
VKSdata<-VKStable2[,c(1,3:5)]
VKSdata[1,1]<-'Normal no error'
VKSdata[2:9,1]<-'Normal'
VKSdata[10:13,1]<-'Acquired CVD'
# the graphics are similar but not identical because the data used in the plots is the average of the values instead of all the values
VKSgraphic(VKSdata[,1:3],5,4,'D-15 angle vs C-index (Average)','Angle','C-index') # Fig. 6
VKSgraphic(VKSdata[,c(1,2,4)],5,4,'D-15 angle vs S-index (Average)','Angle','S-index') # Fig. 7
}
}
\keyword{programming}
