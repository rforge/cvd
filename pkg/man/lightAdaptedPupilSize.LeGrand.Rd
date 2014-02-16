\name{lightAdaptedPupilSize.LeGrand}
\alias{lightAdaptedPupilSize.LeGrand}
\title{pupil diameter ranges from Le Grand (1992)}
\description{\code{lightAdaptedPupilSize.LeGrand} computes the pupil diameter ranges from Le Grand (1992).
}
\usage{
lightAdaptedPupilSize.LeGrand(L=NULL)
}
\arguments{
  \item{L}{ luminance in cd m^-2}
}
\value{
  \item{PupilSize}{ Pupil size in mm}
}
\author{Jose Gama}
\references{
Vision, Pierre A. Buser, Michel Imbert, MIT Press, 1992
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2
\dontrun{lightAdaptedPupilSize.LeGrand(1)}
}
\keyword{programming}
