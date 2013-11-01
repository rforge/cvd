\name{lightAdaptedPupilSize.DeGrootAndGebhard}
\alias{lightAdaptedPupilSize.DeGrootAndGebhard}
\title{pupil diameter ranges from DeGrootAndGebhard, L. (1952)}
\description{\code{lightAdaptedPupilSize.DeGrootAndGebhard} computes the pupil diameter ranges from DeGrootAndGebhard, L. (1952).
}
\usage{
lightAdaptedPupilSize.DeGrootAndGebhard(L=NULL)
}
\arguments{
  \item{L}{ luminance in cd m^-2}
}
\value{
  \item{PupilSize}{ Pupil size in mm}
}
\author{Jose Gama}
\references{
Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
De Groot, S. G., & Gebhard, J. W. (1952). Pupil size as determined by adapting luminance. Journal of the Optical Society of America A, 42(7), 492–495.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2
\dontrun{lightAdaptedPupilSize.DeGrootAndGebhard(1)}
}
\keyword{programming}
