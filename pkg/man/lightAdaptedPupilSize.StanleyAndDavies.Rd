\name{lightAdaptedPupilSize.StanleyAndDavies}
\alias{lightAdaptedPupilSize.StanleyAndDavies}
\title{pupil diameter ranges from StanleyAndDavies, L. (1995)}
\description{\code{lightAdaptedPupilSize.StanleyAndDavies} computes the pupil diameter ranges from StanleyAndDavies, L. (1995).
}
\usage{
lightAdaptedPupilSize.StanleyAndDavies(L=NULL, a=NULL)
}
\arguments{
  \item{L}{ luminance in cd m^-2}
  \item{a}{ area in deg^2}
}
\value{
  \item{PupilSize}{ Pupil size in mm}
}
\author{Jose Gama}
\references{
Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
Stanley, P. A., & Davies, A. K. (1995). The effect of field of view size on steady-state pupil diameter. Ophthalmic & Physiological Optics, 15(6), 601–603.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2, field diameter = 30 degrees
\dontrun{lightAdaptedPupilSize.StanleyAndDavies(1,30^2)}
}
\keyword{programming}
