\name{lightAdaptedPupilSize.Barten}
\alias{lightAdaptedPupilSize.Barten}
\title{pupil diameter ranges from Barten, L. (1999)}
\description{\code{lightAdaptedPupilSize.Barten} computes the pupil diameter ranges from Barten, L. (1999).
}
\usage{
lightAdaptedPupilSize.Barten(L=NULL, a=NULL)
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
Barten, P. G. J. (1999). Contrast sensitivity of the human eye and its effects on image quality. Bellingham, WA: SPIE Optical Engineering Press.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2, field diameter = 30 degrees
\dontrun{lightAdaptedPupilSize.Barten(1,30^2)}
}
\keyword{programming}
