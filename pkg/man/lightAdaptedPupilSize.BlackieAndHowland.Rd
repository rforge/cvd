\name{lightAdaptedPupilSize.BlackieAndHowland}
\alias{lightAdaptedPupilSize.BlackieAndHowland}
\title{pupil diameter ranges from Blackie, C. A., & Howland, H. C. (1999)}
\description{\code{lightAdaptedPupilSize.BlackieAndHowland} computes the pupil diameter ranges from Blackie, C. A., & Howland, H. C., (1999).
}
\usage{
lightAdaptedPupilSize.BlackieAndHowland(L=NULL)
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
Blackie, C. A., & Howland, H. C. (1999). An extension of an accommodation and convergence model of emmetropization to include the effects of illumination intensity. Ophthalmic and Physiological Optics, 19(2), 112–125.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2
\dontrun{lightAdaptedPupilSize.BlackieAndHowland(1)}
}
\keyword{programming}
