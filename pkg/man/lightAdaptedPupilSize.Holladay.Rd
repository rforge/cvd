\name{lightAdaptedPupilSize.Holladay}
\alias{lightAdaptedPupilSize.Holladay}
\title{pupil diameter ranges from Holladay, L. (1926)}
\description{\code{lightAdaptedPupilSize.Holladay} computes the pupil diameter ranges from Holladay, L. (1926).
}
\usage{
lightAdaptedPupilSize.Holladay(L=NULL)
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
Holladay, L. (1926). The fundamentals of glare and visibility. Journal of the Optical Society of America, 12(4), 271–319.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2
\dontrun{lightAdaptedPupilSize.Holladay(1)}
}
\keyword{programming}
