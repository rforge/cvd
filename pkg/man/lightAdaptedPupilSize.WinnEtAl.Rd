\name{lightAdaptedPupilSize.WinnEtAl}
\alias{lightAdaptedPupilSize.WinnEtAl}
\title{pupil diameter ranges from Winn et al (1995)}
\description{\code{lightAdaptedPupilSize.WinnEtAl} computes the pupil diameter ranges from Winn et al (1995).
}
\usage{
lightAdaptedPupilSize.WinnEtAl(L=NULL, y=NULL)
}
\arguments{
  \item{L}{ luminance in cd m^-2}
  \item{y}{ age in years}
}
\value{
  \item{PupilSize}{ Pupil size in mm}
}
\author{Jose Gama}
\references{
Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
Winn, B., Whitaker, D., Elliott, D. B., & Phillips, N. J. (1994). Factors affecting light-adapted pupil size in
normal human subjects. Investigative Ophthalmology & Visual Science, 35(3):1132–1137, http://www.iovs.org/content/35/3/1132. 
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2, age = 45 years
\dontrun{lightAdaptedPupilSize.WinnEtAl(1,45)}
}
\keyword{programming}
