\name{lightAdaptedPupilSize.WatsonAndYellott}
\alias{lightAdaptedPupilSize.WatsonAndYellott}
\title{pupil diameter ranges from Watson A. B., Yellott J. I. (2012)}
\description{\code{lightAdaptedPupilSize.WatsonAndYellott} computes the pupil diameter ranges from Watson A. B., Yellott J. I. (2012).
}
\usage{
lightAdaptedPupilSize.WatsonAndYellott(L=NULL, a=NULL, y=NULL, y0=NULL, e=NULL)
}
\arguments{
  \item{L}{ luminance in cd m^-2}
  \item{a}{ area in deg^2}
  \item{y}{ age in years}
  \item{y0}{ reference age}
  \item{e}{ number of eyes (1 or 2)}
}
\value{
  \item{PupilSize}{ Pupil size in mm}
}
\author{Jose Gama}
\references{
Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2, field diameter = 30 degrees, age=45, estimated reference age = 28.58, eyes = 2
\dontrun{lightAdaptedPupilSize.WatsonAndYellott(1,30^2,45,28.58,2)}
}
\keyword{programming}
