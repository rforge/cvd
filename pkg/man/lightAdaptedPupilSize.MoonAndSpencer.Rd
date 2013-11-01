\name{lightAdaptedPupilSize.MoonAndSpencer}
\alias{lightAdaptedPupilSize.MoonAndSpencer}
\title{pupil diameter ranges from MoonAndSpencer, L. (1944)}
\description{\code{lightAdaptedPupilSize.MoonAndSpencer} computes the pupil diameter ranges from MoonAndSpencer, L. (1944).
}
\usage{
lightAdaptedPupilSize.MoonAndSpencer(L=NULL)
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
Moon, P., & Spencer, D. E. (1944). On the Stiles-Crawford effect. Journal of the Optical Society of
America, 34(6), 319–329, http://www.opticsinfobase. org/abstract.cfm?URI1⁄4josa-34-6-319.
}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2
\dontrun{lightAdaptedPupilSize.MoonAndSpencer(1)}
}
\keyword{programming}
