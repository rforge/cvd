\name{attenuationNumberOfEyes}
\alias{attenuationNumberOfEyes}
\title{Attenuation as a function of number of eyes}
\description{\code{attenuationNumberOfEyes} computes the attenuation as a function M(e) of number of eyes e (1 or 2), from Watson A. B., Yellott J. I. (2012).
}
\usage{
attenuationNumberOfEyes(e)
}
\arguments{
  \item{e}{ number of eyes (1 or 2)}
}
\value{
  \item{PupilSize}{ attenuation}
}
\author{Jose Gama}
\references{
Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
}
\examples{
\dontrun{
attenuationNumberOfEyes(1)
attenuationNumberOfEyes(2)
}
}
\keyword{programming}
