\name{effectiveCornealFluxDensity}
\alias{effectiveCornealFluxDensity}
\title{Effective Corneal Flux Density}
\description{\code{effectiveCornealFluxDensity} computes the effective Corneal Flux Density = product of luminance, area, and the monocular effect, F = Lae, from Watson A. B., Yellott J. I. (2012).
}
\usage{
effectiveCornealFluxDensity(L=NULL,a=NULL,e=NULL)
}
\arguments{
  \item{L}{ luminance in cd m^-2}
  \item{a}{ field area in deg^2}
  \item{e}{ number of eyes (1 or 2)}
}
\value{
  \item{PupilSize}{ effective Corneal Flux Density}
}
\author{Jose Gama}
\references{
Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
}
\examples{
# effective Corneal Flux Density, luminance in cd m^-2 = 1, field area in deg^2 = 30, number of eyes = 2
\dontrun{effectiveCornealFluxDensity(1,30^2,2)}
}
\keyword{programming}
