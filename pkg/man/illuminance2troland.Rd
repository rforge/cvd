\name{illuminance2troland}
\alias{illuminance2troland}
\alias{luminance2troland}
\title{ Convert from luminance to troland and effective troland}
\description{\code{illuminance2troland} convert from illuminance (lux) to conventional retinal illuminance (troland) and effective troland (trolands per effective area).
\code{luminance2troland} convert from luminance (cd/m^2) to troland and effective troland.
}
\usage{
luminance2troland(Lv, d=NA)
illuminance2troland(Ev, lumFactor, d=NA)
}
\arguments{
  \item{d}{ diameter in mm}
  \item{Lv}{ luminance (cd/m^2)}
  \item{Ev}{ illuminance (lux)}
  \item{lumFactor}{ luminance factor}
}
\value{
  \item{troland}{ conventional retinal illuminance (troland)}
  \item{effectivetroland}{ effective troland (trolands per effective area)}
}
\author{Jose Gama}
\references{
#Smith, VC, Pokorny, J, and Yeh, T: The Farnsworth-Munsell 100-hue test in cone excitation space. Documenta Ophthalmologica Proceedings Series 56:281-291, 1993.
}
\examples{
# Pupil area in mm^2 for diameter = 2 mm
\dontrun{illuminance2troland(2)}
}
\keyword{programming}
