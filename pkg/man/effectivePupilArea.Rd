\name{effectivePupilArea}
\alias{effectivePupilArea}
\title{ Effective area of the illuminated pupil}
\description{\code{effectivePupilArea} computes the effective area of the illuminated pupil from its diameter.
}
\usage{
effectivePupilArea(d)
}
\arguments{
  \item{d}{ diameter in mm}
}
\value{
  \item{PupilSize}{ Pupil effective area in mm^2}
}
\author{Jose Gama}
\references{
#Smith, VC, Pokorny, J, and Yeh, T: The Farnsworth-Munsell 100-hue test in cone excitation space. Documenta Ophthalmologica Proceedings Series 56:281-291, 1993.
}
\examples{
# Pupil area in mm^2 for diameter = 2 mm
\dontrun{effectivePupilArea(2)}
}
\keyword{programming}
