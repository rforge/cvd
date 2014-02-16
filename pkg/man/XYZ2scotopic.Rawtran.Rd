\name{XYZ2scotopic.Rawtran}
\alias{XYZ2scotopic.Rawtran}
\alias{XYZ2scotopic.Rawtran.array}
\title{ Approximation of the scotopic luminance}
\description{\code{XYZ2scotopic.Rawtran} approximates the scotopic luminance from XYZ values, illuminant D65, from Filip Hroch (1998).
Used in the astronomy software Rawtran.

\code{XYZ2scotopic.Rawtran.array} idem, however the data type used is array.
}
\usage{
XYZ2scotopic.Rawtran(XYZmatrix)
}
\arguments{
  \item{XYZmatrix}{ matrix (or array) with XYZ values}
}
\value{
  Matrix (or array) with approximated scotopic luminance.
}
\source{
Filip Hroch, 1998, Computer Programs for CCD Photometry,
20th Stellar Conference of the Czech and Slovak Astronomical Institutes,
DusekJ., http://adsabs.harvard.edu/abs/1998stel.conf...30H
Rawtran - integral.physics.muni.cz
Masaryk University
\url{http://integral.physics.muni.cz/rawtran/}
}
\references{
Filip Hroch, 1998, Computer Programs for CCD Photometry,
20th Stellar Conference of the Czech and Slovak Astronomical Institutes,
DusekJ., http://adsabs.harvard.edu/abs/1998stel.conf...30H
Rawtran - integral.physics.muni.cz
Masaryk University
\url{http://integral.physics.muni.cz/rawtran/}
}
\author{Jose Gama}
\keyword{programming}
