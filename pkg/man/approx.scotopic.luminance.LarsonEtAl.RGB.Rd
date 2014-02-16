\name{approx.scotopic.luminance.LarsonEtAl.RGB}
\alias{approx.scotopic.luminance.LarsonEtAl.RGB}
\alias{approx.scotopic.luminance.LarsonEtAl.RGB.array}
\alias{approx.scotopic.luminance.LarsonEtAl.XYZ}
\alias{approx.scotopic.luminance.LarsonEtAl.XYZ.array}
\title{ Approximation of the scotopic luminance}
\description{\code{approx.scotopic.luminance.LarsonEtAl.RGB} approximates the scotopic luminance from RGB values.
\code{approx.scotopic.luminance.LarsonEtAl.XYZ} approximates the scotopic luminance from XYZ values.
}
\usage{
approx.scotopic.luminance.LarsonEtAl.XYZ(XYZmatrix)
approx.scotopic.luminance.LarsonEtAl.RGB(RGBmatrix)
}
\arguments{
  \item{XYZmatrix}{ matrix with XYZ values}
  \item{RGBmatrix}{ matrix with RGB values}
}
\value{
  approximated scotopic luminance
}
\source{
Larson, G. W., H. Rushmeier, and C. Piatko (1997, October - December).
A visibility matching tone reproduction operator for high dynamic range scenes.
IEEE Transactions on Visualization and Computer Graphics 3 (4), 291–306.
}
\references{
Larson, G. W., H. Rushmeier, and C. Piatko (1997, October - December).
A visibility matching tone reproduction operator for high dynamic range scenes.
IEEE Transactions on Visualization and Computer Graphics 3 (4), 291–306.
}
\author{Jose Gama}
\examples{
\dontrun{
samplePics <- c('fruits', 'pastel_color', 'sample1', 'TurnColorsGrayImage1', 'TurnColorsGrayImage2')
for (pics in samplePics)
{
fname<-paste(system.file(package='CVD'),'/extdata/',pics,'.png',sep='')
imgTest<-loadPNG(fname)
imgTest.array<-approx.scotopic.luminance.LarsonEtAl.RGB.array(imgTest)
png::writePNG(imgTest.array, paste(pics, '.approx.scotopic.luminance.LarsonEtAl.RGB.png',sep=''))
}
}
}
\keyword{programming}
