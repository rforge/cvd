\name{greyscale.avg}
\alias{greyscale.avg}
\alias{greyscale.Y}
\alias{greyscale.Linear}
\alias{greyscale.RMY}
\alias{greyscale.BT709}
\alias{greyscale.Luminosity}
\title{Greyscale algorithms}
\description{Common algorithms to convert color images to greyscale. The input is an array of RGB values and the output is an array with the greyscale values.
\code{greyscale.avg} Greyscale algorithm, convert to average RGB values.
\code{greyscale.Y} Greyscale algorithm YIQ/NTSC - RGB colors in a gamma 2.2 color space.
\code{greyscale.linear} Greyscale algorithm linear RGB colors
\code{greyscale.RMY} Greyscale algorithm RMY
\code{greyscale.BT709} Greyscale algorithm BT709
\code{greyscale.luminosity} Greyscale algorithm using luminosity
}
\usage{
greyscale.avg(colorArray)
}
\arguments{
  \item{colorArray}{ array of RGB colors.}
}
\value{
  \item{colorArray}{ array of RGB colors converted to greyscale.}
}
\author{Jose Gama}
\examples{
\dontrun{
samplePics <- c('fruits', 'pastel_color', 'sample1', 'TurnColorsGrayImage1', 'TurnColorsGrayImage2')
for (pics in samplePics)
{
fname<-paste(system.file(package='CVD'),'/extdata/',pics,'.png',sep='')
imgTest<-loadPNG(fname)
g1<-greyscale.avg(imgTest)
png::writePNG(g1, paste(pics, '.greyscale.avg.png',sep=''))
}

imgTest<-loadPNG(fname)
g1<-greyscale.avg(imgTest)
png::writePNG(g1, paste(pics, '.greyscale.avg.png',sep=''))
g1<-greyscale.BT709(imgTest)
png::writePNG(g1, paste(pics, '.BT709.png',sep=''))
g1<-greyscale.Linear(imgTest)
png::writePNG(g1, paste(pics, '.Linear.png',sep=''))
g1<-greyscale.Luminosity(imgTest)
png::writePNG(g1, paste(pics, '.Luminosity.png',sep=''))
g1<-greyscale.RMY(imgTest)
png::writePNG(g1, paste(pics, '.RMY.png',sep=''))
g1<-greyscale.Y(imgTest)
png::writePNG(g1, paste(pics, '.Y.png',sep=''))

}
}
\keyword{programming}
