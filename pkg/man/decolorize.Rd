\name{decolorize}
\alias{decolorize}
\alias{decolorizeFile}
\title{Decolorize algorithm from Mark Grundland and Neil A. Dodgson}
\description{\code{decolorize} converts a color image to contrast enhanced greyscale algorithm from Mark Grundland and Neil A. Dodgson. The input is an array of RGB values and the output is an array with the greyscale values.
\code{decolorizeFile} sends the output to a file instead of returning an array}
\usage{
decolorize(fileIN=NULL,effect=0.5,scale=NULL,noise=0.001,recolor=FALSE)
}
\arguments{
  \item{fileIN}{ PNG file}
  \item{effect}{ how much the picture's achromatic content should be altered to accommodate the chromatic contrasts}
  \item{scale}{ in pixels is the typical size of relevant color contrast features}
  \item{noise}{ noise quantile indicates the amount of noise in the picture enabling the dynamic range of the tones to be appropriately scaled}
  \item{recolor}{ return also the chromatic content of the picture}
}
\value{
  \item{colorArray}{ array of RGB colors converted to contrast enhanced greyscale.}
}
\author{Jose Gama}
\references{
Mark Grundland and Neil A. Dodgson, "Decolorize: Fast, Contrast Enhancing, Color to Grayscale Conversion",
Pattern Recognition, vol. 40, no. 11, pp. 2891-2896, (2007).
http://www.Eyemaginary.com/Portfolio/Publications.html
}
\examples{
\dontrun{
samplePics <- c('fruits', 'pastel_color', 'sample1', 'TurnColorsGrayImage1', 'TurnColorsGrayImage2')
for (pics in samplePics)
{
fname<-paste(system.file(package='CVD'),'/extdata/',pics,'.png',sep='')
g1<-decolorize(fname)
png::writePNG(g1$tones, paste(pics, '.decolorize.png',sep=''))
}
}
}
\keyword{programming}
