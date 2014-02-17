\name{calculateCircle}
\alias{calculateCircle}
\title{ Generate points from a circle}
\description{\code{calculateCircle} generates points from a circle 
with many options, equally spaced, randomly spaced, with noise added 
to the radius or limited to a segment of angle alpha.
}
\usage{
calculateCircle(x, y, r, steps=50,sector=c(0,360),randomDist=FALSE, 
randomFun=runif,...)
}
\arguments{
  \item{x}{ center point x}
  \item{y}{ center point y}
  \item{r}{ radius}
  \item{steps}{ number of points}
  \item{sector}{ limited circular sector}
  \item{randomDist}{ logical, TRUE = randomly spaced}
  \item{randomFun}{ random function}
  \item{...}{ optiomal parameters to pass to randomFun}
}
\value{
  \item{points}{ array n x 2 of point coordinates.}
}
\author{Jose Gama}
\examples{
\dontrun{
# 100 points from a circle at c(0,0) with radius=200
a<-calculateCircle(0,0,200,100)
plot(a[,1],a[,2],xlim=c(-200,200),ylim=c(-200,200))
par(new=TRUE)
# 12 points from a circle at c(0,0) with radius=190, points between 0 and 90
# degrees
a<-calculateCircle(0,0,190,12,c(0,90))
plot(a[,1],a[,2],xlim=c(-200,200),ylim=c(-200,200),col='red')
par(new=TRUE)
# 12 points from a circle at c(0,0) with radius=180, points between 0 and 180
# degrees, uniform random distribution
a<-calculateCircle(0,0,180,12,c(0,180),TRUE)
plot(a[,1],a[,2],xlim=c(-200,200),ylim=c(-200,200),col='green')
par(new=TRUE)
# 12 points from a circle at c(0,0) with radius=170, points between 0 and 180
# degrees, normal random distribution
a<-calculateCircle(0,0,170,12,c(0,180),TRUE,rnorm)
plot(a[,1],a[,2],xlim=c(-200,200),ylim=c(-200,200),col='blue')
}
}
\keyword{programming}
