\name{lightAdaptedPupilSize.Crawford}
\alias{lightAdaptedPupilSize.Crawford}
\title{pupil diameter ranges from Crawford, L. (1936)}
\description{\code{lightAdaptedPupilSize.Crawford} computes the pupil diameter ranges from Crawford, L. (1936).
}
\usage{
lightAdaptedPupilSize.Crawford(L=NULL)
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
Crawford, B. H. (1936). The dependence of pupil size upon external light stimulus under static and variable conditions. Proceedings of the Royal Society of London, Series B, Biological Sciences, 121(823), 376–395.}
\examples{
# Pupil diameter in mm for luminance = 1 cd m^-2
\dontrun{lightAdaptedPupilSize.Crawford(1)}
}
\keyword{programming}
