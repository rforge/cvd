library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example identical to Farnsworth D. The Farnsworth Dichotomous Test for Color Blindness Panel D-15 Manual.
New York, The Psychological Corp., 1947, pp. 1-8.')

data(FarnsworthD15)
data(exampleFarnsworth1974)

.pardefault <- par(no.readonly = T)
#png('exampleFarnsworth1974.png')
par(mfrow=c(1,3),mar=c(1,1,3,1),oma=c(0, 0, 2, 0))
scoreD15Graphic(userD15values=exampleFarnsworth1974[,1], titleGraphic="Deuteranope")
scoreD15Graphic(userD15values=exampleFarnsworth1974[,2], titleGraphic="Protanope")
scoreD15Graphic(userD15values=exampleFarnsworth1974[,3], titleGraphic="Tritanope")
mtext('The Farnsworth Dichotomous Test', side = 3, line = 0, outer = TRUE,cex=1.5)
#dev.off()
par(.pardefault)