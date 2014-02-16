library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example identical to Cone dystrophies Part 2 Cone dysfunction syndromes, Matthew P Simunovic')

data(exampleSimunovic2004)

.pardefault <- par(no.readonly = T)
#png('exampleSimunovic2004.png')
par(mfrow=c(1,2),mar=c(1,1,3,1),oma=c(0, 0, 2, 0))
scoreD15Graphic(userD15values=exampleSimunovic2004[,1], titleGraphic="Rod Monochromat")
scoreD15Graphic(userD15values=exampleSimunovic2004[,2], titleGraphic="Blue Cone Monochromat")
mtext('The Farnsworth Dichotomous Test', side = 3, line = 0, outer = TRUE,cex=1.5)
#dev.off()
par(.pardefault)

