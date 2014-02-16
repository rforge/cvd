library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example identical to Fig. 1 from\nThe Desaturated Panel D-15 P.
Lanthony Documenta Ophthalmologica 46,1: 185-189, 1978')

data(LanthonyD15)
data(example1Lanthony1978)

.pardefault <- par(no.readonly = T)
#png('example1Lanthony1978.png')
par(mfrow=c(2,2),mar=c(1,1,3,1),oma=c(0, 0, 2, 0))
scoreD15Graphic(userD15values=example1Lanthony1978[,1], titleGraphic="Test standard")
scoreD15Graphic(userD15values=example1Lanthony1978[,2], titleGraphic="Test desaturated", 
okD15colors=sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B']))
scoreD15Graphic(userD15values=example1Lanthony1978[,3], titleGraphic="Test standard")
scoreD15Graphic(userD15values=example1Lanthony1978[,4], titleGraphic="Test desaturated", 
okD15colors=sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B']))
mtext('Simple Anomalous Trichromacy', side = 3, line = 0, outer = TRUE,cex=1.5)
mtext('Extreme Anomalous Trichromacy', side = 3, line = -20, outer = TRUE,cex=1.5)
#dev.off()
par(.pardefault)
