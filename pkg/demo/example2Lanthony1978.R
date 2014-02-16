library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example identical to Fig. 2 from\nThe Desaturated Panel D-15 P.
Lanthony Documenta Ophthalmologica 46,1: 185-189, 1978')

data(LanthonyD15)
data(example2Lanthony1978)

.pardefault <- par(no.readonly = T)
#png('example2Lanthony1978.png')
par(mfrow=c(3,2),mar=c(1,1,3,1),oma=c(0, 0, 2, 0))
scoreD15Graphic(userD15values=example2Lanthony1978[,1], titleGraphic="Test standard")
scoreD15Graphic(userD15values=example2Lanthony1978[,2], titleGraphic="Test desaturated", 
okD15colors=sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B']))
scoreD15Graphic(userD15values=example2Lanthony1978[,3], titleGraphic="Test standard")
scoreD15Graphic(userD15values=example2Lanthony1978[,4], titleGraphic="Test desaturated", 
okD15colors=sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B']))
scoreD15Graphic(userD15values=example2Lanthony1978[,5], titleGraphic="Test standard")
scoreD15Graphic(userD15values=example2Lanthony1978[,6], titleGraphic="Test desaturated", 
okD15colors=sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B']))
mtext('Central Serous Choroidopathy', side = 3, line = 0, outer = TRUE,cex=1.5)
mtext('Optic Neuritis', side = 3, line = -18, outer = TRUE,cex=1.5)
mtext('Autosomal Dominant Optic Atrophy', side = 3, line = -33, outer = TRUE,cex=1.5)
#dev.off()
par(.pardefault)
