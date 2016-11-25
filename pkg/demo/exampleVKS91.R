library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example similar to Vingrys, A.J. and King-Smith, P.E. (1988).
A quantitative scoring technique for panel tests of color vision.
Investigative Ophthalmology and Visual Science, 29, 50-63.')

data(VKStable2)

.pardefault <- par(no.readonly = T)
#png('exampleVKS91.png')
par(mfrow=c(1,2),mar=c(2,2,3,1),oma=c(0, 0, 2, 0))
VKSdata<-VKStable2[,c(1,3:5)]
VKSdata[1,1]<-'Normal no error'
VKSdata[2:9,1]<-'Normal'
VKSdata[10:13,1]<-'Acquired CVD'
# the graphics are similar but not identical because the data used in the plots is the average of the values instead of all the values
VKSgraphic(VKSdata[,1:3],5,4,'D-15 angle vs C-index (Average)','Angle','C-index') # Fig. 6
VKSgraphic(VKSdata[,c(1,2,4)],5,4,'D-15 angle vs S-index (Average)','Angle','S-index') # Fig. 7
mtext('The Vingrys and King-Smith score', side = 3, line = 0, outer = TRUE,cex=1.5)
#dev.off()
par(.pardefault)

