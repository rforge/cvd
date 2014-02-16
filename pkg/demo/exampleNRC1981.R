library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example identical to Procedures for Testing Color Vision: Report of Working Group 41, 1981, Committee on Vision, National Research Council')

data(exampleNRC1981)

# this is an example of a typo in data from a publication
#the "monochromat" data has "16" instead of "6"
showDuplicated(exampleNRC1981[,3])
fixedData <- exampleNRC1981[,3]
fixedData[7] <- 6
# double check:
showDuplicated(fixedData) # OK!

.pardefault <- par(no.readonly = T)
#png('exampleNRC1981.png')
par(mfrow=c(1,3),mar=c(1,1,3,1),oma=c(0, 0, 2, 0))
scoreD15Graphic(userD15values=exampleNRC1981[,1], titleGraphic="Protanope")
scoreD15Graphic(userD15values=exampleNRC1981[,2], titleGraphic="Deuteranope")
scoreD15Graphic(userD15values=fixedData, titleGraphic="Achromat")
mtext('The Farnsworth Dichotomous Test', side = 3, line = 0, outer = TRUE,cex=1.5)
#dev.off()
par(.pardefault)

