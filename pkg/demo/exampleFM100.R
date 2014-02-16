library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

gmessage('Example identical to proceedings of the New Zealand Generating fast automated reports 
for the Farnsworth-Munsell 100-hue colour vision test Ray Hidayat, Computer Science Research Student Conference 2008')

data(FarnsworthMunsell100Hue)
data(exampleFM100)

allFM100data <- data.matrix(exampleFM100)
fig3data <- allFM100data[1:4*3-2,]
fig3data <- c(t(fig3data))
fig3data <- fig3data[-which(is.na(fig3data))]
scoreFM100Graphic(userFM100values=fig3data)


