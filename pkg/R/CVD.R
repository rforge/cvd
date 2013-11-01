
scoreD15Graphic<-function(userD15colors=NULL,userD15values=NULL, titleGraphic="Farnsworth dichotomous test (D-15) results", okD15colors=NULL)
{# plots the graphic to score the Farnsworth dichotomous test (D-15) by default, or a similar test by modifying titleGraphic and okD15colors
circPos<-matrix(c(22,125,44,82,76,50,118,28,150,28,193,28,246,50,278,92,289,167,278,230,246,263,204,284,172,284,118,274,86,252,44,209),ncol=2,byrow=TRUE)
NumPos<-matrix(c(22,135,44,82-15,76,50-15,118,28-15,150,28-15,193,28-15,246,50-15,278+15,92,289+15,167,278+15,230,246,263+15,204,284+15,172,284+15,118,274+15,86,252+15,44,209+15),ncol=2,byrow=TRUE)
if ((is.null(userD15colors)) & (is.null(userD15values))) stop('Input either the colors chosen by the user for the D-15 test or the position values')
if (is.null(okD15colors)) {
okD15colors<-sprintf('#%02x%02x%02x',FarnsworthD15[-1,'R'],FarnsworthD15[-1,'G'],FarnsworthD15[-1,'B'])#c('#3583B4', '#3B84A7', '#39859C', '#3B8690', '#3F8782', '#588473', '#6C8164', '#837B5D', '#907660', '#9E6E6F', '#9F6D7C', '#9C6D89', '#927099', '#8F6FA4','#8073B2')
}
if (is.null(userD15colors)) {
pos2<-userD15values
} else {
pos2<-c()
for (n in 1:15) pos2<-c(pos2,which(userD15colors[n] == okD15colors) )
}
pos2<-c(1,pos2+1)
circPos2<-circPos[pos2,] 
symbols(circPos[,1], circPos[,2], circles =rep(5,16) , inches = FALSE, xlim = c(1,300), ylim = c(300,1), xaxt='n', yaxt='n', ann=FALSE)
par(new=T)
title(main =titleGraphic)
par(new=T)
plot(circPos2[,1], circPos2[,2], xlim = c(1,300), ylim = c(300,1),type='l', xaxt='n', yaxt='n', ann=FALSE)
par(new=T)
lines(c(96,138),c(27,267),type = 'l', lty=2,col='red')
par(new=T)
lines(c(133,102),c(14,290),type = 'l', lty=2,col='green')
par(new=T)
lines(c(58,293),c(210,131),type = 'l', lty=2,col='blue')
par(new=T)
text(NumPos[,1], NumPos[,2], c('Reference',1:15))
par(new=T)
text(195,145,"Tritan", srt=20) 
par(new=T)
text(94,80,"P\nr\no\nt\na\nn") 
par(new=T)
text(137,83,"D\ne\nu\nt\na\nn") 
}

scoreD15TCDS<-function(userD15colors=NULL,userD15values=NULL)
{# Compute the Total Color Difference Score (TCDS) for the D-15 colors or for their positions
# Bowman's (1982) Total Color Difference Score (TCDS) for congenitally defective observers on the D-15 with enlarged tests.
# K.J. Bowman, A method for quantitative scoring of the Farnsworth Panel D-15, Acta Ophthalmologica, 60 (1982), pp. 907–916
# userD15colors RGB colors chosen by tester
# userD15values position values chosen by tester
if ((is.null(userD15colors)) & (is.null(userD15values))) stop('Input either the colors chosen by the user for the D-15 test or the position values')
if (is.null(userD15colors)) {
pos2<-userD15values
} else {
lColorsOK<-sprintf('#%02x%02x%02x',FarnsworthD15[-1,'R'],FarnsworthD15[-1,'G'],FarnsworthD15[-1,'B'])#c('#3583B4', '#3B84A7', '#39859C', '#3B8690', '#3F8782', '#588473', '#6C8164', '#837B5D', '#907660', '#9E6E6F', '#9F6D7C', '#9C6D89', '#927099', '#8F6FA4','#8073B2')
pos2<-c()
for (n in 1:15) pos2<-c(pos2,which(userD15colors[n] == lColorsOK) )
}


posRow<-c(1,pos2+1)
posRow<-posRow[1:15]
posColumn<- pos2+1
posValue<-c()
for (n in 1:15) {
posValue<-c(posValue,BowmanTCDS[posRow[n],posColumn[n]])
posValueNormal<-c(n,BowmanTCDS[posRow[n],posColumn[n]])
}
TCDS<-sum(posValue)# TCDS
#  Color Confusion Index (CCI = TCDSactual / TCDSnormal)
CCI<-TCDS/sum(posValueNormal)
c(TCDS=TCDS,CCI=CCI)


}


lightAdaptedPupilSize.Holladay<-function(L=NULL){
# pupil diameter ranges, formula "for a probable average healthy young eye" - Holladay, L. (1926)
# L=luminance in cd m^-2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Holladay, L. (1926). The fundamentals of glare and visibility. Journal of the Optical Society of America, 12(4), 271–319.
if (is.null(L)) stop('Please enter the luminance in candels per square meter (cd m^-2)')
if (is.na(L)) stop('Please enter a numeric value for the luminance')
if (!is.numeric(L)) stop('Please enter a numeric value for the luminance')
return(7*exp(-0.1007*L^0.4))
}

lightAdaptedPupilSize.Crawford<-function(L=NULL){
# pupil diameter ranges - Crawford 1936
# L=luminance in cd m^-2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Crawford, B. H. (1936). The dependence of pupil size upon external light stimulus under static and variable conditions. Proceedings of the Royal
# Society of London, Series B, Biological Sciences, 121(823), 376–395.
5-2.2*tanh(0.61151+0.447*log(L))
}

lightAdaptedPupilSize.MoonAndSpencer<-function(L=NULL){
# pupil diameter ranges Moon and Spencer 1944
# L=luminance in cd m^-2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Moon, P., & Spencer, D. E. (1944). On the Stiles-Crawford effect. Journal of the Optical Society of
# America, 34(6), 319–329, http://www.opticsinfobase. org/abstract.cfm?URI1⁄4josa-34-6-319.
4.9-3*tanh(0.4*log(L)-0.00114)
}

lightAdaptedPupilSize.DeGrootAndGebhard<-function(L=NULL){
# pupil diameter ranges De Groot and Gebhard 1952
# L=luminance in cd m^-2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# De Groot, S. G., & Gebhard, J. W. (1952). Pupil size as determined by adapting luminance. Journal of the Optical Society of America A, 42(7), 492–495.
7.175*exp(-0.00092*(7.597+log(L))^3)
}

lightAdaptedPupilSize.StanleyAndDavies<-function(L=NULL, a=NULL){
# pupil diameter ranges Stanley and Davies 1995
# L=luminance in cd m^-2, a = area in deg^2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Stanley, P. A., & Davies, A. K. (1995). The effect of field of view size on steady-state pupil diameter. Ophthalmic & Physiological Optics, 15(6), 601–603.
7.75 - 5.75*((L*a/846)^0.41/((L*a/846)^0.41+2))
}

lightAdaptedPupilSize.Barten<-function(L=NULL, a=NULL){
# pupil diameter ranges Barten 1999
# L=luminance in cd m^-2, a = area in deg^2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Barten, P. G. J. (1999). Contrast sensitivity of the human eye and its effects on image quality. Bellingham, WA: SPIE Optical Engineering Press.
5-3*tanh(0.4*log(L*a/40^2))
}

lightAdaptedPupilSize.BlackieAndHowland<-function(L=NULL){
# pupil diameter ranges Blackie and Howland 1999
# L=luminance in cd m^-2
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Blackie, C. A., & Howland, H. C. (1999). An extension of an accommodation and convergence model of emmetropization to include the effects of illumination intensity. Ophthalmic and Physiological Optics, 19(2), 112–125.
5.697 - 0.658* log(L) + 0.07*(log(L))^2
}

lightAdaptedPupilSize.WinnEtAl<-function(L=NULL, y=NULL){
# pupil diameter ranges Winn, Whitaker, Elliott, and Phillips 1994
# L=luminance in cd m^-2, age y in years
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
# Winn, B., Whitaker, D., Elliott, D. B., & Phillips, N. J. (1994). Factors affecting light-adapted pupil size in
# normal human subjects. Investigative Ophthalmology & Visual Science, 35(3):1132–1137, http://www.iovs.org/content/35/3/1132. 
s <- c( -0.024501, -0.0368073, 0.0210892, 0.00281557)
b <- c( 6.9039, 2.7765, -1.909, 0.25599)
(sum(s* (log(min(4400,max(9,L))))^(0:3) ))*y+sum(b* (log(min(4400,max(9,L))))^(0:3) )
}

attenuationNumberOfEyes<-function(e) ifelse(e==1,0.1,ifelse(e==2,1,0))
# attenuation as a function M(e) of number of eyes e (1 or 2)
# M(1) = 0.1, M(2) = 1, otherwise 0
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.

effectiveCornealFluxDensity<-function(L=NULL,a=NULL,e=NULL){
# effective Corneal Flux Density = product of luminance, area, and the monocular effect, F = Lae
# L=luminance, a = field area in deg^2, e = number of eyes (1 or 2)
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
L*a*attenuationNumberOfEyes(e)
}

lightAdaptedPupilSize.WatsonAndYellott<-function(L=NULL, a=NULL, y=NULL, y0=NULL, e=NULL){
# pupil diameter ranges Watson & Yellott 2012
# L=luminance in cd m^-2, a = field area in deg^2, y = age in years, y0 = reference age, e = number of eyes (1 or 2)
# Watson A. B., Yellott J. I. (2012). A unified formula for light-adapted pupil size. Journal of Vision, 12(10):12, 1–16. http://journalofvision.org/12/10/12/, doi:10.1167/5.9.6.
F <- effectiveCornealFluxDensity(L,a,e)
Dsd <- lightAdaptedPupilSize.StanleyAndDavies(F,1)
Dsd+(y-y0)*(0.02132 - 0.009562*Dsd)
}

greyscale.avg<-function(colorArray){# average RGB values - grayscale algorithm
p3<-apply(colorArray[,,-4],1:2,mean)
p3<-array(p3,dim(colorArray))
if (dim(colorArray)[3]>3) p3[,,4]<-colorArray[,,4]
p3
}

greyscale.Y<-function(colorArray){# YIQ/NTSC - RGB colors in a gamma 2.2 color space - grayscale algorithm
colorArray[,,1]<-colorArray[,,1]*0.299
colorArray[,,2]<-colorArray[,,2]*0.587
colorArray[,,3]<-colorArray[,,3]*0.114
p3<-apply(colorArray[,,-4],1:2,sum)
p3<-array(p3,dim(colorArray))
if (dim(colorArray)[3]>3) p3[,,4]<-colorArray[,,4]
p3
}

greyscale.Linear<-function(colorArray){# linear RGB colors - grayscale algorithm
colorArray[,,1]<-colorArray[,,1]*0.3086
colorArray[,,2]<-colorArray[,,2]*0.6094
colorArray[,,3]<-colorArray[,,3]*0.0820
p3<-apply(colorArray[,,-4],1:2,sum)
p3<-array(p3,dim(colorArray))
if (dim(colorArray)[3]>3) p3[,,4]<-colorArray[,,4]
p3
}

greyscale.RMY<-function(colorArray){# RMY - grayscale algorithm
colorArray[,,1]<-colorArray[,,1]*0.5
colorArray[,,2]<-colorArray[,,2]*0.419
colorArray[,,3]<-colorArray[,,3]*0.081
p3<-apply(colorArray[,,-4],1:2,sum)
p3<-array(p3,dim(colorArray))
if (dim(colorArray)[3]>3) p3[,,4]<-colorArray[,,4]
p3
}

greyscale.BT709<-function(colorArray){# BT709 - grayscale algorithm
colorArray[,,1]<-colorArray[,,1]*0.2125
colorArray[,,2]<-colorArray[,,2]*0.7154
colorArray[,,3]<-colorArray[,,3]*0.0721
p3<-apply(colorArray[,,-4],1:2,sum)
p3<-array(p3,dim(colorArray))
if (dim(colorArray)[3]>3) p3[,,4]<-colorArray[,,4]
p3
}

greyscale.Luminosity<-function(colorArray){# Luminosity - grayscale algorithm
p3<-apply(colorArray[,,-4],1:2,function(x) (max(x)+min(x))/2)
p3<-array(p3,dim(colorArray))
if (dim(colorArray)[3]>3) p3[,,4]<-colorArray[,,4]
p3
}

RGBtoHSL<-function(R,G,B){
# RGB to HSL function to assist Color.Vision.c2g
R2 = R/255
G2 = G/255
B2 = B/255
if (length(R)==1 & length(G)==1 & length(B)==1){
Cmax = max(R2, G2, B2)
Cmin = min(R2, G2, B2)
D = Cmax - Cmin
if (Cmax==R2) H<-60*(((G2-B2)/D) %% 6)
if (Cmax==G2) H<-60*((B2-R2)/D+2)
if (Cmax==B2) H<-60*((R2-G2)/D+4)
L<-(Cmax+Cmin)/2
if (D==0) S<-0 else S<-D/(1-abs(2*L-1))
} else {
Tmp<-cbind(R2,G2,B2)
Cmax = apply(Tmp,1,max)
Cmin = apply(Tmp,1,min)
D = Cmax - Cmin
H<-S<-rep(0,length(R))
w<-which(Cmax==R2)
H[w]<-60*(((G2[w]-B2[w])/D[w]) %% 6)
w<-which(Cmax==G2)
H[w]<-60*((B2[w]-R2[w])/D[w]+2)
w<-which(Cmax==B2)
H[w]<-60*((R2[w]-G2[w])/D[w]+4)
L<-(Cmax+Cmin)/2
w<-which(D!=0)
S[w]<-D[w]/(1-abs(2*L[w]-1))
}
list(H=H,S=S,L=L)
}

Color.Vision.c2g<-function(fileIN=NULL, fileOUT=NULL, CorrectBrightness=FALSE){
#Color Image to Grayscale Conversion
#Original work by Martin Faust 2008
#http://www.e56.de/c2g.php
#Translated to R by Jose Gama 2013
#fileIN='ars_puzzlefighter_normal_vision.png'; fileOUT='ars_puzzlefighter_c2g.png';CorrectBrightness=FALSE
if (is.null(fileIN)) stop('A file input must be defined')
if (!file.exists(fileIN)) stop('Error! File does not exist' )
if (is.null(fileOUT)) stop('A file output must be defined')
if (is.null(CorrectBrightness)) stop('CorrectBrightness must be defined')
if (!is.logical(CorrectBrightness)) stop('CorrectBrightness must be logical')
p<-png::readPNG(fileIN)
p<-p*255
r<-c(p[,,1])
g<-c(p[,,2])
b<-c(p[,,3])
grayMat<-rep(0,length(r))
h2<-RGBtoHSL(r,g,b)
HSVmat<-rbind(h=h2$H,s=h2$S,l=h2$L)
wSat0<-which(HSVmat['s',] == 0.0)
grayMat[wSat0] = 1.5 * HSVmat['l',wSat0]
if (length(wSat0)==0) grayMat = HSVmat['l',] + HSVmat['l',] * HSVmat['s',] else grayMat[-wSat0] = HSVmat['l',-wSat0] + HSVmat['l',-wSat0] * HSVmat['s',-wSat0]
minC = min(grayMat)
maxC = max(grayMat)
meanC = mean(grayMat)
cat('Grey values: min',minC,', mean',meanC,', max',maxC)
minC = 0.0; maxC = (meanC + maxC) * 0.5
if (CorrectBrightness){
HSVmat['l',] <- 0.9 * (grayMat - minC) / (maxC - minC)
} else {
HSVmat['l',] <- (grayMat - minC) / (maxC - minC)
}
HSVmat['l',which(HSVmat['l',] > 1.0)] <- 1.0
HSVmat['l',which(HSVmat['l',] < 0.0)] <- 0.0
p2<-HSVmat['l',]
p3<-p[,,-4]
p3[,,1]<-array(t(p2),dim(p)[1:2])
p3[,,2]<-p3[,,1]
p3[,,3]<-p3[,,1]
png::writePNG(p3, fileOUT)
}

Color.Vision.Daltonize<-function(fileIN=NULL, fileOUT=NULL, myoptions=NULL, amount=1.0){
# converts images so that the most problematic colors are more visible to people with CVD
# Based on:
# Michael Deal Daltonize.org http://mudcu.be/labs/Color/Vision http://www.daltonize.org/p/about.html
#"Analysis of Color Blindness" by Onur Fidaner, Poliang Lin and Nevran Ozguven.
#http://scien.stanford.edu/class/psych221/projects/05/ofidaner/project_report.pdf
#"Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats" by Francoise Vienot, Hans Brettel and John D. Mollon
#http://vision.psychol.cam.ac.uk/jdmollon/papers/colourmaps.pdf
if (is.null(fileIN)) stop('A file input must be defined')
if (!file.exists(fileIN)) stop('Error! File does not exist' )
if (is.null(fileOUT)) stop('A file output must be defined')
if (is.null(myoptions)) stop('Options must be defined')
if ((amount>1.0)|(amount<0.0)) stop('Amount must be between 0.0 and 1.0')
if (!is.character(myoptions)) stop('Options must be "Protanope","Deuteranope" or "Tritanope"')
if (!(myoptions %in% c("Protanope","Deuteranope", "Tritanope"))) stop('Wrong option, must be: Protanope, Deuteranope or Tritanope.')

CVDMatrix <- array(c(0.000000,0.000000,0.000000,2.023440,1.000000,0.000000,-2.525810,
0.000000,1.000000,1.000000,0.494207,0.000000,0.000000,0.000000,
0.000000,0.000000,1.248270,1.000000,1.000000,0.000000,-0.395913,
0.000000,1.000000,0.801109,0.000000,0.000000,0.000000),c(3,3,3))

#Protanope, Deuteranope, Tritanope
p<-png::readPNG(fileIN)
r<-p[,,1]
g<-p[,,2]
b<-p[,,3]
x<-which(c("Protanope","Deuteranope", "Tritanope")==myoptions)
cvd_a = CVDMatrix[1,1,x]
cvd_b = CVDMatrix[1,2,x]
cvd_c = CVDMatrix[1,3,x]
cvd_d = CVDMatrix[2,1,x]
cvd_e = CVDMatrix[2,2,x]
cvd_f = CVDMatrix[2,3,x]
cvd_g = CVDMatrix[3,1,x]
cvd_h = CVDMatrix[3,2,x]
cvd_i = CVDMatrix[3,3,x]
L = (17.8824 * r) + (43.5161 * g) + (4.11935 * b)
M = (3.45565 * r) + (27.1554 * g) + (3.86714 * b)
S = (0.0299566 * r) + (0.184309 * g) + (1.46709 * b)
l = (cvd_a * L) + (cvd_b * M) + (cvd_c * S)
m = (cvd_d * L) + (cvd_e * M) + (cvd_f * S)
s = (cvd_g * L) + (cvd_h * M) + (cvd_i * S)
R = (0.0809444479 * l) + (-0.130504409 * m) + (0.116721066 * s)
G = (-0.0102485335 * l) + (0.0540193266 * m) + (-0.113614708 * s)
B = (-0.000365296938 * l) + (-0.00412161469 * m) + (0.693511405 * s)
R = r - R
G = g - G
B = b - B
RR = (0.0 * R) + (0.0 * G) + (0.0 * B)
GG = (0.7 * R) + (1.0 * G) + (0.0 * B)
BB = (0.7 * R) + (0.0 * G) + (1.0 * B)
R = RR + r
G = GG + g
B = BB + b
# Return values
R[which(R>1.0)]<-1.0
R[which(R<0.0)]<-0.0
G[which(G>1.0)]<-1.0
G[which(G<0.0)]<-0.0
B[which(B>1.0)]<-1.0
B[which(B<0.0)]<-0.0
p[,,1]<-R
p[,,2]<-G
p[,,3]<-B
png::writePNG(p, fileOUT)
}

Color.Vision.Simulate<-function(fileIN=NULL, fileOUT=NULL, myoptions=NULL, amount=1.0){
# converts images so that the colors look similar to how they are seen by people with CVD
# Based on:
# Michael Deal Daltonize.org http://mudcu.be/labs/Color/Vision http://www.daltonize.org/p/about.html
#"Analysis of Color Blindness" by Onur Fidaner, Poliang Lin and Nevran Ozguven.
#http://scien.stanford.edu/class/psych221/projects/05/ofidaner/project_report.pdf
#"Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats" by Francoise Vienot, Hans Brettel and John D. Mollon
#http://vision.psychol.cam.ac.uk/jdmollon/papers/colourmaps.pdf
if (is.null(fileIN)) stop('A file input must be defined')
if (!file.exists(fileIN)) stop('Error! File does not exist' )
if (is.null(fileOUT)) stop('A file output must be defined')
if (is.null(myoptions)) stop('Options must be defined')
if ((amount>1.0)|(amount<0.0)) stop('Amount must be between 0.0 and 1.0')
ConfusionLines <- data.frame(name=c("Protanope","Deuteranope","Tritanope","Achromatope"),x=c(0.7465,1.4,0.1748,0), y=c(0.2535,-0.4,0.0,0),
m=c(1.273463,0.968437,0.062921,0), yint=c(-0.073894,0.003331,0.292119,0))
if (is.character(myoptions)) if (!(myoptions %in% as.vector(ConfusionLines[["name"]]))) stop('Wrong option, must be: Protanope, Deuteranope, Tritanope or Achromatope.')

library('png', character.only=TRUE)
if (is.character(myoptions))
{
if (myoptions %in% as.vector(ConfusionLines[["name"]])) {
if (myoptions=="Achromatope")
{
p<-png::readPNG(fileIN)
numrows<-dim(p)[1]
numcols<-dim(p)[2]
p0=0.212656*p[,,1] + 0.715158*p[,,2] + 0.072186*p[,,3]
p0=p0*amount
a0<-1.0 - amount
p[,,1]<-p[,,1] * a0 + p0
p[,,2]<-p[,,2] * a0 + p0
p[,,3]<-p[,,3] * a0 + p0
png::writePNG(p, fileOUT)
return()
}
if (myoptions %in% as.vector(ConfusionLines[["name"]]))
{
tmp<-ConfusionLines[which(ConfusionLines[["name"]]==myoptions),]
confuse_x<-tmp[["x"]]
confuse_y<-tmp[["y"]]
confuse_m<-tmp[["m"]]
confuse_yint<-tmp[["yint"]]
opName<-myoptions
}
}
} else {
if (!is.numeric(myoptions)) stop('Options must be "Protanope","Deuteranope","Tritanope","Achromatope" or a numeric vector with 4 custom parameters for the confusion lines')
if (is.numeric(myoptions)) if(length(myoptions)!=4) stop('Options must be a numeric vector with 4 parameters')
if (is.numeric(myoptions)) if(length(myoptions)==4)
{ confuse_x<-myoptions[1];confuse_y<-myoptions[2];confuse_m<-myoptions[3];confuse_yint<-myoptions[4];opName<-'Custom' }
}
#Simulate: Protanope, Deuteranope, or Tritanope
p<-png::readPNG(fileIN)
sr<-p[,,1]
sg<-p[,,2]
sb<-p[,,3]
dr = sr # destination-pixel
dg = sg
db = sb
# Convert source color into XYZ color space
pow_r = sr^2.2
pow_g = sg^2.2
pow_b = sb^2.2
X = pow_r * 0.412424 + pow_g * 0.357579 + pow_b * 0.180464 # RGB->XYZ (sRGB:D65)
Y = pow_r * 0.212656 + pow_g * 0.715158 + pow_b * 0.0721856
Z = pow_r * 0.0193324 + pow_g * 0.119193 + pow_b * 0.950444
# Convert XYZ into xyY Chromacity Coordinates (xy) and Luminance (Y)
chroma_x = X / (X + Y + Z)
chroma_y = Y / (X + Y + Z)
chroma_x[which(is.na(chroma_x))]<-0#X[which(is.na(chroma_x))]
chroma_y[which(is.na(chroma_y))]<-0#Y[which(is.na(chroma_y))]
# Generate the “Confusion Line" between the source color and the Confusion Point
m = (chroma_y - confuse_y) / (chroma_x - confuse_x) # slope of Confusion Line
yint = chroma_y - chroma_x * m # y-intercept of confusion line (x-intercept = 0.0)
# How far the xy coords deviate from the simulation
deviate_x = (confuse_yint - yint) / (m - confuse_m)
deviate_y = (m * deviate_x) + yint
# Compute the simulated color’s XYZ coords
X = deviate_x * Y / deviate_y
Z = (1.0 - (deviate_x + deviate_y)) * Y / deviate_y
# Neutral grey calculated from luminance (in D65)
neutral_X = 0.312713 * Y / 0.329016 
neutral_Z = 0.358271 * Y / 0.329016 
# Difference between simulated color and neutral grey
diff_X = neutral_X - X
diff_Z = neutral_Z - Z
diff_r = diff_X * 3.24071 + diff_Z * -0.498571 # XYZ->RGB (sRGB:D65)
diff_g = diff_X * -0.969258 + diff_Z * 0.0415557
diff_b = diff_X * 0.0556352 + diff_Z * 1.05707
# Convert to RGB color space
dr = X * 3.24071 + Y * -1.53726 + Z * -0.498571 # XYZ->RGB (sRGB:D65)
dg = X * -0.969258 + Y * 1.87599 + Z * 0.0415557
db = X * 0.0556352 + Y * -0.203996 + Z * 1.05707
# Compensate simulated color towards a neutral fit in RGB space
fit_r2 = (ifelse(dr < 0.0, 0.0,1.0) - dr) / diff_r
fit_g2 = (ifelse(dg < 0.0,0.0,1.0) - dg) / diff_g
fit_b2 = (ifelse(db < 0.0,0.0,1.0) - db) / diff_b
fit_r=fit_r2
fit_g=fit_g2
fit_b=fit_b2
fit_r[which(is.infinite(fit_r2))]<-(ifelse(dr[which(is.infinite(fit_r2))] < 0.0, 0.0,1.0) - dr[which(is.infinite(fit_r2))])
fit_g[which(is.infinite(fit_g2))]<-(ifelse(dg[which(is.infinite(fit_g2))] < 0.0, 0.0,1.0) - dg[which(is.infinite(fit_g2))])
fit_b[which(is.infinite(fit_b2))]<-(ifelse(db[which(is.infinite(fit_b2))] < 0.0, 0.0,1.0) - db[which(is.infinite(fit_b2))])
fit_r2 = fit_r
fit_g2 = fit_g
fit_b2 = fit_b
fit_r2[which((fit_r > 1.0) | (fit_r < 0.0))]<-0.0
fit_g2[which((fit_g > 1.0) | (fit_g < 0.0))]<-0.0
fit_b2[which((fit_b > 1.0) | (fit_b < 0.0))]<-0.0
# highest value
adjust2 = fit_r2 - fit_g2
w<-which(adjust2<0)
adjust2[w]<-fit_g2[w]
adjust2[-w]<-fit_r2[-w]
adjust = adjust2 - fit_b2
w<-which(adjust<0)
adjust[w]<-fit_b2[w]
adjust[-w]<-adjust2[-w]
# Shift proportional to the greatest shift
dr = dr + (adjust * diff_r)
dg = dg + (adjust * diff_g)
db = db + (adjust * diff_b)
# Apply gamma correction
dr = dr^(1.0 / 2.2)
dg = dg^(1.0 / 2.2)
db = db^(1.0 / 2.2)
# Anomylize colors
dr = sr * (1.0 - amount) + dr * amount
dg = sg * (1.0 - amount) + dg * amount
db = sb * (1.0 - amount) + db * amount
# Return values
dr[which(dr>1.0)]<-1.0
dr[which(dr<0.0)]<-0.0
dg[which(dg>1.0)]<-1.0
dg[which(dg<0.0)]<-0.0
db[which(db>1.0)]<-1.0
db[which(db<0.0)]<-0.0
p[,,1]<-(dr)
p[,,2]<-(dg)
p[,,3]<-(db)
png::writePNG(p, fileOUT)
}

Color.Vision.VingrysAndKingSmith <-function(capnumbers=NULL,testType='D-15',silent=TRUE){
#method of scoring the results of the "D-15", "D-15DS" or "FM1OO-Hue" tests
#translated to R by Jose Gama 2013
#Implementation of the Vingrys and King-Smith method (1988)
#Vingrys, A.J. and King-Smith, P.E. (1988).
#A quantitative scoring technique for panel tests of color vision.
#Investigative Ophthalmology and Visual Science, 29, 50-63.
if (is.null(capnumbers)) stop('capnumbers must be defined')
if (!is.numeric(capnumbers)) stop('capnumbers must be numeric')
tType<-which(testType==c('D-15', 'D-15DS', 'FM1OO-Hue'))
if (length(testType)==0) stop('testType must be "D-15", "D-15DS" or "FM1OO-Hue"')
if (any(trunc(capnumbers)!=capnumbers)) stop('capnumbers must be integers')
if (testType %in% c('D-15', 'D-15DS')) 
{
if (length(capnumbers) != 15) stop('capnumbers must be a vector of 15 elements for D-15')
if (!all(sort(capnumbers) == 1:15)) stop('capnumbers must be between 1 and 15, without repetition')
}
if (testType == 'FM1OO-Hue') 
{
if (length(capnumbers) != 85) stop('capnumbers must be a vector of 85 elements for FM1OO-Hue')
if (!all(sort(capnumbers) == 1:85)) stop('capnumbers must be between 1 and 85, without repetition')
}
dataVKS<-list(
standardD15=matrix(c(-21.54, -38.39,-23.26,-25.56, -22.41,-15.53, -23.11,-7.45,-22.45,1.10, -21.67,7.35, -14.08,18.74,
-2.72,28.13, 14.84,31.13, 23.87,26.35,31.82,14.76, 31.42,6.99, 29.79,0.10,26.64,-9.38, 22.92,-18.65, 11.20,-24.61),16,2,byrow=T)
,
desaturatedD15=matrix(c(-4.77,-16.63,-8.63,-14.65, -12.08,-11.94, -12.86,-6.74,-12.26,-2.67, -11.18,2.01, -7.02,9.12,
1.30,15.78, 9.90,16.46, 15.03,12.05,15.48,2.56, 14.76,-2.24, 13.56,-5.04,11.06,-9.17, 8.95,-12.39, 5.62,-15.20),16,2,byrow=T)
,
FM100HUE=matrix(c(43.57,4.76,43.18,8.03, 44.37,11.34, 44.07,13.62, 44.95,16.04, 44.11,18.52,
42.92,20.64, 42.02,22.49, 42.28,25.15, 40.96,27.78, 37.68,29.55,
37.11,32.95, 35.41,35.94, 33.38,38.03, 30.88,39.59, 28.99,43.07,
25.00,44.12, 22.87,46.44, 18.86,45.87, 15.47,44.97, 13.01,42.12,
10.91,42.85, 8.49,41.35, 3.11,41.70, .68,39.23, -1.70,39.23,
-4.14,36.66, -6.57,32.41, -8.53,33.19, -10.98,31.47, -15.07,27.89,
-17.13,26.31, -19.39,23.82, -21.93,22.52, -23.40,20.14, -25.32,17.76,
-25.10,13.29, -26.58,11.87, -27.35,9.52, -28.41,7.26, -29.54,5.10,
-30.37,2.63, -31.07,0.10, -31.72,-2.42, -31.44,-5.13, -32.26,-8.16,
-29.86,-9.51, -31.13,-10.59, -31.04,-14.30, -29.10,-17.32, -29.67,-19.59,
-28.61,-22.65, -27.76,-26.66, -26.31,-29.24, -23.16,-31.24, -21.31,-32.92,
-19.15,-33.17, -16.00,-34.90, -14.10,-35.21, -12.47,-35.84, -10.55,-37.74,
-8.49,-34.78, -7.21,-35.44, -5.16,-37.08, -3.00,-35.95, -.31,-33.94,
1.55,-34.50, 3.68,-30.63, 5.88,-31.18, 8.46,-29.46, 9.75,-29.46,
12.24,-27.35, 15.61,-25.68, 19.63,-24.79, 21.20,-22.83, 25.60,-20.51,
26.94,-18.40, 29.39,-16.29, 32.93,-12.30, 34.96,-11.57, 38.24,-8.88,
39.06,-6.81, 39.51,-3.03, 40.90,-1.50, 42.80,0.60, 43.57,4.76),86,2,byrow=T))

if (!silent) print("SUMS OF U AND V")
if (!silent) cat(sum(dataVKS[[tType]][,1]),sum(dataVKS[[tType]][,2]),'\n')
#CHOOSE FIRST CAP NUMBER
if (tType==3) capnumbers<-c(capnumbers[85],capnumbers) else capnumbers<-c(0,capnumbers)
tSize<-c(16,16,86)[tType]
#CALCULATE SUMS OF SQUARES AND CROSS PRODUCTS
#REM COLOR DIFFERENCE VECTORS
DU = dataVKS[[tType]][capnumbers[2:(tSize)]+1,1]-dataVKS[[tType]][capnumbers[1:(tSize-1)]+1,1]
DV = dataVKS[[tType]][capnumbers[2:(tSize)]+1,2]-dataVKS[[tType]][capnumbers[1:(tSize-1)]+1,2]
U2 = sum(DU^2)
V2 = sum(DV^2)
UV = sum(DU * DV)
# CALCULATE MAJOR AND MINOR RADII AND ANGLE
D = U2 - V2
#ANGLE
if (D == 0) A0 = 0.7854 else A0 = atan(2 * UV / D) / 2
#MAJOR MOMENT
I0 = U2 * sin(A0)^2 + V2 * cos(A0)^2 - 2 * UV * sin(A0) * cos(A0)
#PERPENDICULAR ANGLE
if (A0 < 0) A1 = A0 + 1.5708 else A1 = A0 - 1.5708
#MINOR MOMENT
I1 = U2 * sin(A1)^2 + V2 * cos(A1)^2 - 2 * UV * sin(A1) * cos(A1)
#CHECK THAT MAJOR MOMENT GREATER THAN MINOR
if (!(I0 > I1)) {
#SWAP ANGLES & MOMENTS
P = A0
A0 = A1
A1 = P
P = I0
I0 = I1
I1 = P
}
#RADII & TOTAL ERROR
R0 = sqrt(I0 / (tSize-1))
R1 = sqrt(I1 / (tSize-1))
R = sqrt(R0^2 + R1^2)
if (tType == 1) { R2 = 9.234669; if (!silent) print ("STANDARD D-15")}
if (tType == 2) { R2 = 5.121259; if (!silent) print ("DESATURATED D-15")}
if (tType == 3) { R2 = 2.525249; if (!silent) print ("FM-100 HUE")}
if (!silent) cat ("ANGLE\tMAJ RAD\tMIN RAD\tTOT ERR\tS-INDEX\tC-INDEX\n")
if (!silent) cat('\t',57.3 * A1,'\t', R0,'\t', R1,'\t', R,'\t', R0 / R1,'\t', R0 / R2)
list(Angle=57.3 * A1,MajRad=R0,MinRad=R1,TotErr=R,Sindex=R0 / R1,Cindex=R0 / R2)
}

decolorize<-function(fileIN=NULL,effect=0.5,scale=NULL,noise=0.001,recolor=FALSE){
#Color Image to Grayscale Conversion
#Original work by Mark Grundland and Neil A. Dodgson
# Mark Grundland and Neil A. Dodgson, "Decolorize: Fast, Contrast Enhancing, Color to Grayscale Conversion",
# Pattern Recognition, vol. 40, no. 11, pp. 2891-2896, (2007).
# http://www.Eyemaginary.com/Portfolio/Publications.html
if (is.null(fileIN)) stop('A file input must be defined')
if (!file.exists(fileIN)) stop('Error! File does not exist')
p<-png::readPNG(fileIN)
p<-p*255
frameP<-dim(p)[1:2]
pixelsP<-prod(frameP)
if (is.null(scale)) scale<-sqrt(2*min(frameP))
tolerance=100 * .Machine$double.eps#2^(-52)
# Define the YPQ color space
colorconvert<-matrix(c(0.2989360212937753847527155, 0.5870430744511212909351327, 0.1140209042551033243121518,
0.5, 0.5, -1, 1, -1, 0),3,3,byrow=F)
colorrevert<-matrix(c(1, 0.1140209042551033243121518, 0.6440535265786729530912086, 1, 0.1140209042551033243121518, 
-0.3559464734213270469087914, 1, -0.8859790957448966756878482, 0.1440535265786729530912086),3,3,byrow=F)
colorspan<-matrix(c(0, 1, -1, 1, -1, 1),3,2,byrow=F)
maxluminance=1.0
scaleluminance=0.66856793424088827189
maxsaturation=1.1180339887498948482
alterP=effect*(maxluminance/maxsaturation)
# Convert picture to the YPQ color space
p2=array(p,c(pixelsP,3))
imageP=p2 %*% colorconvert
originalP=imageP
chromaP=sqrt(imageP[,2] * imageP[,2] + imageP[,3] * imageP[,3])

# Pair each pixel with a randomly chosen sample site
meshP=cbind(rep(1:frameP[1],frameP[2]),rep(1:frameP[2],each=frameP[1]))
displaceP=(scale * sqrt(2/pi)) * matrix(rnorm(pixelsP*2),pixelsP,2)#
lookP=round(meshP+displaceP)
redoP=which((lookP[,1]<1))
lookP[redoP,1]=2-sign(lookP[redoP,1])*(lookP[redoP,1] %% (frameP[1]-1))#
redoP=which((lookP[,2]<2))
lookP[redoP,2]=2-sign(lookP[redoP,2])*(lookP[redoP,2] %% (frameP[2]-1))#
redoP=which((lookP[,1]>frameP[1]))
lookP[redoP,1]=frameP[1]-1-sign(lookP[redoP,1])*((lookP[redoP,1]-2) %% (frameP[1]-1))#
redoP=which((lookP[,2]>frameP[2]))
lookP[redoP,2]=frameP[2]-1-sign(lookP[redoP,2])*((lookP[redoP,2]-2) %% (frameP[2]-1))#
lookP=lookP[,1]+frameP[1] * (lookP[,2]-1)#

# Calculate the color differences between the paired pixels
deltaP=imageP-imageP[lookP,]
contrastchange=abs(deltaP[,1])
contrastdirection=sign(deltaP[,1])
colordifference=p2-p2[lookP,]
colordifference=sqrt(apply(colordifference * colordifference,1,sum))+2^(-52)

# Derive a chromatic axis from the weighted sum of chromatic differences between paired pixels
weightP=1-((contrastchange/scaleluminance)/colordifference)

weightP[which(colordifference<tolerance)]=0
axisP=weightP * contrastdirection
axisP=deltaP[,2:3] * c(axisP, axisP)
axisP=apply(axisP,2,sum)

# Project the chromatic content of the picture onto the chromatic axis
projection=imageP[,2] * axisP[1] + imageP[,3] * axisP[2]
quantile(abs(projection),1-noise)
projection=projection / (quantile(abs(projection),1-noise)+tolerance)

# Combine the achromatic tones with the projected chromatic colors and adjust the dynamic range
imageP[,1]=imageP[,1]+effect*projection
imagerange=quantile(imageP[,1],c(noise, 1-noise))
imageP[,1]=(imageP[,1]-imagerange[1])/(imagerange[2]-imagerange[1]+tolerance)
targetrange=effect*c(0.0, maxluminance)+(1-effect)*quantile(originalP[,1],c(noise, 1-noise))
imageP[,1]=targetrange[1]+(imageP[,1]*(targetrange[2]-targetrange[1]+tolerance))

iP1<-imageP[,1]
w<-which(imageP[,1]<(originalP[,1]-alterP*chromaP))
iP1[w]<-originalP[w,1]-alterP*chromaP[w]

w<-which(iP1>(originalP[,1]+alterP*chromaP))
iP1[w]<-originalP[w,1]+alterP*chromaP
imageP[,1]=iP1/255.0
imageP[which(imageP<0.0)]=0.0
imageP[which(imageP>maxluminance)]=maxluminance

# Return the results
tones=imageP[,1] / maxluminance
tones=array(tones,dim(p))

if (recolor){
    recolorP=imageP %*% colorrevert
    recolorP=array(c(array(recolorP[,1],frameP),array(recolorP[,2],frameP),array(recolorP[,3],frameP)),dim(p))
    recolorP[which(recolorP<0)]=0.0
    recolorP[which(recolorP>1)]=1.0
    return(list(tones=tones,recolor=recolorP))
} else return(list(tones=tones,recolor=NULL))
}

decolorizeFile<-function(fileIN=NULL,fileOUT=NULL,effect=0.5,scale=NULL,noise=0.001,fileRecolorOUT=NULL){
#Color Image to Grayscale Conversion
#Original work by Mark Grundland and Neil A. Dodgson
# Mark Grundland and Neil A. Dodgson, "Decolorize: Fast, Contrast Enhancing, Color to Grayscale Conversion",
# Pattern Recognition, vol. 40, no. 11, pp. 2891-2896, (2007).
# http://www.Eyemaginary.com/Portfolio/Publications.html
if (is.null(fileIN)) stop('A file input must be defined')
if (!file.exists(fileIN)) stop('Error! File does not exist')
if (is.null(fileOUT)) stop('A file output must be defined')
if (!is.null(fileRecolorOUT)) {
if (!is.character(fileRecolorOUT)) stop('A file output for recolor must have a valid name')
if (fileRecolorOUT=='') stop('A file output for recolor must have a valid name')
}
if (is.null(fileRecolorOUT)) recolor<-FALSE else recolor<-TRUE
decF<-decolorize(fileIN,effect,scale,noise,recolor)
png::writePNG(decF$tones, fileOUT)
if (recolor) png::writePNG(decF$recolor, fileRecolorOUT)
}

