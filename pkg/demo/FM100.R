library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

incTimer<-function( h , ... )
{# timer event - decrease timer and validate at the end
if (is.na(timeC)) timeC<<-9*60 else { timeC<<-timeC - 1
svalue(lTimer) <- paste('Time left  ',trunc(timeC/60),':',(timeC-60*trunc(timeC/60)),sep='') }
if (timeC<0) validate(h)
}

validate<-function( h , ... )
{# close the window and score the test
dispose(w)
scoreFM100Graphic(userFM100values=lColors)
lColorsOK<-sprintf('#%02x%02x%02x',FarnsworthMunsell100Hue[-1,'R'],FarnsworthMunsell100Hue[-1,'G'],FarnsworthMunsell100Hue[-1,'B'])
pos2<-c()
for (n in 1:85) pos2<-c(pos2,which(lColors[n] == lColorsOK) )
tmpR<-paste('total error score (TES) using Farnsworth\'s method:',paste(sum(calculateTES(pos2,FALSE)-2*85),collapse='\t',sep=' '),
'total error score (TES) using Kinnear\'s method:',paste(sum(calculateTES(pos2,TRUE)-2*85),collapse='\t',sep=' '),
'\nANGLE\tMAJ\tRAD\tMIN\tRAD\tTOT\tERR\tS-INDEX\tC-INDEX',
'Vingrys and King-Smith method (1988)',
paste(round(unlist(Color.Vision.VingrysAndKingSmith(pos2,testType='Farnsworth Munsell 100-Hue')),2),collapse='\t',sep=' '),sep='\n')
gmessage(tmpR)
}

dropF<-function(h,strToB)
{# function to assist the drop event
if (!(tmpv %in% as.character(1:85))) return(FALSE)
rangeRow<-c(1:20,85)
if (!(tmpv %in% as.character(rangeRow)) & (strToB %in% as.character(rangeRow))) return(FALSE)
rangeRow<-c(22:42)
if (!(tmpv %in% as.character(rangeRow)) & (strToB %in% as.character(rangeRow))) return(FALSE)
rangeRow<-c(43:63)
if (!(tmpv %in% as.character(rangeRow)) & (strToB %in% as.character(rangeRow))) return(FALSE)
rangeRow<-c(64:84)
if (!(tmpv %in% as.character(rangeRow)) & (strToB %in% as.character(rangeRow))) return(FALSE)
ndxs<-1:85
ndxs[as.numeric(tmpv)]<-NA
posI<-(as.numeric(strToB))
if ((posI== as.numeric(tmpv)+1) ) posI<-posI+1
if (posI %in% c(1,22,43,64)) ndxs<-c(as.numeric(tmpv),ndxs) else ndxs<-append(ndxs, as.numeric(tmpv), after=posI-1)
ndxs<-ndxs[-which(is.na(ndxs))]
lColors<<-lColors[ndxs]
mapply(modify_button, buttons[1:85], lColors)
tmpv<<-''
}

modify_button <- function(b, col) {
col <- as.GdkColor(col)
getToolkitWidget(b)$modifyBg(GtkStateType["normal"], col)
getToolkitWidget(b)$modifyBg(GtkStateType["active"], col)
getToolkitWidget(b)$modifyBg(GtkStateType["prelight"], col)
getToolkitWidget(b)$modifyBg(GtkStateType["selected"], col)
getToolkitWidget(b)$modifyFg(GtkStateType["normal"], col)
getToolkitWidget(b)$modifyFg(GtkStateType["active"], col)
getToolkitWidget(b)$modifyFg(GtkStateType["prelight"], col)
getToolkitWidget(b)$modifyFg(GtkStateType["selected"], col)
}

# instructions
#gmessage('', title="Farnsworth Munsell 100-Hue color vision test - instructions",icon = "info")

# prepare variables for the colors to be displayed and the sequence from the user
data(FarnsworthMunsell100Hue) # list of colors

lColorsStart<-sprintf('#%02x%02x%02x',FarnsworthMunsell100Hue[,'R'],FarnsworthMunsell100Hue[,'G'],FarnsworthMunsell100Hue[,'B'])

#color1st<-lColorsStart[c(84,21,21,42,42,63,63,83)]
#lColorsStart<-sample(lColorsStart,85) # mix them

# corner
color1st<-lColorsStart[c(84,21,42,63,22,43,64,85)]
# mix them
lColorsStart<-c(sample(lColorsStart[c(1:21,85)],22),sample(lColorsStart[c(22:42)],21), sample(lColorsStart[c(43:63)],21), sample(lColorsStart[c(64:84)],21))


lColors<-lColorsStart
# create GUI
tmpv<-''
timeC<-NA
w <- gwindow("Farnsworth-Munsell100-hue test")
getToolkitWidget(w)$maximize()
g0 <- ggroup(cont=w, expand=TRUE, horizontal=F, spacing =0)
g <- ggroup(cont=g0, expand=TRUE, horizontal=T, spacing =0)

wlayout = glayout(visible=TRUE,container=g0, expand=TRUE, spacing =0)

scrPos<-matrix( c(rbind( cbind(1,2:23), cbind(3,2:22), cbind(5,2:22), cbind(7,2:22) )),85,2,byrow=F)
buttons <- lapply(1:93, function(x) gbutton('\n', cont=wlayout))
for (n in 1:85) wlayout[scrPos[n,1],scrPos[n,2], expand=TRUE] <- buttons[[n]]

scrPos2<-matrix(c(rbind(cbind(1:4*2-1,1),c(1,24), cbind(2:4*2-1,23))) ,8,2,byrow=F)
for (n in 1:8) wlayout[scrPos2[n,1],scrPos2[n,2], expand=TRUE] <- buttons[[85+n]]

wlayout[2,1, expand=TRUE] <- gbutton('\n', cont=wlayout)
wlayout[4,1, expand=TRUE] <- gbutton('\n', cont=wlayout)
wlayout[6,1, expand=TRUE] <- gbutton('\n', cont=wlayout)
modify_button(wlayout[2,1],'#000000')
modify_button(wlayout[4,1],'#000000')
modify_button(wlayout[6,1],'#000000')

mapply(modify_button, buttons, c(lColors, color1st))

lTimer<-glabel('Time left  9:00', cont = g0)

font(lTimer) <- c(color="red", weight = 'bold', scale = "xx-large")

bDONE<-gbutton("Done", cont=g0,handler = validate)
font(bDONE) <- c(color="red", weight = 'bold', scale = "xx-large")

getToolkitWidget(w)$modifyBg(GtkStateType["normal"], "black")

for (n in 1:85 ) {
b<-wlayout[scrPos[n,1],scrPos[n,2]]
eval(parse( text=paste('addDropSource(b, handler = function(h,...) tmpv<<-',as.character(n),')',sep='') ))
eval(parse( text=paste('addDropTarget(b,targetType="object", handler = function(h,...) dropF(h,',as.character(n),'))',sep='') ))
}

gtimer(2000, incTimer, lTimer )
