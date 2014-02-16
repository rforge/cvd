library(gWidgets2)
options(guiToolkit="RGtk2")
library(RGtk2)
library(CVD)

incTimer<-function( h , ... )
{# timer event - decrease timer and validate at the end
if (is.na(timeC)) timeC<<-120 else { timeC<<-timeC - 1
svalue(lTimer) <- paste('Time left  ',trunc(timeC/60),':',(timeC-60*trunc(timeC/60)),sep='') }
if (timeC<0) validate(h)
}

validate<-function( h , ... )
{# close the window and score the test
lColors <- paste('#',substr(lColors,nchar(lColors)-9,nchar(lColors)-4),sep='')
dispose(w)
scoreD15Graphic(lColors, titleGraphic="Lanthony desaturated D-15 test (D-15d) results", okD15colors=sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B']))
lColorsOK<-sprintf('#%02x%02x%02x',LanthonyD15[-1,'R'],LanthonyD15[-1,'G'],LanthonyD15[-1,'B'])
pos2<-c()
for (n in 1:15) pos2<-c(pos2,which(lColors[n] == lColorsOK) )
tmpR<-paste('Bowman\'s (1982) Total Color Difference Score (TCDS) and Color Confusion Index (CCI)\nTCDS\tCCCI',
paste(unlist(scoreD15TCDS(lColors, distTable=GellerTCDS, D15colors=LanthonyD15)),collapse='\t',sep=' '),'\nANGLE\tMAJ\tRAD\tMIN\tRAD\tTOT\tERR\tS-INDEX\tC-INDEX',
'Vingrys and King-Smith method (1988)',
paste(round(unlist(Color.Vision.VingrysAndKingSmith(pos2, testType='D-15DS')),2),collapse='\t',sep=' '),sep='\n')
gmessage(tmpR)
}

dropF<-function(h,strToB)
{# function to assist the drop event
if (!(tmpv %in% as.character(1:15) )) return(FALSE)
ndxs<-1:15
ndxs[as.numeric(tmpv)]<-NA
posI<-(as.numeric(strToB))
if ((posI== as.numeric(tmpv)+1) ) posI<-posI+1
if (posI==1) ndxs<-c(as.numeric(tmpv),ndxs) else ndxs<-append(ndxs, as.numeric(tmpv), after=posI-1)
ndxs<-ndxs[-which(is.na(ndxs))]
lColors<<-lColors[ndxs]
mapply(modify_button, buttons, c(color1st,lColors))
tmpv<<-''
}

modify_button <- function(b, colX) {
svalue(b) <- colX
}

# prepare variables for the colors to be displayed and the sequence from the user
data(LanthonyD15) # list of colors
lColorsStart<-vectorPNGbuttons(LanthonyD15)
color1st<-lColorsStart[1]
lColorsStart<-lColorsStart[-1]
lColorsStart<-sample(lColorsStart,15) # mix them
lColors<-lColorsStart
# create GUI
tmpv<-''
timeC<-NA
w <- gwindow("Lanthony desaturated D-15 test (D-15d)")
getToolkitWidget(w)$maximize()
g0 <- ggroup(cont=w, expand=TRUE, horizontal=F, spacing =0)
g <- ggroup(cont=g0, expand=TRUE, horizontal=T, spacing =0)

wlayout = glayout(visible=TRUE,container=g0, expand=TRUE, spacing =0)

buttons <- lapply(1:16, function(x) gimage(c(color1st,lColors)[x], cont = wlayout))
for (n in 1:16) wlayout[1,n, expand=TRUE] = buttons[[n]]

#mapply(modify_button, buttons, c(color1st,lColors))

lTimer<-glabel('Time left  2:00', cont = g0 )

font(lTimer) <- c(color="red", weight = 'bold', scale = "xx-large")

bDONE<-gbutton("Done", cont=g0,handler = validate)
font(bDONE) <- c(color="red", weight = 'bold', scale = "xx-large")

getToolkitWidget(w)$modifyBg(GtkStateType["normal"], "black")

n<-1
for (b in wlayout[1,2:16]) {
eval(parse( text=paste('addDropSource(b, handler = function(h,...) tmpv<<-',as.character(n),')',sep='') ))
eval(parse( text=paste('addDropTarget(b,targetType="object", handler = function(h,...) dropF(h,',as.character(n),'))',sep='') ))
n<-n+1
}

gtimer(2000, incTimer, lTimer )
