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
scoreRoth28Graphic(lColors)
lColorsOK<-sprintf('#%02x%02x%02x',Roth28[-1,'R'],Roth28[-1,'G'],Roth28[-1,'B'])
pos2<-c()
for (n in 1:28) pos2<-c(pos2,which(lColors[n] == lColorsOK) )
tmpR<-paste('total error score (TES) using Farnsworth\'s method:',
paste(sum(calculateTES(pos2)),collapse='\t',sep=' '),'\nANGLE\tMAJ\tRAD\tMIN\tRAD\tTOT\tERR\tS-INDEX\tC-INDEX',
'Vingrys and King-Smith method (1988)',
paste(round(unlist(Color.Vision.VingrysAndKingSmith(pos2,testType='Roth28-Hue')),2),collapse='\t',sep=' '),sep='\n')
gmessage(tmpR)
}

dropF<-function(h,strToB)
{# function to assist the drop event
if (!(tmpv %in% as.character(1:28) )) return(FALSE)
ndxs<-1:28
ndxs[as.numeric(tmpv)]<-NA
posI<-(as.numeric(strToB))
if ((posI== as.numeric(tmpv)+1) ) posI<-posI+1
if (posI==1) ndxs<-c(as.numeric(tmpv),ndxs) else ndxs<-append(ndxs, as.numeric(tmpv), after=posI-1)
ndxs<-ndxs[-which(is.na(ndxs))]
lColors<<-lColors[ndxs]

mapply(modify_button, buttons[1:28], lColors)

tmpv<<-''
}

# instructions

# prepare variables for the colors to be displayed and the sequence from the user
data(Roth28) # list of colors
lColorsStart<-vectorPNGbuttons(Roth28)
color1st<-lColorsStart[1]
lColorsStart<-lColorsStart[-1]
lColorsStart<-sample(lColorsStart,28) # mix them
lColors<-lColorsStart
# create GUI
tmpv<-''
timeC<-NA
w <- gwindow("Roth 28 test")
getToolkitWidget(w)$maximize()
g0 <- ggroup(cont=w, expand=TRUE, horizontal=F, spacing =0)
g <- ggroup(cont=g0, expand=TRUE, horizontal=T, spacing =0)

wlayout = glayout(visible=TRUE,container=g0, expand=TRUE, spacing =0)
#wlayout[1,1, expand=TRUE] = gbutton('\n', cont=wlayout)
#for (n in 1:15) wlayout[1,n+1, expand=TRUE] = gbutton('\n', cont=wlayout)

scrPos<-matrix(c(5,1,4,1,3,2,2,3,1,4,1,5,1,6,1,7,1,8,2,9,3,10,4,11,5,11,6,11,7,11,8,11,9,11,10,10,11,9,12,8,12,7,12,6,12,5,12,4,11,3,10,2,9,1,8,1),28,2,byrow=TRUE)

buttons <- lapply(1:29, function(x) gimage(c(color1st,lColors)[x], cont = wlayout))#
for (n in 1:28) wlayout[scrPos[n,1],scrPos[n,2], expand=TRUE] = buttons[[n]]
wlayout[6,1, expand=TRUE] = buttons[[29]]

#sapply( lColors, as.GdkColor)
modify_button <- function(b, colX) {
svalue(b) <- colX
}

#mapply(modify_button, buttons, c(lColors, color1st))

lTimer<-glabel('Time left  2:00', cont = g0 )

font(lTimer) <- c(color="red", weight = 'bold', scale = "xx-large")

bDONE<-gbutton("Done", cont=g0,handler = validate)
font(bDONE) <- c(color="red", weight = 'bold', scale = "xx-large")

getToolkitWidget(w)$modifyBg(GtkStateType["normal"], "black")

for (n in 1:28 ) {
b<-wlayout[scrPos[n,1],scrPos[n,2]]
eval(parse( text=paste('addDropSource(b, handler = function(h,...) tmpv<<-',as.character(n),')',sep='') ))
eval(parse( text=paste('addDropTarget(b,targetType="object", handler = function(h,...) dropF(h,',as.character(n),'))',sep='') ))
}

gtimer(2000, incTimer, lTimer )