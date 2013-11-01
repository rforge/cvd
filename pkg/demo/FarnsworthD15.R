library(gWidgets)
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
dispose(w)
scoreD15Graphic(lColors)
lColorsOK<-sprintf('#%02x%02x%02x',FarnsworthD15[-1,'R'],FarnsworthD15[-1,'G'],FarnsworthD15[-1,'B'])
pos2<-c()
for (n in 1:15) pos2<-c(pos2,which(lColors[n] == lColorsOK) )
tmpR<-paste('Bowman\'s (1982) Total Color Difference Score (TCDS) and Color Confusion Index (CCI)\nTCDS\tCCCI',
paste(unlist(scoreD15TCDS(lColors)),collapse='\t',sep=' '),'\nANGLE\tMAJ\tRAD\tMIN\tRAD\tTOT\tERR\tS-INDEX\tC-INDEX',
'Vingrys and King-Smith method (1988)',
paste(round(unlist(Color.Vision.VingrysAndKingSmith(pos2)),2),collapse='\t',sep=' '),sep='\n')
gmessage(tmpR)
}

colorgBtn<-function(allButtons,allColors)
{# assign colors to the buttons
n<-1
for (b in allButtons) {
b$modifyBg('normal', allColors[n])
b$modifyBg('active', allColors[n])
b$modifyBg('prelight', allColors[n])
b$modifyBg('selected', allColors[n])
b$modifyFg('normal', allColors[n])
b$modifyFg('active', allColors[n])
b$modifyFg('prelight', allColors[n])
b$modifyFg('selected', allColors[n])
n<-n+1
}
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
colorgBtn(wlayout[1,2:16],lColors)
tmpv<<-''
}

# instructions
gmessage('The Farnsworth dichotomous test (D-15) classifies subjects into Strongly/Medium color deficient or Mildly color deficient/normal.

The test should be administered on a black background, to prevent any interference from background colors.

The recommended illumination is approximately 6700º Kelvin at 25 foot-candles or greater (Illuminant C) or daylight.

The working distance is 50 cm (19.5 inches). There are 15 colored caps to be sorted, the most common sizes are 12 mm (0.5 inches) and 33 mm (1.3 inches).

The first colored cap (from the left) is the "reference cap", this is the first color from the sequence of colors and it can\'t be moved.
The test taker has two minutes to move the caps to form a sequence of colors. The test will finish after two minutes or after pressing the button "Done".


Instructions based on: 
Farnsworth D-15 and Lanthony Test Instructions
Rev 1.7 (05/06)
Richmond Products Inc.', title="Farnsworth D-15 color vision test - instructions",icon = "info")

# prepare variables for the colors to be displayed and the sequence from the user
data(FarnsworthD15) # list of colors
lColorsStart<-sprintf('#%02x%02x%02x',FarnsworthD15[,'R'],FarnsworthD15[,'G'],FarnsworthD15[,'B'])
color1st<-lColorsStart[1]
lColorsStart<-lColorsStart[-1]
lColorsStart<-sample(lColorsStart,15) # mix them
lColors<-lColorsStart
# create GUI
tmpv<-''
timeC<-NA
w <- gwindow("Farnsworth dichotomous test (D-15)")
getToolkitWidget(w)$maximize()
g0 <- ggroup(cont=w, expand=TRUE, horizontal=F, spacing =0)
g <- ggroup(cont=g0, expand=TRUE, horizontal=T, spacing =0)

wlayout = glayout(visible=TRUE,container=g, expand=TRUE, spacing =0)

wlayout[1,1, expand=TRUE] = gbutton('\n', cont=wlayout)
for (n in 1:15) wlayout[1,n+1, expand=TRUE] = gbutton('\n', cont=wlayout)
colorgBtn(wlayout[1,],c(color1st,lColors))

lTimer<-glabel('Time left  2:00', cont = g0 )
font(lTimer) <- c(color="red", size='30')
bDONE<-gbutton("Done", cont=g0,handler = validate)
font(bDONE) <- c(color="red", size='30')

w$modifyBg(GtkStateType["normal"], "black")

n<-1
for (b in wlayout[1,2:16]) {
eval(parse( text=paste('addDropSource(b, handler = function(h,...) tmpv<<-',as.character(n),')',sep='') ))
eval(parse( text=paste('adddroptarget(b,targetType="object", handler = function(h,...) dropF(h,',as.character(n),'))',sep='') ))
n<-n+1
}

addHandlerIdle (lTimer, handler = incTimer, interval = 2000)