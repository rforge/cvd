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

modify_button <- function(b, colX) {
svalue(b) <- colX
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

# instructions

gmessage('General Description.\n\nThe Farnsworth Dichotomous Test for Color Blindness (Panel D-15) is designed to select those observers with severe discrimination loss. In addition to indicating red-green discrimination loss, the test also indicates blue-yellow dicrimination loss and detects monochromacy. The test consists of 15 colored caps placed in a box, with one reference cap at a fixed location. The samples are chosen to represent approximately equal hue steps in the natural color circle and are similar in chroma to those of the FM 100-hue test. They are set in plastic caps and subtend 1.5 degrees at 50 cm. The movable caps are numbered on the back according to the correct color circle. An instruction manual and scoring sheets are provided. Additional scoring sheets are available.\n\nQuoted from:\nProcedures for Testing Color Vision: Report of Working Group 41\nCommittee on Vision, National Research Council\nISBN: 0-309-58883-9, 128 pages, 8.5 x 11, (1981)', title="Farnsworth D-15 color vision test - 
General Description",icon = "info")
gmessage('Administration.\n\nThe examiner prearranges the caps in random order on the upper lid of the open box. The subject is instructed to "arrange the caps in order according to color" in the lower tray, starting with the cap closest in color to the fixed reference cap. The box is presented at a comfortable distance under daylight illumination of at least 270 lux. The majority of individuals with normal color vision can complete the test within one minute. The observer is allowed as long as is necessary to complete the task. People with poor coordination may have difficulty in handling the caps.\n\nQuoted from:\nProcedures for Testing Color Vision: Report of Working Group 41\nCommittee on Vision, National Research Council\nISBN: 0-309-58883-9, 128 pages, 8.5 x 11, (1981)', title="Farnsworth D-15 color vision test - Administration",icon = "info")

# The Farnsworth dichotomous test (D-15) classifies subjects into Strongly/Medium color deficient or Mildly color deficient/normal.
# 
# The test should be administered on a black background, to prevent any interference from background colors.
# 
# The recommended illumination is approximately 6700 deg. Kelvin at 25 foot-candles or greater (Illuminant C) or daylight.
# 
# The working distance is about 50 cm (20 inches). There are 15 colored caps to be sorted, the most common sizes are 12 mm (0.5 inches) and 33 mm (1.3 inches).
# 
# The first colored cap (from the left) is the "reference cap", this is the first color from the sequence of colors and it can\'t be moved.
# The test taker has two minutes to move the caps to form a sequence of colors. The test will finish after two minutes or after pressing the button "Done".
# 
# 
# Instructions based on: 
# Farnsworth D-15 and Lanthony Test Instructions
# Rev 1.7 (05/06)
# Richmond Products Inc.

# prepare variables for the colors to be displayed and the sequence from the user
data(FarnsworthD15) # list of colors
lColorsStart<-vectorPNGbuttons(FarnsworthD15)
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

wlayout = glayout(visible=TRUE,container=g0, expand=TRUE, spacing =0)

buttons <- lapply(1:16, function(x) gimage(c(color1st,lColors)[x], cont = wlayout))
for (n in 1:16) wlayout[1,n, expand=TRUE] = buttons[[n]]

###mapply(modify_button, buttons, c(color1st,lColors))

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
