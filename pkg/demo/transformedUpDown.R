# same plot as:
# Transformed Up-Down Method (1 up/2 down rule)
# using the simple up/down rule until the first (neglected) reversal
# Reference: www.Bedienhaptik.de
# Dr. Manuel Kuehner
stimulusIntensity<-c(11,9,7,5,3:6,6,5,5,4,5,6,6,5,6,7,7,6,6,5,5,4,4)
stimulusDetected<-c('Right','Right','Right','Right','Wrong','Wrong','Wrong','Right','Right','Right','Right','Wrong','Wrong','Right','Right','Wrong','Wrong','Right','Right','Right','Right','Right','Right','Right','Wrong')
rvrslsN<-CountReversals(stimulusDetected)
rvrsls<-MarkReversalsUD(rvrslsN, stimulusIntensity)

xl<-c(1, 25);yl<-c(0,12)
plot(1:25, answers, type='b',xlim=xl,ylim=yl, pch=as.character(rvrslsN),xlab='Trial Number',ylab='Stimulus Intensity x',
main='Transformed Up-Down Method (1 up/2 down rule)')
dev.new()
plot(1:25, answers, type='b',xlim=xl,ylim=yl, pch=ifelse(stimulus=='Right',15,0),xlab='Trial Number',ylab='Stimulus Intensity',
main='Transformed Up-Down Method (1 up/2 down rule)')
par(new=TRUE)
plot(1:25, answers, type='p',xlim=xl,ylim=yl, pch=ifelse(rvrsls,1,NA),cex=1.75,xlab='',ylab='')
par(new=TRUE)
plot(1:25, (answers+0.5)*ifelse(rvrsls,1,-1), type='p',xlim=xl,ylim=yl, pch=ifelse(rvrsls,as.character(rvrslsN-1),NA),cex=.75,xlab='',ylab='')
legend("topright",c('Answers','Stimulus detected','Stimulus not detected','Reversal Ri'),pch=c(NA,15,0,1))
