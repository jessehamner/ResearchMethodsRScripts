# A nice small script to provide some graphic insight to test grade distributions.

library(foreign)

fontsize=10

# A function to draw a distribution normal curve over the histogram:
# (Thanks to Peter Dalgaard)
addNorm <- function(data,color,linewidth) {	
	xfit<-seq(min(data),max(data),length=80)
	yfit<-dnorm(xfit,mean=mean(data),sd=sd(data))
	yfit <- yfit*diff(h$mids[1:2])*length(data)
	lines(xfit, yfit, col=color, lwd=linewidth) 
return;
}

addText <- function(xcoord,ycoord,avg,sd,med) {
	text(xcoord,ycoord,labels=paste("µ = ",avg,sep=""), cex=1.4)
	text(xcoord,(ycoord-0.5),labels=paste("σ = ",sd,sep=""),cex=1.4)
	text(xcoord,(ycoord-1.0),labels=paste("med. = ",med,sep=""),cex=1.4)
return;	
}

flatTop <- function(datavector,topofrange) {
	for(i in 1:length(datavector)) {
		if (datavector[i]>topofrange) { datavector[i]=topofrange }
	}
return(datavector);	
}

######################################################################
# Now to the work:
######################################################################

# get grades for the first exam:
grades<-read.table("secondtest.csv", sep = "\t", header=TRUE, fill=TRUE)
attach(grades)

# Create summary statistics for Exam 1 raw values:
minimum=min(Exam2PCT)
maximum=max(Exam2PCT)
avg<-round(mean((Exam2PCT)),digits=1)
sd<-round(sd(Exam2PCT),digits=1)
mediangrade<-median(Exam2PCT)

firstCurve <- 6
secondCurve <- 6

png(filename="test2rawhistogram.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Histogram
h<-hist(Exam2PCT,breaks=5,col="gray", main="Histogram of Original Grades on the Second Exam", cex=1.0, xlab="Grades", ylim=c(0,10))

# Annotation text:
addText(xcoord=55,ycoord=8.5,avg=avg,sd=sd,med=mediangrade)
text(75,8.0,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

dev.off()


# Second Histogram

png(filename="test2histogram.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

curved<-flatTop((grades$Exam2PCT+firstCurve),100)
avg<-round(mean((curved)),digits=1)
sd<-round(sd(curved),digits=1)
mediangrade<-median(curved)
minimum=min(curved)
maximum=max(curved)

h<-hist(curved,breaks=5,col="gray", main="Histogram of Final Exam Grades Plus One Curve", cex=1.0, xlab="Grades", ylim=c(0,10))

# Normal Curve (note--doesn't care if the distribution is not normal):
#addNorm(data=ComputedScore,color="red",linewidth=2)

# Annotation text:
addText(xcoord=50,ycoord=8.5,avg=avg,sd=sd,med=mediangrade)
text(50,6.9,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"+]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

dev.off()


# Last Histogram:

DoubleCurve<-(Exam2PCT+firstCurve+secondCurve)

set1<-flatTop(DoubleCurve,100)
avg<-round(mean((set1)),digits=1)
sd<-round(sd(set1),digits=1)
mediangrade<-median(set1)

png(filename="test2histogram_withnormal.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Draw histogram
h<-hist(set1,breaks=5,col="gray", main="Histogram of Curved Grades on the Final Exam\nPlus Secondary Curve", cex=1.0, xlab="Grades", ylim=c(0,10))

# Normal Curve (note--doesn't care if the distribution is not normal):
addNorm(data=set1,color="red",linewidth=2)

# Annotation text:
addText(xcoord=60,ycoord=7.75,avg=avg,sd=sd,med=mediangrade)
#text(60,6.75,labels=paste(paste(paste("Raw Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)

dev.off()

# Time to complete the exam: did it affect scores at all?

png(filename="test2timescatterplot.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6,height=6,units="in")

grades$time<-(grades$TimeTurnedIn-1200)
# for tests less than two hours long:
for(i in 1:length(grades$time)){if (grades$time[i]>=100){grades$time[i]<-grades$time[i]-40} }
plot(grades$time,grades$Exam2RAW,xlab="Time to Complete Exam",ylab="Raw Exam Score",main="Test Score as a Function of Time to Finish")
gradetime<-lm(grades$Exam2RAW~grades$time, data=grades)
abline(gradetime,col="red")

dev.off()


detach(grades)

# EOF