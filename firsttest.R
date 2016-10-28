# A nice small script to provide some graphic insight to test grade distributions.

library(foreign)
# setwd("")

fontsize=10

# A function to draw a distribution normal curve over the histogram:
# (Thanks to Peter Dalgaard)
addNorm <- function(data,color,linewidth) {	
	xfit<-seq(min(data),max(data),length=80)
	yfit<-dnorm(xfit,mean=mean(data),sd=sd(data))
	yfit <- yfit*diff(h$mids[1:2])*length(data)
	lines(xfit, yfit, col=color, lwd=linewidth) 
return
}

addText <- function(xcoord,ycoord,avg,sd,med) {
	text(xcoord,ycoord,labels=paste("µ = ",avg,sep=""), cex=1.4)
	text(xcoord,(ycoord-0.5),labels=paste("σ = ",sd,sep=""),cex=1.4)
	text(xcoord,(ycoord-1.0),labels=paste("med. = ",med,sep=""),cex=1.4)
return
}

flatTop <- function(datavector,topofrange) {
	for(i in 1:length(datavector)) {
		if (datavector[i]>topofrange) { datavector[i]=topofrange }
	}
return(datavector)
}

# get grades for the first exam:
grades<-read.table("firsttest.tab", sep = "\t", header=TRUE, fill=TRUE)
attach(grades)

# Create summary statistics for Exam 1 raw values:
minimum=min(Exam1RAW)
maximum=max(Exam1RAW)
avg<-round(mean((Exam1RAW)),digits=1)
sd<-round(sd(Exam1RAW),digits=1)
mediangrade<-median(Exam1RAW)

png(filename="test1rawhistogram.png", 
    res=300, 
    bg="white", 
    type="quartz", 
    pointsize=fontsize, 
    width=6, height=6, 
    units="in"
    )

# Histogram
h<-hist(Exam1RAW,
        breaks=5,
        col="gray", 
        main="Histogram of Original Grades on the First Exam", 
        xlab="Grades", 
        ylim=c(0,10)
        )

# Annotation text:
addText(xcoord=44,ycoord=8.5,avg=avg,sd=sd,med=mediangrade)
text(70,8.0,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

dev.off()


# Second Histogram

png(filename="test1histogram.png", 
    res=300, 
    bg="white", 
    type="quartz", 
    pointsize=fontsize, 
    width=6, height=6, 
    units="in"
    )

curve1<-flatTop((Exam1RAW+Curve),100)

avg<-round(mean((curve1)),digits=1)
sd<-round(sd(curve1),digits=1)
mediangrade<-median(curve1)
minimum=min(curve1)
maximum=max(curve1)

h<-hist(curve1,
        breaks=5,
        col="gray", 
        main="Histogram of Curved Grades on the First Exam", 
        cex=1.0, 
        xlab="Grades", 
        ylim=c(0,10)
        )

# Normal Curve (note--doesn't care if the distribution is not normal):
#addNorm(data=ComputedScore,color="red",linewidth=2)

# Annotation text:
addText(xcoord=50,
        ycoord=8.5,
        avg=avg,
        sd=sd,
        med=mediangrade
        )
text(50,6.9,
     labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"+]",
                  sep=""),
     cex=1.2
     )
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

dev.off()


# Last Histogram:

set1<-flatTop(ComputedScore,100)
avg<-round(mean((set1)),digits=1)
sd<-round(sd(set1),digits=1)
mediangrade<-median(set1)

png(filename="test1histogram_withnormal.png", 
    res=300, bg="white", 
    type="quartz", 
    pointsize=fontsize, 
    width=6, height=6, 
    units="in"
    )

# Draw histogram
h<-hist(set1,breaks=5,
        col="gray", 
        main="Histogram of Curved Grades on the First Exam\nPlus Attendance Bonus Quiz Yesterday", 
        cex=1.0, 
        xlab="Grades", 
        ylim=c(0,10)
        )

# Normal Curve (note--doesn't care if the distribution is not normal):
addNorm(data=set1,color="red",linewidth=2)

# Annotation text:
addText(xcoord=54,ycoord=7.75,avg=avg,sd=sd,med=mediangrade)
#text(50,6.75,labels=paste(paste(paste("Raw Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)

dev.off()

# Time to complete the exam: did it affect scores at all?

png(filename="test1timescatterplot.png", 
    res=300, bg="white", 
    type="quartz", 
    pointsize=fontsize, 
    width=6,height=6,
    units="in"
    )

grades$time<-(grades$TimeTurnedIn-1200)
# for tests less than two hours long:
for(i in 1:length(grades$time)){
  if (grades$time[i]>=100){
    grades$time[i]<-grades$time[i]-40
  } 
}

plot(grades$time,
     grades$Exam1RAW,
     xlab="Time to Complete Exam",
     ylab="Raw Exam Score",
     main="Test Score as a Function of Time to Finish"
     )

gradetime<-lm(grades$Exam1RAW~grades$time, data=grades)

abline(gradetime,col="red")

dev.off()


detach(grades)

# EOF