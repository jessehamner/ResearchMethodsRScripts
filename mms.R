# A script to provide some insight to M&M distributions.
# Jesse Hamner, 2012


library(foreign)
#setwd("")

fontsize=12
cexval=1.2

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
	text(xcoord,ycoord,labels=paste("µ = ",avg,sep=""), cex=1.5)
	text(xcoord,(ycoord-0.7),labels=paste("σ = ",sd,sep=""),cex=1.5)
	text(xcoord,(ycoord-1.4),labels=paste("med. = ",med,sep=""),cex=1.5)
return;	
}

flatTop <- function(datavector,topofrange) {
	for(i in 1:length(datavector)) {
		if (datavector[i]>topofrange) { datavector[i]=topofrange }
	}
return(datavector);	
}

# get grades for the first exam:
mms<-read.table("MM2012.csv", sep = "\t", header=TRUE, fill=TRUE)
attach(mms)

sumstat=Total

# Create summary statistics for Exam 1 raw values:
minimum=min(sumstat)
maximum=max(sumstat)
avg<-round(mean((sumstat)),digits=1)
sd<-round(sd(sumstat),digits=1)
mediangrade<-median(sumstat)

png(filename="Totalmmsrawhist.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Histogram
h<-hist(x=sumstat,breaks=8,
        col="gray", 
        main="Histogram of M&Ms", 
        cex=1.0, 
        xlab=paste("Total","Count", sep=" "), 
        ylim=c(0,12), 
        xlim=c(45,65)
        )

# Annotation text:
addText(xcoord=50,ycoord=6,avg=avg,sd=sd,med=mediangrade)
text(70,8.0,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=cexval)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

addNorm(data=sumstat,color="red", linewidth=2)

dev.off()


# Second Histogram

sumstat=Blue

# Create summary statistics for Exam 1 raw values:
minimum=min(sumstat)
maximum=max(sumstat)
avg<-round(mean((sumstat)),digits=1)
sd<-round(sd(sumstat),digits=1)
mediangrade<-median(sumstat)

#png(filename="Bluemmsrawhist.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Histogram
h<-hist(x=sumstat,breaks=8,col="gray", main="Histogram of Blue M&Ms", cex=1.0, xlab=paste("Blue","Count", sep=" "), ylim=c(0,12), xlim=c(5,20))

# Annotation text:
addText(xcoord=7,ycoord=9,avg=avg,sd=sd,med=mediangrade)
#text(70,8.0,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)
addNorm(data=sumstat,color="red", linewidth=2)

#dev.off()


# Third Histogram

sumstat=Green

# Create summary statistics for Exam 1 raw values:
minimum=min(sumstat)
maximum=max(sumstat)
avg<-round(mean((sumstat)),digits=1)
sd<-round(sd(sumstat),digits=1)
mediangrade<-median(sumstat)

png(filename="Greenmmsrawhist.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Histogram
h<-hist(x=sumstat,breaks=8,col="gray", main="Histogram of Green M&Ms", cex=1.0, xlab=paste("Green","Count", sep=" "), ylim=c(0,7), xlim=c(6,18))

# Annotation text:
addText(xcoord=12,ycoord=6,avg=avg,sd=sd,med=mediangrade)
#text(70,8.0,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)
addNorm(data=sumstat,color="red", linewidth=2)

dev.off()


# Fourth Histogram

sumstat=Brown

# Create summary statistics for Exam 1 raw values:
minimum=min(sumstat)
maximum=max(sumstat)
avg<-round(mean((sumstat)),digits=1)
sd<-round(sd(sumstat),digits=1)
mediangrade<-median(sumstat)

png(filename="Brownmmsrawhist.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Histogram
h<-hist(x=sumstat,breaks=8,col="gray", main="Histogram of Brown M&Ms", cex=1.0, xlab=paste("Brown","Count", sep=" "), ylim=c(0,7), xlim=c(2,14))

# Annotation text:
addText(xcoord=12,ycoord=5,avg=avg,sd=sd,med=mediangrade)
#text(70,8.0,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

addNorm(data=sumstat,color="red", linewidth="2")

dev.off()


# fifth Histogram

sumstat=Weight

# Create summary statistics for Exam 1 raw values:
minimum=min(sumstat)
maximum=max(sumstat)
avg<-round(mean((sumstat)),digits=1)
sd<-round(sd(sumstat),digits=1)
mediangrade<-median(sumstat)

png(filename="Weightmmsrawhist.png", res=300, bg="white", type="quartz", pointsize=fontsize, width=6, height=6, units="in")

# Histogram
h<-hist(x=sumstat,breaks=4,col="gray", main="Histogram of M&M Bag Weights", cex=1.0, xlab=paste("Weight","of each bag", sep=" "), ylim=c(2,14), xlim=c(47,52))

# Annotation text:
addText(xcoord=48,ycoord=12,avg=avg,sd=sd,med=mediangrade)
text(48,9.7,labels=paste(paste(paste("Range: [",minimum,sep=""),maximum,sep=","),"]",sep=""),cex=1.2)
#text(50,6.75,labels=paste("Raw Min = ",minimum,sep=""),cex=1.4)
#text(50,6.25,labels=paste("Raw Max = ",maximum,sep=""),cex=1.4)

addNorm(data=sumstat,color="red", linewidth="2")

dev.off()



detach(mms)

# EOF