# Laying the foundation for probability distribution theory
# This plot of coin flips makes a nice illustration of 
# a probability distribution function.
#
# heads.tab is generated, with additional useful information, by factorialprobability.plx. 
#
# Jesse Hamner, 2011/2012.

# setwd("")

# graphics margins:
margins<-c(4.5,4.5,1,2)

a<-(read.delim("heads.tab", header=TRUE, sep="\t"))

# first graphic: just the bar plot of counts, possible outcomes, and probability
png(filename="headprobabilities.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")
par(mar=margins)

barplot(a$POSSIBLE, 
	xlab="Count of Heads", 
	ylab="Possible Outcomes With That Many Heads", 
	names.arg=a$HEADCOUNT, 
	col="red",
	bg="black", 
	axes=TRUE, 
#	main="Possible Outcomes of Heads in 10 Coin Flips",
	cex.names=1.2, 
	cex.axis=1.5,
	cex.main=1.5, 
	cex.lab=1.5)
axis(4, at=axTicks(2), label=prettyNum(axTicks(2) /1024,width=4,digits=3 ))
text (13,100,labels="Probability", pos=4, cex=1.5, srt=90, col='blue')

dev.off()


# sets up the normal curve data:
lb		<- -4
ub		<- 4
mean	<- 0
sd 		<- 1
x 		<- seq(lb,ub,length=400)*sd + mean
hx		<- dnorm(x,mean,sd)

# transforms & scales the normal curve data to better match this binomial curve:
x<-(x+5)
hx<-(hx*500)

# second graphic: as first, but with a normal curve overlaid.
png(filename="headprobwithnormal.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")
par(mar=margins)

barplot(a$POSSIBLE, 
	xlab="Count of Heads", 
	ylab="Possible Outcomes With That Many Heads", 
	names.arg=a$HEADCOUNT, 
	col="red",
	bg="black", 
	axes=TRUE, 
#	main="Possible Outcomes of Heads in 10 Coin Flips",
	cex.names=1.2, 
	cex.axis=1.5,
	cex.main=1.5, 
	cex.lab=1.5)
axis(4, at=axTicks(2), label=prettyNum(axTicks(2) /1024,width=4,digits=3 ))
text (13,100,labels="Probability", pos=4, cex=1.5, srt=90, col='blue')

#plot(x, hx, xlab=xlabel, ylab=ylabel, main=mainlabel, axes=FALSE, pch=21, col="blue", bg="blue")
par(new=T)

# prints nothing but re-sets the axes and printing boundaries for the lines() command:
plot(x,hx,xlab="",ylab="",main="",axes=F, pch=21,col=NULL)

# prints the scaled normal curve on top of the binomial curve:
lines(x,hx,lty=1,lwd=3, type="l", col="black")
	
dev.off()