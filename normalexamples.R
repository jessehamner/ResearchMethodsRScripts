# R plots of distributions

# 2011-2012, Jesse Hamner
# Much of this R code was assembled from multiple sources
# in the public domain. The remainder (and any errors) are mine.
#
# This script creates a series of varied normal plots, with 
# variations intended to instruct students about probability and 
# the normal distribution itself. 
#
# This series of graphics would likely not be used in its entirety, 
# at least, not all on the same day. The majority of these graphics
# would likely follow the section on coin flips, found in 
# headplot.R (a separate script, elsewhere in this folder).
#

# To keep titles consistent, here's three suggested labels:
primaryTitle<-("Normal Distribution")
primaryXTitle<-("Standard Deviations Above or Below the Mean")
primaryYTitle<-("Probability Density")

# Set narrow margins for these graphics:
margins<-c(4,4,2,1)

###############################################################
# A custom function to draw fairly basic normal plots.
# This function takes a few arguments, but it's designed to
# also give a very consistent feel across plots.
###############################################################

normalPlot <- function(stddev,lb,ub,xlabel,ylabel,mainlabel,fillcolor,linecolor) {
	mean=0
	sd <- stddev
	x <- seq(lb,ub,length=200)*sd + mean
	hx <- dnorm(x,mean,sd)
	i <- x >= lb & x <= ub

	if ( linecolor=="none" | linecolor==0 | linecolor=="" | linecolor=="null" ) {
		linecolor<-("black")	
	}	

	plot(x, hx, type="n", xlab=xlabel, ylab=ylabel, main=mainlabel, 
		axes=TRUE, col=linecolor)
	
	if ( fillcolor=="none" | fillcolor=="null" | fillcolor=="") {
		fillcolor<-("white")	
	}	

	polygon(c(lb,x[i],ub), c(0,hx[i],0), col=fillcolor)
	lines(x,hx, col=linecolor)

return;
} # end of the function


###############################################################
# Sort of a demonstration of the above function -- a simple 
# normal plot from -3 to 3 std devs, mean at zero.
###############################################################


png(filename="normalplot.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

normalPlot(stddev=1,lb=-3,ub=3,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="gray",linecolor="black")

dev.off()


###############################################################
# Another simple bell curve to show that the mean=median=mode 
# for this type of unimodal distribution:
###############################################################

png(filename="unimodalsymmetricplot.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

normalPlot(stddev=1,lb=-3,ub=3,xlabel="Mean = Median = Mode", ylabel=primaryYTitle,main="Unimodal Distribution",fillcolor="gray", linecolor="black")
abline(v=0, h=1, col="blue")

dev.off()


###############################################################
# A normal plot with colored blocks between standard deviations:
###############################################################

png(filename="normalplotwithsd.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

mean=0
sd=1
x <- seq(-3,3,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab=primaryXTitle, ylab=primaryYTitle, main=primaryTitle, axes=TRUE)
lines(x,hx)

#
# build layers of standard deviation on top of each other:
#
# from -3 to +3 s (red):
lb=-3; ub=3
x <- seq(-3,3,length=400)*sd + mean
hx <- dnorm(x,mean,sd)
lines(x, hx)
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< mean <",ub,") =",
   signif(area, digits=3))
#mtext(result,1) 

# from -2 to +2 s (light blue):
lb=-2; ub=2
x <- seq(-3,3,length=400)*sd + mean
hx <- dnorm(x,mean,sd)
lines(x, hx)
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="light blue")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< mean <",ub,") =",
   signif(area, digits=3))
#mtext(result,1) 

# from -1 to +1 s (dark green):
lb=-1; ub=1
x <- seq(-3,3,length=400)*sd + mean
hx <- dnorm(x,mean,sd)
lines(x, hx)
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="dark green")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< mean <",ub,") =",
   signif(area, digits=3))
#mtext(result,2) 

dev.off()


######################################################
#
# This graph displays probability between 1.96 and 4 SD
# (95th percentile of probability for a two-tailed test)
#
######################################################

png(filename="normalplot95pctile.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

normalPlot(stddev=1,lb=-4,ub=4,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="lightgray", linecolor="black")

lb=-4; ub=4
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="light gray")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< mean <",ub,") =", signif(area, digits=3))
# mtext(result,1) 


mean=0
sd=1
lb=1.96; ub=4

x1 <- seq(lb,ub,length=100)*sd + mean
hx1 <- dnorm(x1,mean,sd)
i <- x1 >= lb & x1 <= ub

lines(x1, hx1)
polygon(c(lb,x1[i],ub), c(0,hx1[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< mean <",ub,") =", signif(area, digits=3))
   
arrows(-4,0.05,4,0.05, code=3)

dev.off()

######################################################
#
# This graph displays probability between 0 and 1 SD
# (34th percentile of probability), or, here,
# 34% of the total probability distribution function area
#
######################################################

png(filename="34pctarea.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=ub,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="cyan",linecolor="blue")

x=seq(0,sd,length=200)
y=dnorm(x)
polygon(c(0,x,1),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=3)

text(x=3, y=0.2, labels=("34% of area\n between\nmean and 1 s"), cex=1.0)

dev.off()

######################################################
#
# This graph displays probability between -infinity and 0 SD,
# 50% of the total probability distribution function area
#
######################################################

png(filename="50pctarea.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

par(mar=margins)

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=ub,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="cyan",linecolor="blue")

x=seq(-4,0,length=200)
y=dnorm(x)
polygon(c(lb,x,0),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
text(x=-3, y=0.2, labels=("50% of area\n(probability)\nbelow\nmean"), cex=1.0)

dev.off()


######################################################
#
# This graph displays probability between -3 and 0 SD, nearly
# 50% of the total probability distribution function area
#
######################################################

png(filename="49pctarea.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

par(mar=margins)

sd <- 1
lb <- -3
ub <- 3

normalPlot(stddev=sd,lb=lb,ub=ub,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel="",fillcolor="cyan",linecolor="blue")

x=seq(lb,0,length=200)
y=dnorm(x)
polygon(c(lb,x,0),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
# text(x=-3, y=0.2, labels=("50% of area\n(probability)\nbelow\nmean"), cex=1.0)

dev.off()

################################################
#
# This graph hopefully demonstrates some basic
# fucking arithmetic to the little darlings.
#
################################################

png(filename="normalplotarithmetic.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")


sd <- 1
x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue", xlab=primaryXTitle,ylab=primaryYTitle,main=primaryTitle)
polygon(c(-4,x,4),c(0,y,0),col="cyan")

x=seq(0,sd,length=200)
y=dnorm(x)
polygon(c(0,x,sd),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=3)

text(x=2, y=0.3, labels=("34% of area\n between\nmean and 1 s"), cex=1.0)

x1=seq(1,4,length=200)
y1=dnorm(x1)
polygon(c(1,x1,4),c(0,y1,0),col="navy")
text(x=3,y=0.1, labels=("16% of area\n between\n 1s and 4s"),cex=1.0)

dev.off()

################################################
#
# This plot shows 95 percent of the total probability
# up to 1.645 s, i.e. like a one-tailed test
#
################################################
png(filename="95pctarea.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=4,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="cyan",linecolor="blue")

sd <- 1.645
x=seq(sd,ub,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=1)
#abline(v=-1.96,col=1,lty=1)

#text(x=0, y=0.2, labels=("Cannot reject\n null hypothesis"), cex=1.0)
text(x=3, y=0.2, labels=("5% of area\nabove 1.645 s"), cex=1.0)
#text(x=-3, y=0.2, labels=("p<0.05,\nReject null"), cex=1.0)

dev.off()

################################################
#
# This plot shows 90 percent of the total probability
# with two 5% tails (outside of +/- 1.645 standard deviations)
#
################################################

png(filename="90pctarea.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=4,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="cyan",linecolor="blue")

sd <- 1.645
x=seq(sd,4,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col="red")

x=seq(-4,-(sd),length=200)
y=dnorm(x)
polygon(c(-4,x,-(sd)),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=1)
abline(v=-(sd),col=1,lty=1)

text(x=0, y=0.2, labels=("90% of\narea inside\n[-1.65s,+1.65s]"), cex=1.0)
text(x=3, y=0.1, labels=("5% here"), cex=1.0)
text(x=-3, y=0.1, labels=("5% here"), cex=1.0)

dev.off()

################################################
#
# This plot shows 95 percent of the total 
# probability with two 2.5% tails (outside of
# +/- 1.96 standard deviations)
#
################################################

png(filename="95pctconfidence.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=4,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="cyan",linecolor="blue")

sd <- 1.96

x=seq(sd,4,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col="red")

x=seq(-4,-(sd),length=200)
y=dnorm(x)
polygon(c(-4,x,-(sd)),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=1)
abline(v=-(sd),col=1,lty=1)

text(x=0, y=0.2, labels=("95% of\narea inside\n[-1.96s,+1.96s]"), cex=1.0)
text(x=3, y=0.1, labels=("2.5% here"), cex=1.0)
text(x=-3, y=0.1, labels=("2.5% here"), cex=1.0)

dev.off()

################################################
#
# This plot overlays multiple normal plots
# to help students visualize *sample* versus
# *population* distribution
#
################################################

png(filename="multiplesamples.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

sd <- 1
x=seq(-6,6,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=3, col="blue", ylab="Probability Density", main="Population versus Samples")

x=seq(-6,6,length=200)
y=dnorm(x)
polygon(c(-4,x,4),c(0,y,0),col="cyan")

#x=seq(0,sd,length=200)
#y=dnorm(x)
#polygon(c(0,x,sd),c(0,y,0), col="red")


x=seq(-1,6,length=200)
y=dnorm(x, mean=3, sd=0.5)
#polygon(c(-3,x,3),c(0,y,0),col="green")
lines(x,(y*0.3), type="l", lwd=3, col="black")


x=seq(-5,4,length=200)
y=dnorm(x, mean=-1, sd=1.2)
polygon(c(-5,x,4),c(0,(y*0.4),0),col="gray")
lines(x,(y*0.4), type="l", lwd=3, col="red")

abline(v=-1,col=1,lty=3)
abline(v=3,col=1,lty=3)
abline(v=0, lty=2, col="blue")

x=seq(-4,-1.5,length=200)
y=dnorm(x)
lines(x,y,type="l", lty=2, lwd=1, col="blue")


text(x=0, y=0.18, labels=("Overall\nPopulation"), cex=1.0)
text(x=4.7, y=0.2, labels=("Sample B"), cex=1.0)
text(x=-1, y=0.05, labels=("Sample A"), cex=1.0)

dev.off()


###############################################################
# Simplistic labels on the normal plot:
###############################################################

png(filename="normalplot_simplelabels.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

par(mar=margins)

normalPlot(stddev=1,lb=-3,ub=3,xlabel="Value of some parameter X", ylabel="Likelihood of value X occurring", mainlabel="",fillcolor="gray",linecolor="black")
abline(v=0,col="blue")
text(0.2,0.2,labels="Average value of X", srt=90, cex=1.4)

dev.off()


################################################
#
# This plot shows 95 percent of the total 
# probability with two 2.5% tails (outside of
# +/- 1.96 standard deviations)
#
################################################

png(filename="95pctconfidencePSCIsample.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=4,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor="cyan",linecolor="blue")

sd <- 1.96

x=seq(sd,4,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col="red")

x=seq(-4,-(sd),length=200)
y=dnorm(x)
polygon(c(-4,x,-(sd)),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=1)
abline(v=-(sd),col=1,lty=1)

#text(x=0, y=0.2, labels=("95% of\narea inside\n[-1.96s,+1.96s]"), cex=1.0)
text(x=-0.09, y=0.15, labels="10% would be\nmajors (µ)", cex=1.0, srt=90)
text(x=3.1, y=0.15, labels=("more than\n12.6% would\nbe majors"), cex=1.0)
text(x=-3, y=0.15, labels=("less than\n7.4% would\nbe majors"), cex=1.0)

dev.off()



