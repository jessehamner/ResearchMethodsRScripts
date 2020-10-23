# R plots of distributions

# 2011-2020, Jesse Hamner
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

darkred <- "#990000ff"
mygreen <- "#00aa00ff"
paleblue <- "#bbccee"
paleyellow <- "#eeeeaa"
turquoise <- "#66bbdd"

# Set narrow margins for these graphics:
margins <- c(4,4,2,1)

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

	polygon(c(lb,x[i],ub), c(0,hx[i],0), col=fillcolor, border=fillcolor)
	lines(x,hx, col=linecolor)

return;
} # end of the function


###############################################################
# Sort of a demonstration of the above function -- a simple 
# normal plot from -3 to 3 std devs, mean at zero.
###############################################################


png(filename="normalplot.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

normalPlot(stddev=1,
           lb=-3,
           ub=3,
           xlabel=primaryXTitle,
           ylabel=primaryYTitle,
           mainlabel=primaryTitle,
           fillcolor="gray",
           linecolor="black"
          )

dev.off()


###############################################################
# Another simple bell curve to show that the mean=median=mode 
# for this type of unimodal distribution:
###############################################################

png(filename="unimodalsymmetricplot.png",
    res=300,
    bg="white",
    type="quartz",
    pointsize=12,
    width=6,
    height=6,
    units="in"
   )

normalPlot(stddev = 1,
           lb = -3,
           ub = 3,
           xlabel = "Mean = Median = Mode",
           ylabel = primaryYTitle,
           main = "Unimodal Distribution",
           fillcolor = "gray",
           linecolor = "black"
          )
abline(v = 0,
       h = 1,
       lty = 2,
       col = "blue",
       lwd = 2
      )

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

#rect(-0.75, 0.14, 0.75, 0.16, col="#ddddddaa")
#text(result, x=0, y=0.15) 

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
result <- paste("P(",lb, expression("< " * mu * "<"), ub,") =", signif(area, digits=3))
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
   
#arrows(-4,0.05,4,0.05, code=3)

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

normalPlot(stddev = sd,
           lb = lb,
           ub = ub,
           xlabel = primaryXTitle,
           ylabel = primaryYTitle,
           mainlabel = primaryTitle,
           fillcolor = paleblue,
           linecolor="blue"
          )

x=seq(0, sd, length = 200)
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

png(filename="50pctarea.png", 
    width=5,
    height=4,
    units="in",
    pointsize=12,
    bg="white",
    res=300,
    type="quartz"
   )

par(mar=margins)

sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=ub,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor=paleblue,linecolor="blue")

x=seq(-4,0,length=200)
y=dnorm(x)
polygon(c(lb,x,0),c(0,y,0), col=darkred)

abline(v=0,col=1,lty=3)
text(x=-3, y=0.2, labels=("50% of area\n(probability)\nbelow\nmean"), cex=1.0)

dev.off()



png(filename="50pctareanolabels.png", 
    width=5,
    height=4,
    units="in",
    pointsize=12,
    bg="white",
    res=300,
    type="quartz"
)

par(mar=margins)
sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,lb=lb,ub=ub,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor=paleblue,linecolor="blue")

x=seq(-4,0,length=200)
y=dnorm(x)
polygon(c(lb,x,0),c(0,y,0), col=darkred)
abline(v=0,col=1,lty=3)
rect(-4.1, 0.14, -1.3, 0.26, col="#ddddddaa")
text(x=-2.7, y=0.2, labels=("How much of the\narea (probability)\nlies below\nthe mean?"), cex=0.9)

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

normalPlot(stddev=sd,lb=lb,ub=ub,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel="",fillcolor=paleblue,linecolor="blue")

x=seq(lb,0,length=200)
y=dnorm(x)
polygon(c(lb,x,0),c(0,y,0), col=darkred)

abline(v=0,col=1,lty=3)
# text(x=-3, y=0.2, labels=("50% of area\n(probability)\nbelow\nmean"), cex=1.0)

dev.off()

################################################
#
# This graph demonstrates some hopefully basic
# arithmetic.
#
################################################

png(filename="normalplotarithmetic.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

sd <- 1
x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue", xlab=primaryXTitle,ylab=primaryYTitle,main=primaryTitle)
polygon(c(-4,x,4),c(0,y,0),col=darkred)

x=seq(0,sd,length=200)
y=dnorm(x)
polygon(c(0,x,sd),c(0,y,0), col=paleblue)

abline(v=0,col=1,lty=3)
abline(v=sd,col=1,lty=3)

text(x=2, y=0.3, labels=("34% of area\n between\nmean and 1 s"), cex=1.0)

x1=seq(1,4,length=200)
y1=dnorm(x1)
polygon(c(1,x1,4),c(0,y1,0),col="yellow")
text(x=3,y=0.12, labels=(expression("16% of area between")),cex=0.8)
text(x=3,y=0.1, labels=(expression("1s and " * infinity)), cex=0.8)
# expression(hat(beta) == (X^t * X)^{-1} * X^t * y)
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

normalPlot(stddev=sd,lb=lb,ub=4,xlabel=primaryXTitle, ylabel=primaryYTitle, mainlabel=primaryTitle,fillcolor=paleblue,linecolor="blue")

sd <- 1.645
x=seq(sd,ub,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col=darkred)

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


####################################################
#
# This plot displays the area to the left of z=1.37
#
####################################################


png(filename="z137areaplot.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,
           lb=lb,
           ub=4,
           xlabel=primaryXTitle,
           ylabel=primaryYTitle,
           mainlabel=primaryTitle,
           fillcolor=paleblue,
           linecolor="blue"
          )

sd <- 1.37
x=seq(sd,4,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col=paleyellow)

abline(v=0, col="gray40", lty=2, lwd=1)
abline(v=sd, lty=1, col="gray40")
lines(x=c(1.37, 1.37), y=c(0,y[1]), lwd=3)

rect(-3.8, 0.07, -0.8, 0.17, col="#ddddddaa")
text(x=-2.3, y=0.14, expression("% of area inside"), cex=1.0)
text(x=-2.3, y=0.1, expression("(-" * infinity * ", +1.37s]"), cex=1.0)
text(x=1.9, y=0.17, "1.37s")

dev.off()


png(filename="z137areaplot02.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,
           lb=lb,
           ub=ub,
           xlabel=primaryXTitle,
           ylabel=primaryYTitle,
           mainlabel=primaryTitle,
           fillcolor=paleyellow,
           linecolor="blue"
)

sd <- 1.37
x=seq(sd,4,length=200)
y=dnorm(x)
polygon(c(sd,x,4),c(0,y,0), col=paleblue)
abline(v=sd, lwd=1, col="gray40")
lines(x=c(1.37, 1.37), y=c(0,y[1]), lwd=2)
text(x=0, y=0.15, "0.9147")
#rect(1.7, 0.12, 3.6, 0.17, col="#ddddddaa")
text(x=2.8, y=0.15, "1 - 0.9147")
arrows(x0 = 2.4, x1 = 2, y0 = 0.13, y1 = 0.07, col = "blue", lwd = 1, angle = 15, length = 0.1) 

dev.off()




png(filename="z-203areaplot02.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -4
ub <- 4

normalPlot(stddev=sd,
           lb=lb,
           ub=ub,
           xlabel=primaryXTitle,
           ylabel=primaryYTitle,
           mainlabel=primaryTitle,
           fillcolor=paleyellow,
           linecolor=darkred
)

sd <- -2.03
x <- seq(-4, sd,length=100)
y <- dnorm(x)
polygon(c(lb,x,sd),c(0,y,0), col = paleblue, border="black")
lines(x, y, col=darkred)
abline(v = sd, lwd = 1, col = "gray80")
lines(x = c(sd, sd), y = c(0,y[100]), lwd = 2)
rect(-2.9, 0.17, -1.1, 0.23, col = "#ddddddbb")
text(x = -2, y = 0.2, "z = -2.03")
text(x=-3.2, y=0.1, "Area =\n0.0212", cex=0.8)
arrows(x0=-3, x1=-2.5, y0=0.06, y1 = 0.02, col="blue", lwd=1, angle=15, length=0.1) 
dev.off()



####################################################
#
# Make a symmetric, but not-normal distribution
#
####################################################

sd <- 1
ub <- 7
lb <- -7
mean <- 0
x <- seq(lb, ub , length = 400) * sd + mean
x1 <- seq(-3, 3 , length = 200) * sd + mean

hx <- dnorm(x, mean, sd)
hxx <- dnorm(x1, mean, sd)  * 1.5

# create a 1-column matrix and add zero-value columns
# to it
q <- matrix(data=x, nrow=length(x), ncol=1)
q <- cbind(q, hx)
#q[,2] <- hx
q <- cbind(q, 0)
q[201:400,3] <- hxx
q <- cbind(q, 0)
q[1:200,4] <- hxx
q <- cbind(q, 0)
q[,5] <- q[,2] + (q[,3] + q[,4])


png(filename="symmetricnotnormal.png",
    width=5,
    height=4,
    units="in",
    pointsize=12,
    bg="white",
    res=300, type="quartz"
   )

plot(q[,1], q[,5], 
     xlab = '', ylab = '', 
     ylim = c(0,0.7), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

polygon(c(lb, q[,1], ub),
        c(0, q[,5], 0),
        col = paleyellow,
        border = darkred
       )
axis(side = 1, at = c(-7,7), labels = NA)
axis(side = 2, at = c(0,0.7), labels = NA)
dev.off()


png(filename="multiplenormalplots.png", 
    width=5,
    height=4,
    units="in",
    pointsize=12,
    bg="white",
    res=300,
    type="quartz"
   )

sd <- 1
lb <- -8
ub <- 8
mean <- 0
x <- seq(lb, ub , length = 400) * sd + mean
hxiv <- dnorm(x, mean, sd)

plot(x, 
     hxiv,
     type="l",
     xlab="",
     ylab="Probability Density",
     main = "", 
     axes = TRUE,
     xlim = c(lb, ub),
     bg = paleyellow,
     col = darkred)
polygon(c(lb,x,ub),c(0,hxiv,0), col=paleyellow, border=darkred)

hx <- dnorm(x, mean, sd * 2)
hxx <- dnorm(x, mean, sd * 3.5) * 0.9

polygon(c(lb,x,ub),c(0,hx,0), col=paleyellow, border=paleyellow)
polygon(c(lb,x,ub),c(0,hxx,0), col=paleyellow, border=paleyellow)

mean <- -4
x1 <- seq(lb, ub , length = 400) * sd + mean
hxxx <- dnorm(x1, mean, sd * 1.25) * 0.75
polygon(c(lb,x1,ub),c(0,hxxx,0), col=paleyellow, border=paleyellow)

lines(x, hx, col="darkgreen", lwd=2)
lines(x, hxx, col="purple", lwd=2)
lines(x1, hxxx, col="blue", lwd=2)
lines(x, hxiv, col=darkred, lwd=2)


dev.off()


png(filename="1sigmanormalplot.png", 
    width=5,
    height=4,
    units="in",
    pointsize=12,
    bg="white",
    res=300,
    type="quartz"
)
qmat <- matrix(data=c(c(-1,0,1), c(-1,0,1), c(0,0,0), dnorm(c(-1,0,1), 0, 1)), byrow=FALSE, nrow=3, ncol=4)

par(mai=c(0.5,0.25,0.25,0.25), cex.lab = 1.2)
sd <- 1
lb <- -4
ub <- 4
mean <- 0
x <- seq(lb, ub , length = 400) * sd + mean
hx <- dnorm(x, mean, sd)
greeklabels <- c(expression(mu * " + " * sigma),
                 expression(mu),
                 expression(mu * " - " * sigma))

plot(x, 
     hxiv,
     type="l",
     xlab="",
     ylab="Probability Density",
     main = "", 
     axes = FALSE,
     xlim = c(lb, ub),
     ylim = c(0, 0.45),
     bg = paleyellow,
     col = darkred
    )

polygon(c(lb,x,ub),c(0,hx,0), col=paleyellow, border="black")
lines(x, hx, col=darkred, lwd=2)
lines (x=qmat[1,1:2], y=qmat[1,3:4], lty=2, col="gray50", lwd=2)
lines (x=qmat[2,1:2], y=qmat[2,3:4], lty=2, col="gray50", lwd=2)
lines (x=qmat[3,1:2], y=qmat[3,3:4], lty=2, col="gray50", lwd=2)
axis(side=1, at=c(-1,0,1), tick=TRUE, outer = FALSE, pos=0, padj=1, labels=greeklabels, cex=1.2)

dev.off()





png(filename="1stddevnormalplot.png", 
    width=5,
    height=4,
    units="in",
    pointsize=12,
    bg="white",
    res=300,
    type="quartz"
)
par(mai=c(0.5,0.25,0.25,0.25), cex.lab = 1.2)
sd <- 1
lb <- -4
ub <- 4
mean <- 0
x <- seq(lb, ub , length = 400) * sd + mean
hx <- dnorm(x, mean, sd)
samplelabels <- c(-1,0,1)

qmat <- matrix(data=c(c(-1,0,1), c(-1,0,1), c(0,0,0), dnorm(c(-1,0,1), 0, 1)), byrow=FALSE, nrow=3, ncol=4)

plot(x, 
     hxiv,
     type="l",
     xlab="",
     ylab="Probability Density",
     main = "", 
     axes = FALSE,
     xlim = c(lb, ub),
     ylim = c(0, 0.45),
     bg = paleyellow,
     col = darkred
)

polygon(c(lb,x,ub),c(0,hx,0), col=paleyellow, border="black")
lines(x, hx, col=darkred, lwd=2)
lines (x=qmat[1,1:2], y=qmat[1,3:4], lty=2, col="gray50", lwd=2)
lines (x=qmat[2,1:2], y=qmat[2,3:4], lty=2, col="gray50", lwd=2)
lines (x=qmat[3,1:2], y=qmat[3,3:4], lty=2, col="gray50", lwd=2)

axis(side=1, at=c(-1,0,1), tick=TRUE, outer = FALSE, pos=0, padj=1, labels=samplelabels, cex=1.2)

dev.off()







png(filename="z1mirror01.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

x1 <- seq(sd, ub, length=100)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd*-1, 0, sd), labels = c("-z", "0", "z"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(sd,x1,ub),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

lines(x = c(sd,sd), y= c(0, dnorm(sd)), lwd = 2, col = turquoise)
lines(x = c(sd * -1 , sd * -1), y= c(0, dnorm(sd * -1)), lwd = 2, col = turquoise)

text(x = 2, y = 0.23, "Area to the")
text(x=2, y=0.185, "right of z", cex=1)
arrows(x0=2, x1=1.6, y0=0.17, y1 = 0.09, col="blue", lwd=1, angle=15, length=0.1) 
dev.off()


png(filename="z1mirror02.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

x1 <- seq(lb, sd*-1, length=100)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd*-1, 0, sd), labels = c("-z", "0", "z"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(lb,x1,sd*-1),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

lines(x = c(sd,sd), y= c(0, dnorm(sd)), lwd = 2, col = turquoise)
lines(x = c(sd * -1 , sd * -1), y= c(0, dnorm(sd * -1)), lwd = 2, col = turquoise)

text(x = -2, y = 0.23, "Area to the")
text(x = -2, y = 0.185, "left of z", cex = 1)
arrows(x0 = -2, x1 = -1.6, y0 = 0.17, y1 = 0.09, col = "blue", lwd = 1, angle = 15, length = 0.1) 
dev.off()



png(filename="z1mirror03.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

x1 <- seq(sd*-1, sd, length=100)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd*-1, 0, sd), labels = c("-z", "0", "z"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(sd*-1, x1, sd),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

lines(x = c(sd,sd), y= c(0, dnorm(sd)), lwd = 2, col = turquoise)
lines(x = c(sd * -1 , sd * -1), y= c(0, dnorm(sd * -1)), lwd = 2, col = turquoise)

text(x = -2, y = 0.23, "Area to the")
text(x = -2, y = 0.185, "left of z", cex = 1)
arrows(x0 = -2, x1 = -1.6, y0 = 0.17, y1 = 0.09, col = "blue", lwd = 1, angle = 15, length = 0.1) 
dev.off()




png(filename="z1mirror04.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 2
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

x1 <- seq(lb, sd*-1, length=100)
y1 <- dnorm(x1)

x2 <- seq(sd, ub, length=100)
y2 <- dnorm(x2)


plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd*-1, 0, sd), labels = c("-z", "0", "z"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(lb, x1, sd * -1),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

polygon(c(sd, x2, ub),c(0,y2,0), col = paleblue, border = paleblue)
lines(x2, y2, col=darkred)

lines(x = c(sd,sd), y= c(0, dnorm(sd)), lwd = 2, col = turquoise)
lines(x = c(sd * -1 , sd * -1), y= c(0, dnorm(sd * -1)), lwd = 2, col = turquoise)

text(x = -2, y = 0.23, "Area to the")
text(x = -2, y = 0.185, "left of z", cex = 1)
arrows(x0 = -2, x1 = -1.6, y0 = 0.17, y1 = 0.09, col = "blue", lwd = 1, angle = 15, length = 0.1) 
dev.off()



png(filename="normaltemp01.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

sd1 <- -2.33
x1 <- seq(lb, sd1, length=100)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd1, 0, sd1 * -1), labels = c("-2.33", "0", "2.33"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(lb,x1,sd1),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

lines(x = c(sd1,sd1), y= c(0, dnorm(sd1)), lwd = 2, col = turquoise)

#text(x = 2, y = 0.23, "Area to the")
#text(x=2, y=0.185, "right of z", cex=1)
#arrows(x0=2, x1=1.6, y0=0.17, y1 = 0.09, col="blue", lwd=1, angle=15, length=0.1) 
dev.off()


png(filename="normaltemp02.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

sd1 <- -2.33
x1 <- seq(lb, sd1, length=100)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd1, 0, sd1 * -1), labels = c("96.9", "98.6", ""))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(lb,x1,sd1),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)
lines(x = c(sd1,sd1), y= c(0, dnorm(sd1)), lwd = 2, col = turquoise)

dev.off()







png(filename="normaltemp03.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

sd1 <- -1.37
x1 <- seq(sd1, ub, length=200)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd1, 0), labels = c("-1.37", "0"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(sd1,x1,ub),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

lines(x = c(sd1,sd1), y= c(0, dnorm(sd1)), lwd = 2, col = turquoise)
dev.off()


png(filename="normaltemp04.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
sd <- 1
lb <- -3
ub <- 3
mean <- 0

x <- seq(lb, ub, length=200)
y <- dnorm(x)

sd1 <- -1.37
x1 <- seq(sd1, ub, length=200)
y1 <- dnorm(x1)

plot(x, y, 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = c(sd1, 0), labels = c("97.6", "98.6"))
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,x,ub),c(0,y,0), col = paleyellow, border = paleyellow)
lines(x, y, col=darkred)

polygon(c(sd1,x1,ub),c(0,y1,0), col = paleblue, border = paleblue)
lines(x1, y1, col=darkred)

lines(x = c(sd1,sd1), y= c(0, dnorm(sd1)), lwd = 2, col = turquoise)
dev.off()





png(filename="normaltemp05.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
sd <- 1
lb <- -3
ub <- 3
mean <- 0
sd1 <- -0.82
sd2 <- 0.55
templabels <- c("-0.82", "0", "0.55")
labelplacement <- c(sd1, mean, sd2)

x <- seq(lb, ub, length=200)
qmat <- matrix(data=x, ncol=1)
qmat <- cbind(qmat, dnorm(qmat[,1]))
qmat <- cbind(qmat, seq(lb, sd1, length=200))
qmat <- cbind(qmat, dnorm(qmat[,3]))
qmat <- cbind(qmat, seq(sd2, ub, length=200))
qmat <- cbind(qmat, dnorm(qmat[,5]))

plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleblue, border = paleblue)
polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = paleyellow, border = paleyellow)
polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleyellow, border = paleyellow)

lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

dev.off()


png(filename="normaltemp06.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
sd <- 1
lb <- -3
ub <- 3
mean <- 0
sd1 <- -0.82
sd2 <- 0.55
templabels <- c("98", "98.6", "99")
labelplacement <- c(sd1, mean, sd2)

x <- seq(lb, ub, length=200)
qmat <- matrix(data=x, ncol=1)
qmat <- cbind(qmat, dnorm(qmat[,1]))
qmat <- cbind(qmat, seq(lb, sd1, length=200))
qmat <- cbind(qmat, dnorm(qmat[,3]))
qmat <- cbind(qmat, seq(sd2, ub, length=200))
qmat <- cbind(qmat, dnorm(qmat[,5]))

plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleblue, border = paleblue)
polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = paleyellow, border = paleyellow)
polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleyellow, border = paleyellow)

lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

dev.off()


png(filename="normaltemp05a.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
sd <- 1
lb <- -3
ub <- 3
mean <- 0
sd1 <- -0.82
sd2 <- 0.55
templabels <- c("-0.82", "0", "0.55")
labelplacement <- c(sd1, mean, sd2)

x <- seq(lb, ub, length=200)
qmat <- matrix(data=x, ncol=1)
qmat <- cbind(qmat, dnorm(qmat[,1]))
qmat <- cbind(qmat, seq(lb, sd1, length=200))
qmat <- cbind(qmat, dnorm(qmat[,3]))
qmat <- cbind(qmat, seq(sd2, ub, length=200))
qmat <- cbind(qmat, dnorm(qmat[,5]))

plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleblue, border = paleblue)
polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = "#88ff8877", border = "#88ff8877")
polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleyellow, border = paleyellow)

lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
abline(v=0.55, lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

arrows(x0 = -0.82, x1 = -3, y0 = 0.28, y1 = 0.28, col = "#88ff88", lwd = 1, angle = 15, length=0.1)
text(x = -2, y=0.25, "Area = 0.2061")

arrows(x0 = 0.55, x1 = -3, y0 = 0.42, y1 = 0.42, col = "blue", lwd = 1, angle = 15, length=0.1)
text(x = -0.42, y=0.45, "Area = 0.7088")

arrows(x0 = 0.55, x1 = 3, y0 = 0.35, y1 = 0.35, col = "orange", lwd = 1, angle = 15, length=0.1)
text(x = 2, y=0.3, "Area =\n1 - 0.7088")

dev.off()



png(filename="normalareabetween.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
sd <- 1
lb <- -3
ub <- 3
mean <- 0
sd1 <- -1.25
sd2 <- sd1 * -1
templabels <- c("Z1", "0", "Z2")
labelplacement <- c(sd1, mean, sd2)

x <- seq(lb, ub, length=200)
qmat <- matrix(data=x, ncol=1)
qmat <- cbind(qmat, dnorm(qmat[,1]))
qmat <- cbind(qmat, seq(lb, sd1, length=200))
qmat <- cbind(qmat, dnorm(qmat[,3]))
qmat <- cbind(qmat, seq(sd2, ub, length=200))
qmat <- cbind(qmat, dnorm(qmat[,5]))

plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleblue, border = paleblue)
polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = paleyellow, border = paleyellow)
polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleyellow, border = paleyellow)

lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

arrows(x0 = -1.5, x1 = -0.8, y0 = 0.24, y1 = 0.1, col = "black", lwd = 1, angle = 15, length=0.1)
text(x = -1.6, y=0.26, "?")

dev.off()


png(filename="normalareaoutside.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
sd <- 1
lb <- -3
ub <- 3
mean <- 0
sd1 <- -1.25
sd2 <- sd1 * -1
templabels <- c("Z1", "0", "Z2")
labelplacement <- c(sd1, mean, sd2)

x <- seq(lb, ub, length=200)
qmat <- matrix(data=x, ncol=1)
qmat <- cbind(qmat, dnorm(qmat[,1]))
qmat <- cbind(qmat, seq(lb, sd1, length=200))
qmat <- cbind(qmat, dnorm(qmat[,3]))
qmat <- cbind(qmat, seq(sd2, ub, length=200))
qmat <- cbind(qmat, dnorm(qmat[,5]))

plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleyellow, border = paleyellow)
polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = paleblue, border = paleblue)
polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleblue, border = paleblue)

lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

arrows(x0 = -2.3, x1 = -1.8, y0 = 0.11, y1 = 0.04, col = "black", lwd = 1, angle = 15, length=0.1)
arrows(x0 = 2.3, x1 = 1.8, y0 = 0.11, y1 = 0.04, col = "black", lwd = 1, angle = 15, length=0.1)
text(x = -2.4, y=0.13, "?")
text(x = 2.4, y=0.13, "?")

dev.off()



png(filename="normalarealeft.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
sd <- 1
lb <- -3
ub <- 3
mean <- 0
sd1 <- -1.25
sd2 <- sd1 * -1
templabels <- c("Z0", "0")
labelplacement <- c(sd1, mean)

x <- seq(lb, ub, length=200)
qmat <- matrix(data=x, ncol=1)
qmat <- cbind(qmat, dnorm(qmat[,1]))
qmat <- cbind(qmat, seq(lb, sd1, length=200))
qmat <- cbind(qmat, dnorm(qmat[,3]))
qmat <- cbind(qmat, seq(sd2, ub, length=200))
qmat <- cbind(qmat, dnorm(qmat[,5]))

plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleyellow, border = paleyellow)
polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = paleblue, border = paleblue)
# polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleblue, border = paleblue)

lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
# lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

arrows(x0 = -2.3, x1 = -1.8, y0 = 0.11, y1 = 0.04, col = "black", lwd = 1, angle = 15, length=0.1)
# arrows(x0 = 2.3, x1 = 1.8, y0 = 0.11, y1 = 0.04, col = "black", lwd = 1, angle = 15, length=0.1)
text(x = -2.4, y=0.13, "?")
# text(x = 2.4, y=0.13, "?")

dev.off()


png(filename="normalarearight.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
margins <- margins
plot(qmat[,1], qmat[,2], 
     xlab = '', ylab = '',
     xlim = c(lb - 0.1, ub + 0.1),
     ylim = c(0,0.45), axes = FALSE,
     type = "l", 
     col = darkred, 
     lwd = 2)
templabels <- c("0", "Z0")
labelplacement <- c(mean, sd2)

axis(side = 1, at = labelplacement, labels = templabels, cex.axis=0.7)
axis(side = 1, at = c(lb, ub), labels = NA, tick = TRUE, lwd.ticks=0)

polygon(c(lb,qmat[,1],ub),c(0,qmat[,2],0), col = paleyellow, border = paleyellow)
# polygon(c(lb, qmat[,3], sd1),c(0,qmat[,4],0), col = paleblue, border = paleblue)
polygon(c(sd2,qmat[,5],ub),c(0,qmat[,6],0), col = paleblue, border = paleblue)

#lines(x = c(sd1, sd1), y = c(0, dnorm(sd1)), lwd = 2, col = turquoise)
lines(x = c(sd2, sd2), y = c(0, dnorm(sd2)), lwd = 2, col = turquoise)
lines(qmat[,1], qmat[,2], col = darkred, lwd = 2)

# arrows(x0 = -2.3, x1 = -1.8, y0 = 0.11, y1 = 0.04, col = "black", lwd = 1, angle = 15, length=0.1)
arrows(x0 = 2.3, x1 = 1.8, y0 = 0.11, y1 = 0.04, col = "black", lwd = 1, angle = 15, length=0.1)
# text(x = -2.4, y=0.13, "?")
text(x = 2.4, y=0.13, "?")

dev.off()


