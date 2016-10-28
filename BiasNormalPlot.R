
# Set narrow margins for these graphics:
margins<-c(4,4,2,1)


###############################################################
# Make a normal plot with a biased plot of similar shape just below it:
###############################################################


png(filename="biasednormalplots.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

par(new=FALSE)
par(mar=margins)

ub=4
lb=-5
sd=1
xlabel="Value of some parameter X"
ylabel="Likelihood of value X occurring"
mean=0
linecolor="blue"
fillcolor="gray"

x <- seq(lb,ub,length=200)*sd + mean
hx <- dnorm(x,mean,sd)
i <- x >= lb & x <= ub

plot(x, hx, type="n", xlab=xlabel, ylab=ylabel, main=mainlabel, axes=TRUE, col=linecolor)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=fillcolor)
lines(x,hx, col=linecolor)

# Next (here, biased) normal plot:

par(new=TRUE)
x=seq(-6,4,length=200)
y=dnorm(x, mean=-1, sd=1.2)
polygon(c(-6,x,4),c(0,(y*1),0),col="gray90")
lines(x,(y*1), type="l", lwd=3, col="red")

lb <- -6
ub <- -0.6
x=seq(lb,ub,length=200)
hx=dnorm(x,mean=0,sd=1)
# polygon(c(-6,x,-0.5),c(0,hx,0), col="blue", lty=2)
lines(x,hx,type="l", lty=2,lwd=1,col="blue")

# add text and vertical lines:
abline(v=0,col="blue")
text(0.2,0.3,labels="True avg value of X", srt=90, cex=1.4)

abline(v=-1,col="red")
text(-1.2,0.2,labels="Measured avg value of X", srt=90, cex=1.4)

dev.off()





png(filename="normalsampleplots.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

par(new=FALSE)
par(mar=margins)

ub=4
lb=-5
sd=1
xlabel="Value of some parameter X"
ylabel="Likelihood of value X occurring"
mean=0
linecolor="blue"
fillcolor="gray"

x <- seq(lb,ub,length=200)*sd + mean
hx <- dnorm(x,mean,sd)
i <- x >= lb & x <= ub

plot(x, hx, type="n", xlab=xlabel, ylab=ylabel, main=mainlabel, axes=TRUE, col=linecolor)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=fillcolor)
lines(x,hx, col=linecolor)

# Next (here, biased) normal plot:

par(new=TRUE)
x=seq(-6,4,length=200)
y=dnorm(x, mean=-1, sd=1.2)
polygon(c(-6,x,4),c(0,(y*1),0),col="gray90")
lines(x,(y*1), type="l", lwd=3, col="red")

lb <- -6
ub <- -0.6
x=seq(lb,ub,length=200)
hx=dnorm(x,mean=0,sd=1)
# polygon(c(-6,x,-0.5),c(0,hx,0), col="blue", lty=2)
lines(x,hx,type="l", lty=2,lwd=1,col="blue")

# add text and vertical lines:
abline(v=0,col="blue")
text(0.3,0.2,labels="Avg value of X population", srt=90, cex=1.4)

abline(v=-1,col="red")
text(-1.2,0.2,labels="Sample avg value of X", srt=90, cex=1.4)

dev.off()


png(filename="abnormalsampleplots.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

par(new=FALSE)
par(mar=margins)

ub=4
lb=-5
sd=1
xlabel="Value of some parameter X"
ylabel="Likelihood of value X occurring"
mean=0
linecolor="blue"
fillcolor="gray"

x <- seq(lb,ub,length=200)*sd + mean
hx <- dnorm(x,mean,sd)
i <- x >= lb & x <= ub

plot(x, hx, type="n", xlab=xlabel, ylab=ylabel, main=mainlabel, axes=TRUE, col=linecolor)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=fillcolor)
lines(x,hx, col=linecolor)

# Next (here, biased) normal plot:

par(new=TRUE)
ymult=1
x=seq(-6,2,length=200)
y=dnorm(x, mean=-3, sd=1.3)
polygon(c(-6,x,-3),c(0,(y*ymult),0),col="gray90")
lines(x,(y*ymult), type="l", lwd=3, col="red")

lb <- -6
ub <- -0.6
x=seq(lb,ub,length=200)
hx=dnorm(x,mean=0,sd=1)
# polygon(c(-6,x,-0.5),c(0,hx,0), col="blue", lty=2)
lines(x,hx,type="l", lty=2,lwd=1,col="blue")

# add text and vertical lines:
abline(v=0,col="blue")
text(0.3,0.2,labels="Avg value of X population", srt=90, cex=1.4)

abline(v=-3,col="red")
text(-2.6,0.15,labels="Sample avg value of X", srt=90, cex=1.4)

dev.off()


