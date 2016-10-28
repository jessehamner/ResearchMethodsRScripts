# logplots and exponential plots

# Set narrow margins for these graphics:
margins<-c(4,4,2,1)

x=seq(0,10,0.05)
y=seq(0,6,0.1)
hx=x**2
hy=y**3
lx=log(x)

png(filename="exponentialplots.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

par(mar=margins)

plot(x,hx, type="l", xlim=c(0,10), ylim=c(0,150), xlab="Value of X", cex.lab=1.4, ylab="Value of Y",pch=21,col="red",lwd=3, main="", cex=1.7) 

# main="Exponential Examples"

par(new=TRUE)
#plot(y,hy, xlab="",ylab="",xlim=c(0,10), ylim=c(0,150) )
lines(y,hy,lty=1,lwd=3)
lines(x,x,lty=2,lwd=2,col="blue")

text(4,100, labels=expression(x^3), cex=1.4)
text(8,72, labels=expression(x^2), cex=1.4)
text(9,15, labels=expression(x^1), cex=1.4)

dev.off()

x1=seq(1,1000000,100)
y1=log10(x1)
y2=log(x1)


png(filename="logarithmplots.png", res=300, bg="white", type="quartz", pointsize=12, width=6, height=6, units="in")

par(mar=margins)
plot(x1,y1, type="l", xlab="Value of X", ylim=c(0,15), cex.lab=1.4, ylab="Value of Y",col="red",lwd=3, main="", cex=1.7)
lines(x1,y2,lty=1,lwd=3)
text(2e+05,6,labels=expression(log[10]~x))
text(2e+05,13,labels="ln(x)" )

dev.off()