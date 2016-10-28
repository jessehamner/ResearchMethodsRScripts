png(filename="5percenttails.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )

x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(-4,1.96,length=200)
y=dnorm(x)
polygon(c(-4,x,1.96),c(0,y,0),col="cyan")

x=seq(1.96,4,length=200)
y=dnorm(x)
polygon(c(1.96,x,4),c(0,y,0), col="red")

x=seq(-4,-1.96,length=200)
y=dnorm(x)
polygon(c(-4,x,-1.96),c(0,y,0), col="red")

abline(v=0,col=1,lty=3)
abline(v=1.96,col=1,lty=1)
abline(v=-1.96,col=1,lty=1)

text(x=0, y=0.1, labels=("Cannot reject\n null hypothesis"), cex=1.0)
text(x=3, y=0.11, labels=(expression(paste(alpha < 0.05))), cex=1.0)
text(x=3,y=0.095,labels="\n(Reject null)")

text(x=-3, y=0.11, labels=(expression(paste(alpha < 0.05))), cex=1.0)
text(x=-3,y=0.095,labels="\n(Reject null)")

dev.off()