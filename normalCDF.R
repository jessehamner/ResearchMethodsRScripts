library(stats)
require(graphics)

darkred <- "#990000ff"
mygreen <- "#00aa00ff"
alphafill <- rgb(0, 0, 1,0.2)

par(cex=1.25)

xseq <- seq(-4,4,0.005)
normcdf <- pnorm(xseq, mean=0, sd=1)

# If you want to lay a standard normal PDF on top of a plot:
plotGaussianPDF <- function() {
  mean=0
  sd <- 1
  xvalues <- xseq*sd + mean
  hx <- dnorm(xvalues,mean,sd)
  i <- xvalues >= -4 & xvalues <= 4
  lines(x,hx,col="#990000ff", lwd=2)
  return(1)
}

plotGaussianCDF <- function(maintitle) {
  plot(xseq,normcdf, 
       main=maintitle, 
       xlab="Standard Deviations", 
       ylab="Cumulative Probability",
       xlim=c(-4,4),
       ylim=c(0,1),
       pch=".",
       axes=F
  )
  polygon(c(-4,xseq,4), c(0,normcdf,0), col="gray80")
  axis(side=1,at=seq(-4,4,1), pos=0)
  axis(side=2,at=seq(0,1,0.2), pos=-4)
  return(1)  
}

####################################
# Save the plots to PNG files:
####################################

png(file="NormalCDFplain.png", 
    width=6.5, height=5, units="in", 
    res=300
    )

plotGaussianCDF(maintitle="Cumulative Distribution Function (Normal)")

abline(v=0, lty=2)
abline(h=0.5,lty=2, lwd=2, col="blue")
text (x=-1.5,y=0.88, "Total Probability", col="navy")
arrows(-1,0.85 , 
       x1=0.5, y1= 0.7, 
       code=2, length=0.11, 
       angle=15, lwd=2, col="navy"
       )

dev.off()


png(file="NormalCDF.png", 
    width=6.5, height=5, units="in", 
    res=300
    )
plotGaussianCDF(maintitle="Probability Some Event\nHappens at or Below a Given Z")

abline(v=0, lty=2)
abline(h=1, lty=2)
abline(h=0.5,lty=2, lwd=2, col="blue")

y2<-pnorm(1, mean=0, sd=1)
lines(x=c(-4,1), y=c(y2,y2), lwd=1, col=mygreen)
lines(x=c(1,1), y=c(0,y2), lwd=1, col=mygreen)

arrows(-1.8,0.7, 
       x1=-0.5, y1=y2, 
       code=2, 
       length=0.1, angle=15, lwd=2,
       col="navy"
       )
text(-2.5,0.75, "Cumulative\nprobability\nat Z=1")

dev.off()

# Now for a bit more explanation:

png(file="NormalCDF1SD.png", width=6.5, height=5, units="in", res=300)
plotGaussianCDF(maintitle="Probability Some Event\nHappens Between Two Z-scores")

y1<-pnorm(-1, mean=0, sd=1)
y2<-pnorm(1, mean=0, sd=1)

polygon(c(-1,-1, 1, 1), c(y1,y2,y2,y1), col=alphafill)

abline(h=y1, col=darkred)
abline(h=y2, col=darkred)
abline(v=-1, col=mygreen, lwd=2)
abline(v=1, col=mygreen, lwd=2)
abline(v=0,lty=2, lwd=2, col="gray40")

dev.off()

png(file="NormalCDF1SD68.png", width=6.5, height=5, units="in", res=300)
plotGaussianCDF(maintitle="Probability Some Event\nHappens Happens Between Two Z-scores")

y1<-pnorm(-1,mean=0, sd=1)
y2<-pnorm(1, mean=0, sd=1)

polygon(c(-1,-1, 1, 1), c(y1,y2,y2,y1), col=alphafill)

abline(h=y1, col=darkred)
abline(h=y2, col=darkred)
abline(v=-1, col=mygreen, lwd=2)
abline(v=1, col=mygreen, lwd=2)
abline(v=0,lty=2, lwd=2, col="gray40")

#lines(x=c(-2,-2), y=c(y1,y2), lwd=3)
arrows(x0=-2, y0=y1, 
       x1=-2, y1=y2, 
       code=3, length=0.11, angle=15, lwd=2
       )
text(-2.65,0.7, "This\ndistance\nis 0.68!")

dev.off()

# EOF