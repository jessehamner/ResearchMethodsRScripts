# R plots for histogram example
# Now with functions (subroutines) and a for-loop

# The data are included in R, but here's another way to read them
# in as tab-separated text:

sunspotslocal <- read.table("sunspots.dat", sep="\t", header=TRUE)
sunspotdensity <- sunspotslocal

# set up the size of the bins for each histogram:
breaks<-c(4,10,20,30)

# rendertype <- ("windows")
rendertype <- ("quartz")

# pieces of each file name:
filenamestem<-("sunspothistogram")
filenamepost<-("bins")

# A function to draw a distribution normal curve over the histogram:
addNorm <- function(data,color,linewidth) {	
	# Add a Normal Curve (Thanks to Peter Dalgaard)

	xfit<-seq(min(data),max(data),length=40)
	yfit<-dnorm(xfit,mean=mean(data),sd=sd(data))
	yfit <- yfit*diff(h$mids[1:2])*length(data)
	lines(xfit, yfit, col=color, lwd=linewidth) 
return;
}

addGamma <- function(data, color, linewidth){
  a <- 1.5
  s <- 32
  xfit<-seq(min(data),max(data),length=200)
  yfit<-dgamma(xfit,shape=a,scale=s, log=FALSE)
  yfit <- yfit*diff(h$mids[1:2])*length(data)
  lines(xfit, yfit, col=color, lwd=linewidth) 
  return;
}

# Finally, the histogram loop: draw identical histogram graphics files based on the "breaks" array:

for(i in 1:length(breaks)) {
	filenamecur<-paste(filenamestem,breaks[[i]],filenamepost,sep="_")
	png(
    filename=(paste(filenamecur,"png", sep=".")), 
    res=300, 
    bg="white", 
    type=rendertype,
    pointsize=12, 
    width=6, 
    height=6, 
    units="in")
  
	  h<-hist(sunspotdensity$Sunspot.count, 
            breaks=breaks[[i]], 
            col="gray", 
            xlab="Sunspot Count", 
            main="Sunspot count by year, 1700-2004")

    #addNorm(data=sunspotdensity$Sunspot.count,
    #        color="blue",
    #        linewidth=2)
	  addGamma(
      data=sunspotdensity$Sunspot.count, 
      color="blue", 
      linewidth=2
    )
	
  dev.off()
}


# Make a loop-friendly, but less descriptive, name for the histogram:	
# slightly alter the filename stem:
	filenamestem<-paste(filenamestem,'0',sep="")

for(i in 1:length(breaks)) {
# make a new filename prefix:
	filenamecur<-paste(filenamestem,i,sep="")

	png(filename=(paste(filenamecur,"png", sep=".")), 
	    res=300, bg="white", 
	    type=rendertype, 
	    pointsize=12, 
	    width=6, height=6, units="in"
	    )
	h<-hist(sunspotdensity$Sunspot.count, 
	        breaks=breaks[[i]], 
	        col="gray", 
	        xlab="Sunspot Count", 
	        main="Sunspot count by year, 1700-2004")
#	addNorm(data=sunspotdensity$Sunspot.count,color="blue",linewidth=2)
  addGamma(
    data=sunspotdensity$Sunspot.count, 
    color="blue", 
    linewidth=2
  )
	dev.off()

}
