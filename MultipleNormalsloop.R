# Plots several normal curves:
x <- seq(-4,4,by=.005)
y <- dnorm(x) 

sdwidth<-c(0.45,0.5,0.75,1,2)
filename<-("normal")

#plot(x,y, main="Normal Curve Plot", pch=20, ylab="Probability", xlab="Standard Deviations", ylim=c(0,1))

for(i in 1:length(sdwidth)) {
	filenamecur<-paste(paste(filename,i,sep=""),"png",sep=".")

	png(filename=filenamecur, width=4, height=4, units="in", pointsize=12, bg="white", res=150, type="quartz")
	y <- dnorm(x,mean=0,sd=sdwidth[[i]]) 
	plot(x,y, main="Normal Curve Plot", pch=20, ylab="Probability", xlab="Standard Deviations", ylim=c(0,1))
	dev.off()
}
