# Draw the logistic distribution.

x <- seq(-1,1,0.01)
s <- 0.2

# graphics margins:
margins<-c(4.5,4.5,1,2)

png(filename="logitdistribution.png", 
    width=5, height=5, units="in", 
    pointsize=10, bg="white", res=300, 
    type="quartz"
    )

plot(x, 
     plogis(x, location=0, scale=s, lower.tail=TRUE, log.p=FALSE), 
     type = "l", 
     ylim = c(0,1), 
     xlim=c(-1,1), 
     lwd = 2, 
     xlab="X value", 
     ylab="Probability of event occurrence",
     main="Logistic Distribution Cumulative Distribution Function"
    )

polygon(c(x,1),
        c(plogis(x, location=0, scale=s, lower.tail=TRUE, log.p=FALSE),0), 
        col="darkblue")


dev.off()

