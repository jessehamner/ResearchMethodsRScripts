# Draw the binomial probability mass function.
library(ggplot2)


#a <- 0.5
p <- 0.5
n <- 40
x <- seq(0,n,1)
i <- seq(0,n,1)
maxprob<- 0.2
y <- dbinom(x,size=n, prob=p, log=FALSE)

B <- matrix(c(x,y), nrow=length(x),ncol=2)
C <-data.frame(B)


# graphics margins:
margins<-c(4.5,4.5,1,2)


#png(filename="binomialdistribution.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")
#plot(x, y, type = "l", ylim = c(0,maxprob), lwd = 2, xlab="X value", ylab="Probability of event occurrence")
# polygon(c(i),y, density=5, angle=45)
#polygon(c(i),y, col="darkblue")
#dev.off()


#png(filename="binomialdistribution_simple.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")
#par(mar=margins)
#plot(x, y, type = "l", ylim = c(0,maxprob), lwd = 2, xlab="X value", ylab="Probability of event occurrence")
# polygon(c(i),dbinom(x,size=n,prob=p, log=FALSE), density=5, angle=45)
#polygon(c(i),dbinom(x,size=n,prob=p, log=FALSE), col="darkblue")

#dev.off()

png(filename="binomialdistribution.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")

ggplot(data=C, aes(x=x,y=y)) + 
  geom_point(color="red", size=3) + 
  labs(title="Binomial Probability Distribution", x="Count", y="Probability") 

dev.off()

png(filename="binomialdistributionnolabels.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")

bb<- ggplot(data=C, aes(x=x,y=y)) + 
  geom_point(color="red", size=3) + 
  labs(x="Count", y="Probability")
bb

dev.off()