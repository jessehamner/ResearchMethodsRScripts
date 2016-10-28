# Draw the beta  distribution.


x <- seq(0, 1, 0.001)
#a <- 0.5
a <- 2
s <- 2

maxprob<- 1.5

# graphics margins:
margins<-c(4.5,4.5,1,2)

png(filename="betadistribution.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")

plot(x, dbeta(x, shape1 = a, shape2 = s, log=FALSE), type = "l", ylim = c(0,maxprob), lwd = 2, xlab="X value", ylab="Probability of event occurrence")

i<- seq(0,1,0.01)
# polygon(c(i),dbeta(i, shape = a, scale = s), density=5, angle=45)
polygon(c(i),dbeta(i, shape1 = a, shape2 = s), col="darkblue")


text(6, 1.4, expression(f(x) == frac(1, s^a * Beta(a)) * x^(a - 1) * exp(-frac(x, s))))
text(6, 1.2, expression(paste(a > 0, ": shape parameter")))
text(6, 1.1, expression(paste(s > 0, ": scale parameter")))
title("Beta distribution: parameters shape=2 and scale=2",cex = 0.6)

dev.off()



png(filename="betadistribution_simple.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")

par(mar=margins)

plot(x, dbeta(x, shape1 = a, shape2 = s, log=FALSE), type = "l", ylim = c(0,maxprob), lwd = 2, xlab="X value", ylab="Probability of event occurrence")

i<- seq(0,1,0.01)
# polygon(c(i),dbeta(i, shape1 = a, shapes2 = s), density=5, angle=45)
polygon(c(i),dbeta(i, shape1 = a, shape2 = s), col="darkblue")


#text(6, 1.4, expression(f(x) == frac(1, s^a * Beta(a)) * x^(a - 1) * exp(-frac(x, s))))
#text(6, 1.2, expression(paste(a > 0, ": shape parameter")))
#text(6, 1.1, expression(paste(s > 0, ": scale parameter")))
#title("Beta distribution: parameters shape=2 and scale=2",cex = 0.6)

dev.off()

