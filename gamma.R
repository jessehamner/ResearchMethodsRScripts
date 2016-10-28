# Draw the gamma distribution.
library(ggplot2)

x <- seq(0, 100, 0.001)
#a <- 0.5
a <- 1.5
s <- 2
i<- seq(0,100,0.001)

y=dgamma(i, shape = a, scale = s)

y[length(y)] <-0

# graphics margins:
margins<-c(4.5,4.5,1,2)

# C<- data.frame(x,y)
B <- matrix(c(i,y), nrow=length(i),ncol=2)
C <-data.frame(B)

png(filename="gammadistribution.png", 
    width=5, 
    height=5, 
    units="in", 
    pointsize=10, 
    bg="white", 
    res=300, 
    type="quartz")

plot(x, 
     dgamma(x, shape = a, scale = s), 
     type = "l", 
     ylim = c(0,0.3), 
     lwd = 2, 
     xlab="X value", 
     ylab="Probability of event occurrence",
     xlim=c(0,10)
     
     )

# polygon(c(i),dgamma(i, shape = a, scale = s), density=5, angle=45)
polygon(c(i),dgamma(i, shape = a, scale = s), col="darkgreen")


text(6, 1.4, expression(f(x) == frac(1, s^a * Gamma(a)) * x^(a - 1) * exp(-frac(x, s))))
text(6, 1.2, expression(paste(a > 0, ": shape parameter")))
text(6, 1.1, expression(paste(s > 0, ": scale parameter")))
title("Gamma distribution: parameters shape=1.5 and scale=2",cex = 0.6)

dev.off()

png(filename="gammadistribution_simple.png", 
    width=5, 
    height=5, 
    units="in", 
    pointsize=10, 
    bg="white", 
    res=300, 
    type="quartz")

par(mar=margins)

plot(x, dgamma(x, shape = a, scale = s), type = "l", 
     ylim = c(0,0.3), 
     xlim=c(0,10),
     lwd = 2, 
     xlab="X value", 
     ylab="Probability of event occurrence")


# polygon(c(i),dgamma(i, shape = a, scale = s), density=5, angle=45)
polygon(c(i),dgamma(i, shape = a, scale = s), col="darkgreen")


#text(6, 1.4, expression(f(x) == frac(1, s^a * Gamma(a)) * x^(a - 1) * exp(-frac(x, s))))
#text(6, 1.2, expression(paste(a > 0, ": shape parameter")))
#text(6, 1.1, expression(paste(s > 0, ": scale parameter")))
#title("Gamma distribution: parameters shape=1.5 and scale=2",cex = 0.6)
dev.off()


png(filename="gammadistributionnolabels.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")

bb <- ggplot(data=C, aes(x=i,y=y)) + 
  geom_point(size=0.5, color="darkgreen") + 
  labs( x="X value", y="Probability") + 
  geom_polygon(x=i,y=y, fill="darkgreen", size=0.5, alpha=1) +
  xlim(0,10)

bb

dev.off()