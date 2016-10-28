mean=72
sd=4

lb=mean - 15
ub=mean + 30

x1 <- seq(lb,ub, length=300)
hx <- dnorm(x1, mean, sd) *0.8

i <- x1 >=lb & x1 <= ub

mean <- mean + 16
# x2 <- seq(lb,ub-mean, length=300) + mean
hx2 <- dnorm(x1, mean , sd) 

#plot(x1, hx, type="n", axes=TRUE, xlim=c(lb,ub))
#polygon(c(lb,x1[i], ub), c(0,hx[i],0), col="gray")
#polygon(c(lb,x1[i], ub), c(0,hx2[i],0), col="gray")

i <- x1 >=lb & x1 <= ub

f <- as.data.frame(x1)
f <- cbind(f, hx,hx2)

g<-c(f$hx)
g<-cbind(g,f$hx2)
h<- apply(g,1, max)


png(filename="bimodalnormalplot.png", res=150, bg="white", type="quartz", pointsize=14, width=6, height=6, units="in")

plot(x1, hx, type="n", axes=TRUE, xlim=c(lb,ub), ylim=c(0,0.1), xlab="Grade", ylab="Probability", main="Example Bimodal Plot")
polygon(c(lb,x1[i], ub), c(0,h[i],0), col="gray")

dev.off()