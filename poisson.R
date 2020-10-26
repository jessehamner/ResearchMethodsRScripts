# Make a sequence of Poisson distributions.

x <- seq(0,25,1)

xxx <- 0

for (xxx in seq(1,15,1)) {

png(filename=sprintf('Poisson%s.png', xxx), width=6, height=5, units="in",
    bg = "white", res = 300)
plot(x, dpois(x,xxx), xlim=c(0,25), ylim=c(0,0.40), pch=21, 
     ylab="P(X=x|Lambda)", xlab="Observed Count", type="b", bg="yellow",
     main=sprintf("Poisson Probability Mass Function for Lambda = %s", xxx)
)

abline(v=xxx, lty=2)
dev.off()

}