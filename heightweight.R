# avg female (30-39) height & weight, US, 1975

height<-c(1.47,1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 1.70, 1.73, 
          1.75, 1.78, 1.80, 1.83)
weight <- c(52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 63.11, 
            64.47, 66.28, 68.10, 69.92, 72.19, 74.46)

# make a graph of the x & y dots:
png(filename="heightweight.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )

plot(weight~height, 
     main="Avg Weights & Heights of US females, 30-39", 
     pch=21, col="black", bg="red"
     )

dev.off()

# make a graph of the x & y dots, with regression line:
png(filename="heightweightregress.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )

plot(weight~height, 
     main="Avg Weights & Heights of US females, 30-39",  
     pch=21, col="black", bg="red"
     )

fit <-lm(weight~height)
abline(fit, col="blue")
dev.off()


# REgress height on weight:
lm.r=lm(weight~height)
summary(lm.r)


# make a graph of the x & y dots, with regression line:
png(filename="heightweightregressextrapolate.png", 
    width=5, 
    height=4, 
    units="in", 
    pointsize=12, 
    bg="white", 
    res=300,
#    type="windows"
    type="quartz"
)

plot(weight~height, 
     main="Avg Weights & Heights of US females, 30-39",  
     pch=21, 
     col="black", 
     bg="red",
     xlim=c(0.5,2),
     ylim=c(0,80)
     )
  fit <-lm(weight~height)
  abline(fit, col="blue")

dev.off()