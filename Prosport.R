require(foreign)

# setwd("")

# Import the SPSS data using read.spss:
prosport<-data.frame(read.spss("prosport.sav", use.value.labels=TRUE))

# XY plot of Pro athletes versus house districts:

png(filename="prosportscatterplot.png", 
    width=6, height=5, units="in", 
    pointsize=10, 
    bg="white", 
    res=300, 
    type="quartz"
    )

plot(prosport$HOUSE,prosport$PROS, 
     main="Pro Athletes and State House District Counts", 
     xlab="US House Districts in State", 
     ylab="Pro Athletes from State",
     pch=21,
     col="black",
     bg="red"
     )

dev.off()

# Pro Sports scatterplot with regression line:

png(filename="prosportscatterplotregress.png", 
    width=6, height=5, units="in", 
    pointsize=10, 
    bg="white", 
    res=300, 
    type="quartz"
    )

plot(prosport$HOUSE,prosport$PROS, 
     main="Pro Athletes and State House District Counts", 
     xlab="US House Districts in State", 
     ylab="Pro Athletes from State",
     pch=21,
     col="black",
     bg="red"
     )

# Fitted regression line:
regline<-lm(prosport$PROS~prosport$HOUSE)
abline(regline, col="blue")

# All labels:
# text(prosport$HOUSE,prosport$PRO, prosport$ABBREV, cex=1, pos=4, col="black")

# Subset of labels:
proframe<-data.frame(prosport)
labelstates<-subset(proframe, 
                    PROS==310 | PROS==318 | PROS == 164 | PROS == 269 )


# Four points (excluding California, which *should be* obvious)
text(labelstates$HOUSE,
     labelstates$PROS, 
     labelstates$ABBREV, 
     cex=1, 
     pos=4, 
     col="black"
     )

dev.off()

# Just the Southern states:
southern<-subset(proframe, SOUTH==1)
# text(southern$HOUSE,southern$PROS, southern$ABBREV, cex=1, pos=4, col="brown")

# Histogram of state US House districts count:

png(filename="HouseDistrictsHistogram.png", 
    width=5, height=5, units="in", 
    pointsize=12, 
    bg="white", 
    res=300, 
    type="quartz"
    )

h<-hist(prosport$HOUSE, 
        breaks=6, 
        col="red", 
        xlab="Districts Count per State", 
        ylab="Frequency", 
        main="Histogram of US House Districts Count", 
        ylim=c(0,40) 
        )

dev.off()
