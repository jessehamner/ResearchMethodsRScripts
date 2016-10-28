# R plots for histogram example

# The data are included in R, but here's another way to read them in as tab-separated text:
sunspotslocal <- read.table("sunspots.dat", sep="\t", header=TRUE)
sunspotdensity <- sunspotslocal

# Now a scatterplot:
png(filename="sunspotscatterplot.png", 
    res=300, bg="white", 
    type="quartz", 
    pointsize=12, 
    width=6, height=6, units="in"
    )

plot(sunspotslocal$Year,
     sunspotslocal$Sunspot.count, 
     main="Sunspots per year, 1700-2004", 
     xlab="Year", 
     ylab="Count of Sunspots", 
     type="p", 
     col="dark red", 
     pch=20
     )
#abline(lm(sunspots$Sunspot.count~sunspots$Year), col="red") # regression line (y~x)
#lines(lowess(sunspots$Year,sunspots$Sunspot.count), col="blue") # lowess line (x,y) 

dev.off()

# This is a nice line graph of the sunspots:

png(filename="sunspotlineplot.png", 
    res=300, bg="white", 
    type="quartz", 
    pointsize=12, 
    width=6, height=6, units="in"
    )
plot(sunspots, 
     main="Sunspots per year, 1700-2004", 
     xlab="Year", 
     ylab="Count of Sunspots"
     )

dev.off()

# now, a kernel density plot (it's ugly, but makes a point)
png(filename="sunspotkerneldensityplot.png", 
    res=300, 
    bg="white", 
    type="quartz", 
    pointsize=12, 
    width=6, height=6, units="in"
    )
d <- density(sunspotslocal$Sunspot.count) # returns the density data
plot(d, main="Kernel density plot of sunspots") # plots the results

dev.off()
