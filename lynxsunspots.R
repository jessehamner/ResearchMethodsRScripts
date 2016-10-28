# totally stolen from 
# http://pbil.univ-lyon1.fr/members/lobry/corcau/
# -- Jesse, 2013

data(sunspots)
#library(ts)
data(lynx)

# setwd("")


spots <- window(sunspots, freq = 1, start = 1880, end = 1900)
lnx <- window(lynx, start = 1880, end = 1900)
ratio <- max(lnx)/max(spots)
par(mai = rep(1, 4))

png(filename="lynxsunspots.png", 
    res=300, 
    bg="white", 
    type="quartz", 
    pointsize=12, 
    width=6, height=5, units="in"
    )

plot(lnx, 
     main = "Sunspot Count\nAnd Lynx Population Density",
     t = "b", 
     ylab = ""
     )
lines(ratio*spots, col = "red", t = "b" )

axis(side = 4, col = "red", col.axis = "red", at = ratio*pretty(spots),
     lab = pretty(spots))

legend(1887, 4500, col = c("red", "black"), c("spots", "lynx"), pch = 21)

dev.off()


png(filename="lynxsunspotstotal.png", 
    res=300, bg="white", 
    type="quartz", 
    pointsize=12, 
    width=6, height=5, units="in"
    )


spots <- window(sunspots, freq = 1, start = 1821, end = 1934)
ratio <- max(lynx)/max(spots)
plot(lynx, main = "Sunspot Count\nAnd Lynx Population Density",
     ylab = "")
lines(ratio*spots, col = "red")
axis(side = 4, col = "red", col.axis = "red", at = ratio*pretty(spots),
    lab = pretty(spots))
mtext("Sun spots", adj = 1, col = "red")
mtext("Lynx", adj = 0)

dev.off()