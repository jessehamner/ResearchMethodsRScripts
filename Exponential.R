t=seq(1,10)
subscribers=c(1.6,2.7,4.4,6.4,8.9,13.1,19.3,28.2,38.2,48.7)

margins<-c(4,4,2,1)

png(filename="cellphones.png", width=4, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
par(mar=margins)
plot(t,subscribers, xlab="Years since 1987", ylab="Cell phone subscribers", pch=21, col="black", bg="red", main="Mobile Phone Users in the US")

dev.off()


png(filename="cellphoneslinear.png", width=4, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
par(mar=margins)
plot(t,log(subscribers), xlab="Years since 1987", ylab="Log of Cell phone subscribers", pch=21, col="black", bg="red", main="Mobile Phone Users in the US")
res=lm(log(subscribers)~t)
abline(res, col="blue")

dev.off()

# Now, some independent data from the Information Please almanac online:

infoplease<-(read.delim("cellphoneuse.tab", header=TRUE, sep="\t", comment.char="#"))

# First, a map of the raw counts (divided by one million for clarity)
png(filename="cellphones_alt1.png", width=5, height=5, units="in", pointsize=12, bg="white", res=300, type="quartz")
par(mar=margins)
plot(infoplease$YEAR, infoplease$COUNT/1000000, xlab="Years since 1985", ylab="Millions of Mobile Phone Subscribers", pch=21, col="black", bg="red", main="Mobile Phone Users in the US")

dev.off()

infosubset<-(subset(infoplease, YEAR<1998 & YEAR > 1986))
time=seq(1,length(infosubset$COUNT))

# Next, a graphic of the LOG of the counts:

png(filename="cellphones_linearalt1.png", width=5, height=5, units="in", pointsize=12, bg="white", res=300, type="quartz")
par(mar=margins)
plot(time,log(infosubset$COUNT), xlab="Years since 1987", ylab="Log of Cell phone subscribers", pch=21, col="black", bg="red", main="Mobile Phone Users in the US")
res=lm(log(infosubset$COUNT)~time)
abline(res, col="blue")

dev.off()

