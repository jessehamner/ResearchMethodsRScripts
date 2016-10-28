
# Count of congressional districts per state

cong <- c(7, 1, 6, 4, 52, 6, 6, 1, 23, 11, 2, 2, 20, 10, 5, 4, 6, 7, 2, 
          8, 10, 16, 8, 5, 9, 1, 3, 2, 2, 13, 3, 31, 12, 1, 19, 6, 5, 
          21, 2, 6, 1, 9, 30, 3, 1, 11, 9, 3, 9, 1
          )

mean(cong)
median(cong)
mode(cong)
sort(cong)

length(cong)

d <- density(cong)

stem(cong)   

stem(cong, scale=10)

boxplot(cong)

png(filename="congressdistrictsfreqdist.png", 
	width=5.5, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )		
plot(d, 
     xlab="Count of Congressional Districts", 
     ylab="Frequency Density", 
     main="Frequency Distribution of\nCongressional Districts, by State", 
     xlim=c(0,60)
     )
polygon(d, 
        col="red", 
        border="black", 
        xlab="Count of Congressional Districts", 
        ylab="Relative Frequency"
        )

dev.off()




h <- hist(cong, 
          breaks=6, 
          col="red", 
          xlab="Congressional Districts Per State", 
          ylab="Count of States", 
          main="", 
          ylim=c(0,40)
          )
multiplier <- diff(h$mids[1:2]) * length(cong)


png(filename="congressdistrictshistplusdensity.png", 
	width=5.5, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )

hist(cong, breaks=6, 
     col="red", 
     xlab="Congressional Districts Per State", 
     ylab="Count of States", 
     main="Histogram", 
     ylim=c(0,40), 
     cex.lab=1.5, 
     cex.axis=1.5, 
     cex.main=2.5, 
     cex.sub=0.5
    )

# optional: plot the kernel density on top of it:
#lines (d$x, (d$y*multiplier), lw=3, col="blue")

dev.off()