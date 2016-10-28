# R code to read in a set of means

# setwd("")

gdpmeans <- read.table("countrypercapitagdpmeans.txt", sep = "\t", header=TRUE)

barplot(gdpmeans$Mean.Score, 
        names.arg=gdpmeans$Country.Per.Capita.GDP, 
        xlab="Per Capita GDP", 
        ylab="GDP Per Capita Mean Score", 
        ylim=c(0,10), 
        main="Political Rights and Freedoms,\nby Country Per Capita GDP" 
        )

png(filename="gdppccatplot.png", 
    width=6, height=6, units="in", 
    pointsize=12, 
    bg="white", 
    res=300, 
    type="quartz"
    )

plot(gdpmeans$Mean.Score, 
     axes=FALSE, 
     ylim=c(0,10), 
     xlab="GDP Per Capita Categories", 
     ylab="GDP Per Capita Mean Score", 
     pch=20, 
     main="Political Rights and Freedoms,\nby Country Per Capita GDP"
     )
axis(1, at=1:4, lab=c("Low","Medium-Low","Medium-High", "High"))
axis(2, las=1, at=0:10, labels=TRUE)
lines(gdpmeans$Mean.Score)

dev.off()

smokers <- read.table("gallupsmoking.txt", sep="\t", header=TRUE)
# March 21, 2008 poll of 75,073 surveys conducted between Jan 2, 2008 to March 17, 2008
# http://www.gallup.com/poll/105550/Among-Americans-Smoking-Decreases-Income-Increases.aspx
# of note, "Gallup researchers previously found no consistent relationship between the prevalence of smoking in a country and its location or its residents' relative wealth."
# http://www.gallup.com/poll/28432/Smoking-Rates-Around-World-How-Americans-Compare.aspx


# This doesn't work well, because of funky labeling:
# barplot(smokers$Percent.who.smoke, names.arg=smokers$Income.category, xlab="Income Category", ylab="Percent who smoke", ylim=c(0,40), main="Gallup poll of Americans" )

# So let's angle the categories:
mp <- barplot(smokers$Percent.who.smoke, 
              ylim=c(0,40), 
              main="Gallup poll of Americans", 
              axes=FALSE, 
              width=c(0.88), 
              xlim=c(1,9) , 
              ylab="Percentage who smoke"
              )

labels <- paste (smokers$Income.category, sep = " ")
text(1:9, par("usr")[3] - 0.5, srt=45, adj=1.0, labels=labels, xpd=TRUE)
axis(2, las=1, at=c(0,10,20,30,40), labels=TRUE, pos=c(0.1))



png(filename="gallupsmokersbyincome.png", 
    width=7, height=5, units="in", 
    pointsize=12, 
    bg="white", 
    res=300, 
    type="quartz"
)

# default margins (bottom, left, top, right): c(5, 4, 4, 2) + 0.1 lines
margins<-c(6,2,4,2)
omargins <- c(0,0,0,0)
par(mar=margins, oma=omargins)

barplot(height=smokers$Percent.who.smoke, 
        ylim=c(0,40), 
        main="Gallup poll of Americans", 
        axes=FALSE, 
        width=c(0.88), 
        xlim=c(-1,9)
        )

labels <- paste (smokers$Income.category, sep = " ")
text(x=seq(0.7,9.2,1.05), 
     par("usr")[3] - 0.5, 
     srt=45, 
     adj=1.0, 
     labels=labels, 
     xpd=TRUE
     )
axis(2, las=1, at=c(0,10,20,30,40), labels=TRUE, pos=c(0.1))
text(x=-1.0,y=31, labels="Percentage who smoke", pos=2, cex=1.2, srt=90)

dev.off()