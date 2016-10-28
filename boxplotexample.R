# R boxplot examples

A <- read.table("externaldebt.txt", sep = "\t", header=TRUE, fill=TRUE)

# boxplot(A$PercapitaUSD)

newdata <- subset(A, Continent == "E")
# str(newdata)
# boxplot(newdata$PctGDP)

png(filename="boxplotoutliers.png", width=2.5, height=6, units="in", pointsize=10, bg="white", res=300, type="quartz")

out <- boxplot(newdata$PctGDP, border="dark red")$out
outliers <- subset(A, PctGDP>800)

outliers$abbr <- ""
outliers$abbr[outliers$Country == "Norway"] <- "NOR"
outliers$abbr[outliers$Country == "Ireland"] <- "IRE"
outliers$abbr[outliers$Country == "Luxembourg"] <- "LUX"

# names(out) <- paste(out,1:length(out))
names(out) <- paste(out,outliers$abbr)
text(rep(1.3,length(out)), out, names(out))

dev.off()

