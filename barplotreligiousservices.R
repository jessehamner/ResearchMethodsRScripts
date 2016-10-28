library(ggplot2)

religious <- c(1322,571,502,308,380,240,1168)
cats <- c("Never / almost never", "Once a year", "Several times a year", "Once a month", "2-3 times a month", "Nearly every week", "Every week or more")
order <- c(seq(1,length(cats),1))

relservices <- as.data.frame(religious, row.names=cats)
relservices <- cbind(relservices, order)

# barplot(height=relservices[,1], names.arg=rownames(relservices), ylim=c(0,1400))



returnplot <- ggplot(data=relservices, aes(y=relservices$religious, x=reorder(rownames(relservices),order )  )  ) + 
	theme(
			text=element_text(size=20, face="bold", family="sans"),
			axis.title.x = element_text(vjust = -0.5 ),
			axis.text.x = element_text(angle=45, vjust=1, hjust=0.9)
		 ) +
	geom_bar(stat='identity')  +
	labs(title="Frequency of Attendance\nat Worship Services", 
		 y="Count of Respondents", 
		 x=""
		)
	
	
png(filename="religiousattendancebarplot.png", 
	width=5.5, height=6, units="in", pointsize=12, bg="white", res=150, 
	type="quartz"
   )		
returnplot
dev.off()