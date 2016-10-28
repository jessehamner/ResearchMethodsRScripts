# Mosaic plots example, using the UC-Berkeley admissions data
# and the Titanic victim data, both included in R.

require(stats)

# A series of three mosaic plots. 

# First, a simple plot of student admissions at UC-Berkeley 
# to the largest 6 departments in the early 1970s:

# Uncomment this line and the dev.off lines to make a PNG output:
#png(filename="UCBoverall.png", width=5, height=5, units="in", pointsize=9, bg="white", res=300, type="quartz")

# Set up graphic display parameters:
opar <- par(mfrow = c(1, 1), oma = c(2, 0, 2, 0), fg="black", bg="whitesmoke" )

# do the mosaic plot:
mosaicplot(
	apply(UCBAdmissions, c(1, 2), sum),
	xlab="", ylab="", 
	main = mtext(
		expression(bold("Student admissions at UC Berkeley")),
		side=1,		# main title goes at bottom 
		padj=1.5, 	# add buffer space above main title, since it's at the bottom
		cex=2.0		# make the main title twice as large as baseline font
	),
	sub= mtext (
		expression(bold("Size of blocks is proportional to data percentages")),
		side=1, 	# subtitle also goes at bottom
		padj=4,		# add *even more* buffer space for the subtitle
		cex=1.5),	# make the subtitle's font size larger
	color=c("royalblue","pink3"),	# stereotypical, yes, but better contrast
	cex.axis=1.5,					# make the label fonts larger too
	
)

#dev.off()

#png(filename="UCBbydept.png", width=5, height=4, units="in", pointsize=9, bg="white", res=300, type="quartz")

# Second, a more advanced series of 6 mosaic plots (each department):

opar <- par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
for(i in 1:6)
	mosaicplot(UCBAdmissions[,,i],
	xlab = "", ylab = "",
	main = paste("Department", LETTERS[i]), color=c("royalblue","pink3"))
	mtext(expression(bold("Student admissions at UC Berkeley")),
 	outer = TRUE, cex = 1.5)
par(opar)

#dev.off()

# Third, an example of the Titanic victims:

# First, set up the labels of the categories:
survived<-c("Died","Survived")
classes<-c("Crew","3rd Class", "2nd Class", "1st Class" )
ages<-c("Child","Adult")


#png(filename="TitanicVictims.png", width=5, height=4, units="in", pointsize=9, bg="white", res=300, type="quartz")

# Next, set up the layout of the page, here, a 2x2 matrix with a slight offset to the right:
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

# Loop through our two variables of plot division, 
# lived/died and adult/child:
for(i in 1:2)
	for (j in 1:2) 
		mosaicplot(
			Titanic[,,j,i],
	 		xlab = "Age Category", ylab = "Gender",
	 		main = paste(survived[i]), paste(ages[j]), color=c("royalblue","pink3") 
	 	)
 		mtext(
 			expression(bold("Titanic Victims")),
  			outer = TRUE, cex = 1.5
  		)
par(opar)

# dev.off()