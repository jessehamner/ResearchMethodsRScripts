# Ultra simple boxplot example, using included sample data.

png(filename="Boxplotexample.png", 
    width=4, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )
boxplot(mpg~cyl, data=mtcars, main="Car Mileage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon", pch=20)
dev.off()
