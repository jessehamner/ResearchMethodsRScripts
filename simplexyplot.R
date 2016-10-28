# simple plot using ggplot
library(ggplot2)
library(extrafont)
library(fontcm)


xvector <- c(0,1,2,3,4,5,6)
yvector <- 2 * xvector
dd <- NULL
dd <- cbind(dd, xvector)
dd <- cbind(dd, yvector)
df <- as.data.frame(dd)

xlimits <- c(0,9)
ylimits <- c(0,12)
xlabel <- "X axis"
ylabel <- "Y axis"
plottitle <- "Graph of y=2x\n"

simplePlotFunction <- function(data,
                               xlimits, 
                               ylimits,
                               xlabelstring, 
                               ylabelstring, 
                               plottitle, 
                               fontfamily="sans") 
{
	returnplot <- ggplot(data=data, aes(x=xvector,y=yvector)) + 
	theme(
			text=element_text(size=20, face="bold", family=fontfamily),
			axis.title.x = element_text(vjust = -0.5 )
		 ) +
	labs(title=plottitle, x=xlabelstring, y=ylabelstring) + 
	geom_line(data=data, size=1, color="black" ) +
	geom_point(data=data, size=5, color="black" ) +
	coord_cartesian(xlim=xlimits, ylim=ylimits) +
	scale_y_continuous(breaks=seq(ylimits[1],ylimits[2],2)) +
	scale_x_continuous(breaks=seq(xlimits[1],xlimits[2],2))

return(returnplot)
}

PlotFunctionLR <- function(data,
                           xlimits, 
                           ylimits,
                           xlabelstring, 
                           ylabelstring, 
                           plottitle, 
                           fontfamily="sans", 
                           ols, 
                           scalextick=2, 
                           scaleytick=2) 
{
	predval <- as.data.frame(cbind(data$xvector,ols$fit))
	returnplot <- ggplot(data=data, aes(x=xvector,y=yvector)) + 
	theme(
			text=element_text(size=20, face="bold", family=fontfamily),
			axis.title.x = element_text(vjust = -0.5 )
		 ) +
	labs(title=plottitle, x=xlabelstring, y=ylabelstring) + 
	geom_point(data=data, size=5, color="black") +
#	geom_line(data=predval, size=1, color="blue") +
	geom_smooth(method=lm, size=1, color="blue", se=FALSE) + 
	coord_cartesian(xlim=xlimits, ylim=ylimits) +
	scale_y_continuous(breaks=seq(ylimits[1],ylimits[2],scaleytick)) +
	scale_x_continuous(breaks=seq(xlimits[1],xlimits[2],scalextick))

return(returnplot)
}

myplot <- simplePlotFunction(data=df, xlimits=xlimits, 
                             ylimits=ylimits, 
                             xlabelstring=xlabel, 
                             ylabelstring=ylabel, 
                             plottitle=plottitle )

# If you want to use a system font that isn't postscript, you'll have to 
# change the fontfamily to "CM Sans" or whatever, and output to PDF
# instead of PNG or whatever.
#
#ggsave("ggplot_cm.pdf", myplot, width=5.5, height=5)
#embed_fonts("ggplot_cm.pdf", outfile="ggplot_cm_embed.pdf")

png(filename="simplexyplot.png", 
	width=5.5, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )		
myplot
dev.off()

# NOW, for example 2:

xvector <- c(1,2,7,4,5)
yvector <- c(4,5,11,9,8)
dd <- NULL
dd <- cbind(dd, xvector)
dd <- cbind(dd, yvector)
df <- as.data.frame(dd)
plottitle <- "Graph of an unknown function\n"

myplot <- simplePlotFunction(data=df, 
                             xlimits=xlimits, 
                             ylimits=ylimits, 
                             xlabelstring=xlabel, 
                             ylabelstring=ylabel, 
                             plottitle=plottitle )

png(filename="errorxyplot.png", 
	width=5.5, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )		
myplot
dev.off()


# and example 3:

xvector <- c(1,2,7,4,5,3, 4.5, 6  )
yvector <- c(4,5,11,9,8,6,7, 11  )
dd <- NULL
dd <- cbind(dd, xvector)
dd <- cbind(dd, yvector)
df <- as.data.frame(dd)

olsresult <- lm(df, formula = yvector ~ xvector)

plottitle <- "Graph of an unknown function\n"

myplot <- PlotFunctionLR(data=df, 
                         xlimits=xlimits, 
                         ylimits=ylimits, 
                         xlabelstring=xlabel, 
                         ylabelstring=ylabel, 
                         plottitle=plottitle, 
                         ols=olsresult )

myplot <- myplot + geom_text(aes(6, 4, label="Y-intercept: 2.6"), size=10)
myplot <- myplot + geom_text(aes(5.4, 2, label="Slope: 1.23"), size=10)


png(filename="errorxyplotlr.png", 
	width=5.5, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )		
myplot
dev.off()



# and example 4:

xvector <- c(1,2,7,4,5,3, 4.5, 6  )
yvector <- xvector^2
dd <- NULL
dd <- cbind(dd, xvector)
dd <- cbind(dd, yvector)
df <- as.data.frame(dd)
xlimits <- c(0,9)
ylimits <- c(0,50)
olsresult <- lm(df, formula = yvector ~ xvector)
plottitle <- "Graph of an unknown function\n"

myplot <- PlotFunctionLR(data=df, xlimits=xlimits, ylimits=ylimits, xlabelstring=xlabel, ylabelstring=ylabel, plottitle=plottitle, ols=olsresult, scaleytick=10 )

myplot <- myplot + geom_text(aes(3, 40, label="Y-intercept: -12.2"), size=8)
myplot <- myplot + geom_text(aes(2.3, 34, label="Slope: 7.94"), size=8)


png(filename="errorxyplotlrexponential1.png", 
	width=5.5, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )		
myplot
dev.off()


# and example 5:

xvector <- c(seq(0,32,2) )
yvector <- xvector^2
dd <- NULL
dd <- cbind(dd, xvector)
dd <- cbind(dd, yvector)
df <- as.data.frame(dd)

xlimits <- c(0,33)
ylimits <- c(0,1050)

olsresult <- lm(df, formula = yvector ~ xvector)

plottitle <- "Graph of an unknown function\n"

myplot <- PlotFunctionLR(data=df, xlimits=xlimits, ylimits=ylimits, xlabelstring=xlabel, ylabelstring=ylabel, plottitle=plottitle, ols=olsresult, scaleytick=100, scalextick=5 )

#myplot <- myplot + geom_text(aes(6, 4, label="Y-intercept: 2.6"), size=10)
#myplot <- myplot + geom_text(aes(5.4, 2, label="Slope: 1.23"), size=10)



png(filename="errorxyplotlrexponential2.png", 
	width=5.75, height=5, units="in", pointsize=12, bg="white", res=300, 
	type="quartz"
   )		
myplot
dev.off()




