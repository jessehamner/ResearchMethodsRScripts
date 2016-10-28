library(ggplot2)


storks <- read.table("storkdata.txt", header=T)

a <- lm(populationThousands ~ storkCount, data=storks)

# summary(a)
# Call:
# lm(formula = populationThousands ~ storkCount, data = storks)
# 
# Residuals:
#       1       2       3       4       5       6       7 
# -1.3548 -3.2491  3.8830  3.9623 -3.7735 -0.7471  1.2793 
# 
# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 26.04177    5.86058   4.444  0.00674 **
# storkCount   0.19472    0.02898   6.719  0.00111 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.453 on 5 degrees of freedom
# Multiple R-squared:  0.9003,	Adjusted R-squared:  0.8803 
# F-statistic: 45.14 on 1 and 5 DF,  p-value: 0.001107

attach(a)

plot(storks$storkCount, storks$populationThousands)
abline(a, col="red")

setwd("../images")


png(filename="storks.png", res=300, bg="white", type="quartz", pointsize=14, width=6, height=5, units="in")

ggplot(data=storks, aes(x=storkCount,y=populationThousands)) + geom_point() + labs(title="Stork and Human Populations,\nOldenburg Germany, 1930-1936", x="Stork Count", y="Human Population, in Thousands") + geom_abline(data=storks, intercept=a$coefficients[1], slope=a$coefficients[2], color="brown", size=2) +
annotate("text", label="r=0.94\np<0.001", x=145, y=70, size=8, color="black" ) + 			theme_classic(base_size=16, base_family="sans")


dev.off()
