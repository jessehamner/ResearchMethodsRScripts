library(ggplot2)


m1<-read.table("mustangOLSdata.txt", header=TRUE, sep='\t')

lmfit_univar <- lm(price~years,m1)

# summary(lmfit_univar)
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  34129.6     1517.1  22.497 1.55e-13 ***
#years        -2832.8      366.7  -7.724 8.72e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Residual standard error: 2622 on 16 degrees of freedom
#Multiple R-squared: 0.7885,	Adjusted R-squared: 0.7753 

lmfit <- lm(price~years + convertible,m1)

# summary(lmfit)
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  32253.8     1058.8  30.463 6.63e-15 ***
#years        -2698.8      239.5 -11.268 1.02e-08 ***
#convertible   4108.8      856.2   4.799 0.000234 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Residual standard error: 1701 on 15 degrees of freedom
#Multiple R-squared: 0.9166,	Adjusted R-squared: 0.9055 
#F-statistic: 82.43 on 2 and 15 DF,  p-value: 8.107e-09 

convertibles <-subset(m1[ which(m1$convertible==1),])

attach(m1)

png(filename="mustang1.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )
ggplot(data=m1, aes(x=years,y=price)) + 
  geom_point() + 
  labs(title="Price of Mustangs Versus Their Age", 
      x="Age, in Years", 
      y="Sale Price"
    ) 
dev.off()


png(filename="mustang2.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )
ggplot(data=m1, aes(x=years,y=price)) + 
  geom_point() + 
  labs(title="Price of Mustangs Versus Their Age", 
       x="Age, in Years", 
       y="Sale Price"
       ) + 
  geom_abline(data=m1, 
              intercept=lmfit_univar$coefficients[1], 
              slope=lmfit_univar$coefficients[2], 
              color="brown"
              )
dev.off()


png(filename="mustang3.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )
ggplot(data=m1, aes(x=years,y=price)) + 
  geom_point() + 
  labs(title="Price of Mustangs Versus Their Age", 
       x="Age, in Years", 
       y="Sale Price") + 
  geom_point(color="red", 
             size=3, 
             data=convertibles
             )
dev.off()

png(filename="mustang4.png", 
    width=5, height=4, units="in", 
    pointsize=12, bg="white", res=300, 
    type="quartz"
    )
ggplot(data=m1, aes(x=years,y=price)) + 
  geom_point() + 
  labs(title="Price of Mustangs Versus Their Age", 
       x="Age, in Years", 
       y="Sale Price"
       ) + 
  geom_point(color="red", 
             size=3, 
             data=convertibles
             ) + 
  geom_abline(data=m1, 
              intercept=lmfit$coefficients[1], 
              slope=lmfit$coefficients[2],
              color="blue", 
              size=1
              ) + 
  geom_abline(data=m1, 
              intercept=lmfit_univar$coefficients[1], 
              slope=lmfit_univar$coefficients[2], 
              color="gray"
              )
dev.off()


# How to extend the x & y axes ranges here?
#png(filename="mustang5.png", width=5, height=4, units="in", pointsize=12, bg="white", res=300, type="quartz")
ggplot(data=m1, aes(x=years,y=price )) +
  coord_cartesian(xlim = c(0, 10)) +
  coord_cartesian(ylim = c(0, 40000)) +
  geom_point() + 
  labs(title="Price of Mustangs Versus Their Age", 
       x="Age, in Years", 
       y="Sale Price") + 
  geom_point(color="red", size=3, data=convertibles) + 
  geom_abline(data=m1, 
              intercept=lmfit$coefficients[1], 
              slope=lmfit$coefficients[2],
              color="blue", 
              size=1
              ) + 
  geom_abline(data=m1, 
              intercept=lmfit_univar$coefficients[1], 
              slope=lmfit_univar$coefficients[2], 
              color="gray"
              )
#dev.off()


detach(m1)