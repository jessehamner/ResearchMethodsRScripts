DF <- as.data.frame(UCBAdmissions)
xtabs(Freq ~ Gender + Admit, DF)

#        Admit
#Gender   Admitted Rejected
#  Male       1198     1493
#  Female      557     1278

summary(xtabs(Freq ~ ., DF))

# Number of cases in table: 4526 
#Number of factors: 3 
#Test for independence of all factors:
#	Chisq = 2000.3, df = 16, p-value = 0
 
