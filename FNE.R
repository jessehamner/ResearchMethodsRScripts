# R code to import Sian's
# "Fear of Negative Evaluation" data from Excel tab export

library(foreign)

# setwd("K:/imposter/imposter documents/data")
# setwd("")

# ResponseID (V1)
# ResponseSet (V2)
# Name (V3)
# External Data Reference (V4)
# Email Address (V5)
# IP Address (V6)
# Status (V7)
# StartDate (V8)
# EndDate (V9)
# Finished (V10)

# Purpose (Q1)

# Gender: (Q2)
# 1	Female
# 2	Male
# 3	Other
# 4	Prefer not to disclose

# Age:
# 1	20-29
# 2	30-39
# 3	40-49
# 4	50-59
# 5	60-69
# 6	70+
# 7	Prefer not to disclose

# Ethnicity/Race
# 1	White
# 2	Hispanic or Latino
# 3	Black or African American
# 4	Native American or American Indian
# 5	Asian/Pacific Islander
# 6	Other
# 7	Prefer not to disclose

# In what type of library do you work?
# 1	Public
# 2	Academic
# 3	School
# 4	Archives
# 5	Corporate
# 6	Other

# Job Description:
# 1	Technical Services
# 2	Administration
# 3	Public Services
# 4	Technology/Systems/Facilities
# 5	Other

# Are you a supervisor?
# 1	Yes
# 2	No

# Years worked in libraries:
# 1	0-5		
# 2	6-10		
# 3	11-15		
# 4	16-20		
# 5	21-25		
# 6	26-30		
# 7	Over 30		
# 8	Prefer not to disclose		

# Rank/position:
# 1	Dean
# 2	Associate Dean
# 3	Assistant Dean
# 4	Department Manager
# 5	Unit Head
# 6	Coordinator
# 7	Team Leader
# 8	Librarian
# 9	Professor
# 10	Associate Professor
# 11	Assistant Professor


#origvars <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
#	"Q1","Q2","Q3","Q4","Q35","Q35_TEXT","Q5","Q5_TEXT","Q6",
#	"Q8","Q10","Q36","Q33_1","Q33_2","Q33_3","Q33_4","Q33_5",
#	"Q33_6","Q33_7","Q33_8","Q33_9","Q33_10","Q33_11","Q33_12",
#	"Q33_13","Q33_14","Q33_15","Q33_16","Q33_17","Q33_18",
#	"Q33_19","Q33_20","Q33_21","Q33_22","Q33_23","Q33_24",
#	"Q33_25","Q33_26","Q33_27","Q33_28","Q33_29","Q33_30",
#	"Q37","Q38","Q39","LocationLatitude","LocationLongitude",
#	"LocationAccuracy")

# import the tab-separated Excel export data:
rawdata <-  read.delim("FNERawData06122014.txt", header=T, sep="\t", skip=0)
origvars <- names(rawdata)

fneindex <- read.delim("FNEINDEXscores06122014.txt", header=T, sep="\t", skip=0)

# Now replace the less-useful varnames with more-useful ones:
newnames <- c("ResponseID", "ResponseSet", "Name", 
 "ExternalDataReference", "EmailAddress", "IPAddress", "Status", 
 "StartDate", "EndDate", "Finished", "Purpose", "Gender", "Age", 
 "Ethnicity_origin", "WhatTypeOfLibrary", "TEXTWhatTypeOfLibrary", 
 "JobDescription", "TEXTJobDescription", "Supervisor", 
 "YrsWorkedInLibraries", "Rank_position", "AnnualFormalEvalCount" )

for(i in seq(along=newnames)) { 
	string=paste("^",origvars[i],"$",sep="")
	names(rawdata)[i] <- sub(string,names(rawdata)[i],newnames[i])
}

# OCD cleanup:
i <- NA
string <- NA

# Merge data:

mrd <- merge(rawdata,fneindex,by="ResponseID")

# Note: this cuts the observations down to 403 from 527. But if they
# don't have an FNE score, the data are invalid anyway.


# Creating the factor variable for race from the numeric variable:
racelabels <-c("White", "Hispanic or Latino", 
  "Black or African-American", "Native American or American Indian",
  "Asian or Pacific Islander", "Other", "Prefer not to disclose" )

mrd$Ethnicity_origin <- gsub("^$",NA,mrd$Ethnicity_origin)
mrd$Ethnicity_origin[is.na(mrd$Ethnicity_origin)] <- 7
mrd$Ethnicity_origin.f = factor(mrd$Ethnicity_origin, 
 labels=racelabels )

# Gender labels:
gender <- c("Female", "Male", "Other", "Prefer not to disclose")
mrd$Gender[is.na(mrd$Gender)] <- 4
mrd$Gender.f = factor(mrd$Gender, 
 labels=gender[unique(mrd$Gender)]  )

# But "No answer" doesn't help us and does screw up the statistics, so:
mrd$Gender[mrd$Gender==4] <- NA

# Creating the factor variables for age:
agelabels <- c("20-29","30-39", "40-49", "50-59", 
              "60-69","70+","Prefer not to disclose")
mrd$Age <- gsub("^$",NA,mrd$Age)
mrd$Age[is.na(mrd$Age)] <- 7
mrd$Age.f = factor(mrd$Age, 
  labels=agelabels)

# Factor labels for "WhatTypeOfLibrary"
librarytypes <- c("Public", "Academic",
                   "School", "Archives",
                   "Corporate","Other"
)

mrd$WhatTypeOfLibrary <- gsub("^$",NA,mrd$WhatTypeOfLibrary)
mrd$WhatTypeOfLibrary.f = factor(mrd$WhatTypeOfLibrary, 
                       labels=librarytypes )

glibtypes <- c("Public","Academic", "School", "Other")
mrd$LibType<-mrd$WhatTypeOfLibrary
mrd$LibType[mrd$WhatTypeOfLibrary=="4"] <- "6"
mrd$LibType[mrd$WhatTypeOfLibrary=="5"] <- "6"


# Are you a supervisor?
supervisorlabels <- c("yes", "no")
mrd$Supervisor <- gsub("^$",NA,mrd$Supervisor)

mrd$Supervisor.f <- factor(mrd$Supervisor, labels=supervisorlabels)

# Years worked in libraries:
yearsinlibrarieslabels <- c("0-5","6-10", "11-15", "16-20",
 "21-25", "26-30","Over 30", "Prefer not to disclose")

mrd$YrsWorkedInLibraries <- gsub("^$",NA,mrd$YrsWorkedInLibraries)

mrd$YrsWorkedInLibraries[is.na(mrd$YrsWorkedInLibraries)] <- 8
mrd$YrsWorkedInLibraries.f <- factor(mrd$YrsWorkedInLibraries, 
    labels=yearsinlibrarieslabels)

# Rank:
ranklabels <- c("Dean", "Associate Dean", "Assistant Dean", 
                "Department Manager", "Unit Head", "Coordinator", 
                "Team Leader", "Librarian", "Professor", 
                "Associate Professor", "Assistant Professor",
                "Prefer Not to Answer")

mrd$Rank_position <- gsub("^$",NA,mrd$Rank_position)
mrd$Rank_position[is.na(mrd$Rank_position)] <- 12
mrd$Rank_position.f <- factor(mrd$Rank_position, 
     labels=ranklabels)

# Evaluations per year:
evalcountlabels <- c("None", "1","2","3","4+", "Prefer Not to Say")
mrd$AnnualFormalEvalCount <- gsub("^$",NA,mrd$AnnualFormalEvalCount)
mrd$AnnualFormalEvalCount[is.na(mrd$AnnualFormalEvalCount)] <- 6
mrd$AnnualFormalEvalCount.f <- factor(mrd$AnnualFormalEvalCount,
      labels=evalcountlabels)


# We want to evaluate the following variables w/r/t the FNE index:
#
# 1. Gender
# 2. Age
# 4. Library Type
# 6. Supervisor Y/N
# 7. Length of Service
# 9. Count of Annual Evaluations

########################################################
## Now let's analyze some stuff:
########################################################

# Gender:

lmgender <- lm(mrd$FNEINDEX~mrd$Gender.f)
anova(lmgender)
malesonly<-subset(x=mrd, subset=(Gender==2))
hist(malesonly$FNEINDEX)
femalesonly<-subset(x=mrd,subset=(Gender==1))
hist(femalesonly$FNEINDEX)


pdf(file="FNEgenderhistogram.pdf",width=6,height=5)
hist(femalesonly$FNEINDEX, col=rgb(0.8,0,0,0.8),xlim=c(0,30), ylim=c(0,80), 
     main="Histograms of Male and Female\nFNE Index Distributions", ylab="Frequency", 
     xlab="Fear of Negative Evaluation Index Score", font=2, cex=1.25)
hist(malesonly$FNEINDEX, add=T, col=rgb(0, 0.6, 0, 1))
dev.off()


#Age:

lmage <- lm(mrd$FNEINDEX~mrd$Age.f)
anova(lmage)

d1 <- density(subset(x=mrd,subset=(Age=="1"))$FNEINDEX)
d2 <- density(subset(x=mrd,subset=(Age=="2"))$FNEINDEX)
d3 <- density(subset(x=mrd,subset=(Age=="3"))$FNEINDEX)
d4 <- density(subset(x=mrd,subset=(Age=="4"))$FNEINDEX)
d5 <- density(subset(x=mrd,subset=(Age=="5"))$FNEINDEX)
#d6 <- density(subset(x=mrd,subset=(Age=="6"))$FNEINDEX)
#d7 <- density(subset(x=mrd,subset=(Age=="7"))$FNEINDEX)

linewidth=4

pdf(file="FNEagekernelplot.pdf",width=6,height=5)

plot(d1, 
     col=rgb(0.8,0.9,1,1), 
     xlim=c(0,40), ylim=c(0,0.06),
     main="Kernel Density Plots of Age Categories and\nFNE Index Distributions",
     ylab="Likelihood of a Given Score", 
     xlab="Fear of Negative Evaluation Index Score",
     font=2,cex=1.25, lwd=linewidth)

lines(d2, col=rgb(0.65,0.75,1,1), lwd=linewidth)
lines(d3, col=rgb(0.3,0.35,1,1), lwd=linewidth)
lines(d4, col=rgb(0,0,0.8,1,1), lwd=linewidth)
lines(d5, col=rgb(0,0,0.4,1,1), lwd=linewidth)
# lines(d6, col="purple", lwd=linewidth)

#text(x=36.5,y=0.06,labels="Age Range:", font=2)

legend(31,0.06, # places a legend at the appropriate place 
       agelabels[1:5], # puts text in the legend
       lty=c(1), # gives the legend appropriate symbols (lines)
       lwd=c(4),
       bg=rgb(0.95,0.95,0.8,0.2),
       title="Age Range:",
       col=c(rgb(0.8,0.9,1,1), rgb(0.65,0.75,1,1),col=rgb(0.3,0.35,1,1), rgb(0,0,0.8,1,1),rgb(0,0,0.4,1,1))
) # gives the legend lines the correct color and width

dev.off()

# In case you want individual histograms:

hist(subset(x=mrd, subset=(Age=="1"))$FNEINDEX)
hist(subset(x=mrd, subset=(Age=="2"))$FNEINDEX)
hist(subset(x=mrd, subset=(Age=="3"))$FNEINDEX)
hist(subset(x=mrd, subset=(Age=="4"))$FNEINDEX)
hist(subset(x=mrd, subset=(Age=="5"))$FNEINDEX)
# hist(subset(x=mrd, subset=(Age=="6"))$FNEINDEX)
# hist(subset(x=mrd, subset=(Age=="7"))$FNEINDEX)

# Library Type:

lmlibtype <- lm(mrd$FNEINDEX~mrd$LibType.f)
anova(lmlibtype)

hist(subset(x=mrd, subset=(LibType=="1"))$FNEINDEX)
hist(subset(x=mrd, subset=(LibType=="2"))$FNEINDEX)
hist(subset(x=mrd, subset=(LibType=="3"))$FNEINDEX)
hist(subset(x=mrd, subset=(LibType=="6"))$FNEINDEX)

# Supervisor y/n:

lmsupervisor <- lm(mrd$FNEINDEX~mrd$Supervisor.f)
anova(lmsupervisor)

# Length of Service:

lmservicelength <- lm(mrd$FNEINDEX~mrd$YrsWorkedInLibraries.f)
anova(lmservicelength)


hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="1"))$FNEINDEX)
hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="2"))$FNEINDEX)
hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="3"))$FNEINDEX)

hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="4"))$FNEINDEX)
hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="5"))$FNEINDEX)
hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="6"))$FNEINDEX)

hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="7"))$FNEINDEX)
# hist(subset(x=mrd, subset=(YrsWorkedInLibraries=="8"))$FNEINDEX)


e1 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="1"))$FNEINDEX)
e2 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="2"))$FNEINDEX)
e3 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="3"))$FNEINDEX)
e4 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="4"))$FNEINDEX)
e5 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="5"))$FNEINDEX)
e6 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="6"))$FNEINDEX)
e7 <- density(subset(x=mrd,subset=(YrsWorkedInLibraries=="7"))$FNEINDEX)

pdf(file="FNEworklengthkernelplot.pdf",width=6,height=5)

linewidth=4

reds <- c("#fff0f0ff","#ffc0c0ff","#ff6060ff","#ff0000ff","#aa0000ff","#800000ff","#000000ff")

plot(e1, 
     col=reds[1], 
     xlim=c(0,40), ylim=c(0,0.065),
     main="Kernel Density Plots of Library Career Length and\nFNE Index Distributions",
     ylab="Likelihood of a Given Score", 
     xlab="Fear of Negative Evaluation Index Score",
     font=2,cex=1.25, lwd=linewidth)

lines(e2, col=reds[2], lwd=linewidth)
lines(e3, col=reds[3], lwd=linewidth)
lines(e4, col=reds[4], lwd=linewidth)
lines(e5, col=reds[5], lwd=linewidth)
lines(e6, col=reds[6], lwd=linewidth)
lines(e7, col=reds[7], lwd=linewidth)

legend(30,0.067, # places a legend at the appropriate place 
       yearsinlibrarieslabels[1:7], # puts text in the legend
       lty=c(1), # gives the legend appropriate symbols (lines)
       lwd=c(4),
       bg=rgb(0.95,0.95,0.8,0.2),
       title="Work Years:",
       col=reds
) # gives the legend lines the correct color and width

dev.off()


# Race/Ethnicity (Dichotomous)
mrd$racedi <- mrd$Ethnicity_origin
mrd$racedi[mrd$Ethnicity_origin=="7"] <- NA

mrd$racedi[mrd$Ethnicity_origin=="2" | 
             mrd$Ethnicity_origin=="3" | 
             mrd$Ethnicity_origin=="4" |
             mrd$Ethnicity_origin=="5" |
             mrd$Ethnicity_origin=="6"] <- "0"

lmracedi <- lm(mrd$FNEINDEX~mrd$racedi)
anova(lmracedi)

# Count of Annual Evalations:

# table(mrd$AnnualFormalEvalCount.f)



