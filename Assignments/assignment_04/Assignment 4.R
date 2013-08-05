# Homework Assignment
# Create a logistic regression model using several independent variables. (40 points)
# 
# As with the linear regression exercise, you can pick your data set, but 
# - It must be a new dataset you've never used before (10 points), and 
# - You must include it with your submission or provide a URL where we can download it.

# using Prostate Cancer data from here:
# http://www.umass.edu/statdata/statdata/data/pros.dat

# goal is to predict capsule penetration given a set of covariates:
# LIST OF VARIABLES:
# 
# Variable  Description			Codes/Values		    	Name
#________________________________________________________________________________
# 
# 1		Identification Code		1 - 380					      ID
# 2		Tumor Penetration of 	0 = No Penetration, 	CAPSULE
#	  	Prostatic Capsule		  1 = Penetration
# 3		Age 				        	Years					        AGE
# 4		Race				        	1= White, 2 = Black		RACE
# 5		Results of the Digital 	1 = No Nodule			  DPROS
#	    	Rectal Exam 		    	2 = Unilobar Nodule (Left)
#							              	3 = Unilobar Nodule  (Right)
#						               		4 = Bilobar Nodule
# 6		Detection of Capsular 	1 = No, 2 = Yes			DCAPS
#	    	Involvement in Rectal Exam 
# 7		Prostatic Specific 		mg/ml				        	PSA
# 	  	Antigen Value
# 8		Tumor Volume Obtained 	cm3				      		VOL
#	    	from Ultrasound
# 9		Total Gleason Score		0 - 10 			      		GLEASON
#________________________________________________________________________________

library(ggplot2)
setwd("~/Documents/GADataScience2013/Assignments/assignment_04")
data <- read.csv("./Prostate.csv")

set.seed(110685)

# It should be a complete, self-contained script with the following features:
# - An n-fold cross validation framework (20 points)
# - Setting a seed in the appropriate place (10 points)
# - It's well commented, and has a brief description at the end about the trends you see (20 points)

data$ID <- NULL
# drop incomplete cases
data <- data[complete.cases(data),]
data$CAPSULE <- factor(data$CAPSULE)
data$RACE <- factor(data$RACE)
data$DPROS <- factor(data$DPROS)
data$DCAPS <- factor(data$DCAPS)


# function to run logistic regression on a train-test pair. it returns the error
lf.errf <- function(train.data, test.data){
  test.labels = test.data$CAPSULE
  test.no.labels = test.data
  test.no.labels$CAPSULE <- NULL
  logit.fit <- glm(CAPSULE ~ ., family='binomial', data=train.data)
  #I round the probabilities to 0 or 1
  lf.prediction <- round(predict(logit.fit, newdata=test.no.labels, type="response"))
  # calculate error for test labels that were not correctly assigned
  this.err <- sum(test.labels != lf.prediction)/length(test.labels)
  return(this.err)
}


#helper function to extract the k-th subset of data
#and to run logistic regression on it. It returns the prediction error.
lf.k.errf <- function(k, whichK, data){
  train.data <- data[whichK != k,]
  test.data <- data[whichK == k,]
  this.err.k <- lf.errf(train.data, test.data)
  return(this.err.k)
}

# helper function that builds a vector index to generate the CV subsamples.
# Code from: https://gist.github.com/dsparks/3695362
build.whichK <- function(kk, data){
  randomDraw <- rnorm(nrow(data))
  kQuantiles <- quantile(randomDraw, 0:kk/kk)
  whichK <- cut(randomDraw, kQuantiles, include.lowest = TRUE)  # Divide randomDraw into kk equally-sized groups
  levels(whichK) <- LETTERS[1:kk]  # (Optionally) Give the levels handier names

  #returns the index vector
  return(whichK)
}

# build an index
kk <- 10 #n of cv folds
whichK <- build.whichK(kk, data)


# for loop on cv subsets
cv.result <- NULL
for(k in levels(whichK)){
  #append results of specific cv as a column
  cv.result <- rbind(cv.result, lf.k.errf(k, whichK, data))
}


mean(cv.result)


# now let's look at trends
# use all the dataset
final.logit.fit <- glm(CAPSULE ~ ., family='binomial', data=data)

# odds ratio:
exp(coef(final.logit.fit))
xtabs(~CAPSULE + RACE, data=data)
xtabs(~CAPSULE + DPROS, data=data)
xtabs(~CAPSULE + DCAPS, data=data)
# Rectal exam seems to be very indicative, with Left Unilobar Nodule twice as likely to imply
# capsule penetration and Right Unilobar Nodule 4.5 times as likely to imply capsule penetration
# AGE, PSA and VOL have ODD ratio = 1 => each point increases the probability by a factor 1
# Detection of Capsular Involvement also seems to increase the risk
# RACE: the 0.5 Odds ratio for RACE2 would imply RACE2 is half as likely to have capsule penetration
# however the sample contains a lot less records for RACE2
# Finally, each point in the Gleason score increases the probability by a factor of 2.6


# now let's look at the various factors, for average PSA, VOL and AGE
new.data <- with(data, data.frame(AGE=round(mean(AGE)), RACE=rep(factor(1:2),8), DPROS=rep(factor(1:4),c(4,4,4,4)), DCAPS=rep(factor(1:2),c(2,2)), PSA=mean(PSA), VOL=mean(VOL), GLEASON=round(mean(GLEASON))))
new.data$CAPSULEpred <- predict(final.logit.fit, newdata=new.data, type='response')
new.data
# the highest probability is obtained for an individual of race 1, DPROS 3 and DCAPS 2
# I suspect that GLEASON score depends on the other factors and thus should not be used as covariate

# let's look at how probability changes with PSA, which is commonly considered a risk factor
# for the sake of simplicity we will consider only 1 case for the other factors
new.data2 <- with(data, data.frame(AGE=round(mean(AGE)), RACE=rep(rep(factor(1:2),8), each=150),
                                  DPROS=rep(rep(factor(1:4),c(4,4,4,4)),each=150),
                                  DCAPS=rep(rep(factor(1:2),c(2,2)),each=150),
                                  PSA=seq(from=0, to=150.0, length.out=150),
                                  VOL=mean(VOL), GLEASON=round(mean(GLEASON))))
new.data2$CAPSULEpred <- predict(final.logit.fit, newdata=new.data2, type='response')
ggplot(new.data2, aes(x=PSA, y=CAPSULEpred)) + geom_line(aes(colour=DPROS), size=1)

#conclusion: risk of CAPSULE penetration increases with PSA.



# Let me know if you have any questions about this!
# Extra Resources for Logistic Regression
# Vanderbilt logistic regression slides:<br>
# http://www.mc.vanderbilt.edu/gcrc/workshop_files/2004-11-12.pdf
# Interpreting odds ratios: <br>
# http://www.ats.ucla.edu/stat/stata/faq/oratio.htm
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm