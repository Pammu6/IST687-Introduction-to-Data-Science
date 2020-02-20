# IST687
# # Student name: Kartheek Sunkara
# Homework number: 8
# Date submitted: 27 March 2019 (3:03AM)
# Date due: 27 March 2019 (9:00AM) 
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself and with the help of professor
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

# Set working directory 
# Change to the folder containing your homework data files
#setwd("~/MyDesktop/IST687/Homework")

#Step A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveySherison), as a JSON file.

#install.packages("RJSONIO")
library(RJSONIO)
#install.packages("jsonlite")
library("jsonlite")
#install.packages('ggplot2')
library(ggplot2)

#1.Load the dataset 

dataset.name <- "hotelSurveySherison.json"
hotelSurveyOut <-fromJSON(dataset.name, simplify = TRUE, nullValue = NA)
View(hotelSurveyOut) # Look at JSON File
hotelSurveyOut <- data.frame(hotelSurveyOut)
View(hotelSurveyOut)

#2.	Use the str command to make sure you can see the following attributes
str(hotelSurveyOut)

#StepB
#3.Create bivariate plots for each of the attributes.
#Your code should produce nine separate plots. Make sure the Y-axis and X-axis are labeled. Keeping in mind that the overall customer satisfaction
#is the outcome (or dependent) variable, which axis should it go on in your plots?

hotelSurveyOut$overallCustSatJ <- jitter(hotelSurveyOut$overallCustSat)

#ggplot for hotelsize
hotelSurveyOut$hotelSizeJ <- jitter(hotelSurveyOut$hotelSize)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=hotelSizeJ)) + geom_point()

#ggplot for hotelfriendly
hotelSurveyOut$hotelFriendlyJ <- jitter(hotelSurveyOut$hotelFriendly)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=hotelFriendlyJ)) + geom_point()

#ggplot for hotelclean
hotelSurveyOut$hotelCleanJ <- jitter(hotelSurveyOut$hotelClean)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=hotelCleanJ)) + geom_point()

##ggplot for checkinsat
hotelSurveyOut$checkInSatJ <- jitter(hotelSurveyOut$checkInSat)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=checkInSatJ)) + geom_point()

#ggplot for whenbookedtrip
hotelSurveyOut$whenBookedTripJ <- jitter(hotelSurveyOut$whenBookedTrip)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=whenBookedTripJ)) + geom_point()

#ggplot for lengthofstay
hotelSurveyOut$lengthOfStayJ <- jitter(hotelSurveyOut$lengthOfStay)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=lengthOfStayJ)) + geom_point()

#ggplot for guestage
hotelSurveyOut$guestAgeJ <- jitter(hotelSurveyOut$guestAge)
ggplot(hotelSurveyOut, aes(y=overallCustSatJ,x=guestAgeJ)) + geom_point()

#4.What do you observe from the plots? Note via a block comment
#It is observed that with the value of the attributes such as hotelClean, checkInSat, hotelFriendly increases the satisfaction also increases in comparison to other independent variables.

#Step C: Generate a linear model  

#5.Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response). Refer to page 202 in the text for syntax and explanations of lm( ). Make sure to include all predictors in one model - NOT different models each with one predictor.

hotelSurvey <- data.frame(hotelSurveyOut)
hotelSurvey <- hotelSurvey[,-11:-19]
View(hotelSurvey)

m1<- lm(formula = overallCustSat ~ ., data = hotelSurvey)

summary.lm(m1)

#6.Report the R-Squared in a comment. Which of the predictors are statistically significant in the model? In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant. 

# Multiple R-squared:  0.6702,	Adjusted R-squared:  0.6682 
# Statistically Significant predictors are checkInSat, hotelClean, hotelFriendly, guestAge, lengthOfStay, WhenBookedTrip.

#The below mentioned predictors are statistically significant since the p-value is less than 0.05

#checkInSat Co-efficient is  -2.381e-01 
#hotelClean Co-efficient is  4.042e-02
#hotelFriendly Co-efficient is  1.122e+00
#guestAge Co-efficient is  -1.205e-01 

P2 <- lm(formula = overallCustSat~checkInSat+hotelClean, data=hotelSurvey)
summary(P2) # Regression model with two variables

#lengthOfStay Co-efficient is  -3.284e-01 
#WhenBookedTrip Co-efficient is  6.421e-03 


# After running the regression model, we observe that the dependent variable (overallCustSat) gets affected by 6 statistically significant independent variables namely checkInSat, hotelClean, hotelFriendly, guestAge, lengthOfStay, WhenBookedTrip.
#7.	Write a block comment that explains in a narrative your overall interpretation of the model. Make sure to refer to each variable (one dependent and three independent) by a descriptive name (i.e., not X1, X2, etc.).

#The variation is with respect to one variable.
#One variable is not significant enough (not much of a great factor contributing to overall custsat) to explain the variation in overall customer satisfaction.
#Since in m3, other variables give us an r squared value close to 0.4 we can rely on m2 with hotelFriendly and m7 with hotelClean to  explain the variation

#Step D: Generate a different linear model 

#8.Next, create a different regression model predicting the overall customer satisfaction from the one variable you think is best.  Then create another using two variables.
P1 <- lm(formula = overallCustSat~guestAge,data=hotelSurvey)
summary(P1) # Regression model with one variable

# with 2 variables
#9.	Write a block comment comparing the two lm models in #8.
#The adjusted R-squared value of the P2 model is more than the P1 model. As the number of statistically significant independent variables increase in the model the R-squared values improves.