# IST687
# # Student name: Kartheek Sunkara
# Homework number: 10
# Date submitted: 5 April 2019 (1:28PM)
# Date due: 11 April 2019 (9:00AM) 

# Attribution statement: (choose the one statement that is true)

# 1. I did this homework by myself, with help from the book and professor

setwd("C:\\Users\\KARTHEEK SP\\Desktop\\IST 687") #setting working directory

rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

#Part A: Explore Data Set

#install.packages("RJSONIO")
library(RJSONIO)
#install.packages("ggplot2")
library(ggplot2)

#Part A: Load and condition the data  
#1.The data is available on blackboard (hotelSurveyBarriot), as a JSON file.


dataset.name <- "hotelSurveyBarriot.json"
hotelSurvey <-fromJSON(dataset.name, simplify = TRUE, nullValue = NA)
View(hotelSurvey) #look at JSON file
hotelSurvey <- data.frame(hotelSurvey)
View(hotelSurvey)

#Explore Data Set
str(hotelSurvey)
summary(hotelSurvey)

#Part B: Create a happy customer variable 
#2.To focus on predicting happy customers, we need to generate a new column (where overallCustSat is 8 or higher).

hotelSurvey$happycusts <- ifelse(hotelSurvey$overallCustSat >= 8, "1", "0")

#Part C: Create training and test data sets
#Using techniques discussed in class, create two datasets - one for training, one for testing.
#3.	Pages 235 - 237 of the book describe how to create a training data set and a test data set. Following the strategy in the book, the training data should contain about two thirds of the whole data set, with the remaining one third going to the test data.
#4.	Use the dim( ) function to demonstrate that the resulting training data set and test data set contain the appropriate number of cases.

install.packages("kernlab") #The package used here to create a model
library(kernlab)

data(hotelSurvey) #load the data 

str(hotelSurvey) #inspect the dataset

dim(hotelSurvey) #10000 x 12 size dataset

table(hotelSurvey$happycusts) #table function is a numeric variable output
#so we have 5249 zeroes (unhappy) and 4751 ones (happy) customers and this is the ground truth values   

#out:   0    1 
      #5249 4751 

#Randomizing the dataset befor dividing into train and test datasets 

randIndex <- sample(1:dim(hotelSurvey)[1]) #So all the 10000 entries are getting randomized 
summary(randIndex)
#out:    Min. 1st Qu.  Median  Mean    3rd Qu.  Max. 
#         1    2501    5000    5000    7500     10000 

length(randIndex) #is 10000 (it is as expected)

head(randIndex) #will give u first 6 values of randIndex 

#the indices get changed everytime u sample 

cutPoint2_3 <- floor(2 * dim(hotelSurvey)[1]/3) # calculate the cut point that would divide the spam data set into a two-thirds training set and a one-third test set
cutPoint2_3 #floor() function in the above command chops off any decimal part of the calculation


trainData <- hotelSurvey[randIndex[1:cutPoint2_3],] # build our training set from the first 6666 rows

testData <- hotelSurvey[randIndex[(cutPoint2_3+1):dim(hotelSurvey)[1]],] #creating the test set similar to the above

#Part D: Build a Model using ksvm( ) 
#5.	Build a support vector model using the ksvm( ) function using two or three of the variables to predict a happy customer. 
#Once you have specified the model statement and the name of the training data set, you can use the same parameters as shown on page 237: 
#kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE

svmOutput <- ksvm(happycusts~checkInSat+hotelClean+lengthOfStay, 
              data=trainData, kernel=
            'rbfdot',kpar='automatic', C=100,cross=5, 
            prob.model = TRUE)
#6.	Write a block comment that summarizes what you learned from the book about those parameters. The two parameters of greatest interest are C=5 and cross=3.
#

#7.	Store the output of kvsm( ) in a variable and then echo that variable to the console.   
svmOutput #output structure

hist(alpha(svmOutput)[[1]])

#Part E: Predict Values in the Test Data and Create a Confusion Matrix
#8.	Use the predict( ) function to validate the model against test data. Assuming that you put the output from the ksvm( ) call into svmOutput and that your test data set is in a data frame called testData, the call would be:
  svmPred <- predict(svmOutput, testData, type = "votes")
#9.	Now the svmPred object contains a list of votes in each of its rows. The votes are either for "happy" or "notHappy". Review the contents of svmPred using str( ) and head( ).
#10.	Create a confusion matrix (a 2 x 2 table) that compares the second row of svmPred to the contents of testData$happy variable.
#11.	Calculate an error rate based on what you see in the confusion matrix. See pages 243-244 for more information.




#Part F: Find a good prediction
#12.	Repeat Parts C and D to try and improve your prediction

#13.	Explain, in a block comment, why it is valuable to have a "test" dataset that is separate from a "training" dataset?
  








#4)	Map each numeric attribute to a category  - Since we want to create rules, we should convert the attributes that have a numeric range into buckets (ex. low or high)
#Hint: For Survey attributes that range from 0 to 10 one can use the following code:

createBucketsSurvey <- function(vec)
{
  
  
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 7] <- "High"
  vBuckets[vec < 7] <- "Low"
  return(vBuckets)
}

#Converting data fields to buckets

happyCust <- createBucketsSurvey(hotelSurvey$overallCustSat)
View(happyCust)

Checkin <- createBucketsSurvey(hotelSurvey$checkInSat)
View(Checkin)


cleanhotel <- createBucketsSurvey(hotelSurvey$hotelClean)
View(cleanhotel)


friendly_hotel <- createBucketsSurvey(hotelSurvey$hotelFriendly)
View(friendly_hotel)

# Create another Function

createBucketsSurvey1 <- function(vec)
{
  
  
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  
  return(vBuckets)
}

age <- createBucketsSurvey1(hotelSurvey$guestAge)
View(age)

staylength <- createBucketsSurvey1(hotelSurvey$lengthOfStay)
View(staylength)

booktrip <- createBucketsSurvey1(hotelSurvey$whenBookedTrip)
View(booktrip)

#5)	Count the people in each category for the age and friendliness attributes
#Hint: Use the table( ) command.

t <- table(age)
t
View(t)


t1 <- table(friendly_hotel)
t1
View(t1)

#6)Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command

prop.table(t) #percentage for age.

prop.table(t1) # percentage for friendlyhotel

#7)Show a "contingency table" of percentages for the age and the overall satisfaction variables together. Write a block comment about what you see.
t2 <- table(age, happyCust)
prop.table(t2)

# 26% of the population with high age have high satisfaction

#Part C: Coerce the data frame into transactions

#8)	Install and library two packages: arules and arulesViz.
install.packages("arules")
library(arules)
install.packages('arulesViz')
library(arulesViz)
#install.packages("grid")
library(grid)

#9)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:
ruleDf <- data.frame(happyCust, Checkin, cleanhotel, friendly_hotel, age, staylength, booktrip)
View(ruleDf) 
hotelSurveyX <- as(ruleDf,"transactions")
View(hotelSurveyX)

# hotelSurveyX <- as(ruleDF,"transactions")  Make sure you create a data frame before creating matrix

# 10)	Use the itemFrequency( ) and item FrequencyPlot( ) commands to explore the contents of hotelSurveyX.
itemFrequency(hotelSurveyX) 
itemFrequencyPlot(hotelSurveyX) 
#Observation from the plot: clean has high frequency 

#Part D: Use arules to discover patterns
#Support is the proportion of times that a particular set of items occurs relative to the whole dataset. Confidence is proportion of times that the consequent occurs when the antecedent is present. See the review on the next page.  

#11)	 Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high - above 7).
#apriori refers to the specific algorithm that R will use to scan the data set
#for appropriate rules.
ruleSet <- apriori(hotelSurveyX, 
                   parameter = list(support =0.1, confidence=.5),
                   appearance = list(default="lhs", rhs=("happyCust=High"))
)
ruleSet #55rules

# 3 rules have highest lift. 
plot(ruleSet) #check for high lift, rules with highest lift have 99% confidence

#12)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 
inspect(ruleSet) #view ruleSet
top.support <- sort(ruleSet, decreasing = TRUE, na.last = NA, by = "lift") #to sort the ruleSet list by lift in decreasing order
inspect(top.support) #view sorted ruleSet


#13)	If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, what would those two rules be?  Use a block comment to explain your answer.

#Regarding the highest customer satisfaction For the association rule analysis, due to the highest lift values the combination of (high cleanliness, high whenbookedTrip, average friendliness and high checkIn) 
# and the services: (high cleanliness, high whenbookedTrip and average friendliness,) lead to highest customer satisfaction
# If these are fulfilled most (99%) of the customers will be satisfied


  
  
  
