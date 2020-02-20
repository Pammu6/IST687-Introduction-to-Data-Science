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

#install.packages("kernlab") #The package used here to create a model
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
            'rbfdot',kpar='automatic', C=50,cross=20, 
            prob.model = TRUE)

#6.	Write a block comment that summarizes what you learned from the book about those parameters. The two parameters of greatest interest are C=5 and cross=3.

#By specifying a small value of C, we get more number of support vectors. As C increases, the value 
#of number of support vectors becomes saturated.
# As cross values increase, the algorithm is trained too specific to the this problem. Also, cross validation error 
#and sigma values decrease as these two values increase.

#7.	Store the output of kvsm( ) in a variable and then echo that variable to the console.   
svmOutput #output structure

hist(alpha(svmOutput)[[1]])

#Part E: Predict Values in the Test Data and Create a Confusion Matrix

#8.	Use the predict( ) function to validate the model against test data. Assuming that you put the output from the ksvm( ) call into svmOutput and that your test data set is in a data frame called testData, the call would be:
  svmPred <- predict(svmOutput, testData, type = "votes")

#9.	Now the svmPred object contains a list of votes in each of its rows. The votes are either for "happy" or "notHappy". Review the contents of svmPred using str( ) and head( ).
str(svmPred)
#View(svmPred)
head(svmPred,n=5)

happyPred<- svmPred[2,]
happyPred

str(happyPred)
#View(happyPred)

#10
CompTable <- data.frame(testData$happycusts,happyPred)
#iew(CompTable)
table(CompTable)

# Adjust the column names to "Test" and "Pred"
colnames(CompTable) <- c("Test", "Pred")
res <- table(CompTable)
res

#11
percentCorrect <- (res[1,1] + res[2,2])/(sum(res))
percentCorrect   #78.97%

errorRate <- 1-percentCorrect
errorRate # 21.02%

#Part F: Find a good prediction

#12.	Repeat Parts C and D to try and improve your prediction

# The above model predics the best with the best possible error rate.

#13.	Explain, in a block comment, why it is valuable to have a “test” dataset that is separate from a “training” dataset?

#By having a separate test dataset and training dataset data overfitting to the algorithm(model ) can be avoided. 






