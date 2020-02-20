# IST687
# # Student name: Kartheek Sunkara
# Homework number: 4
# Date submitted: 13 Feb 2019 
# Date due: 13 Feb 2019  
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself and with the help of professor 
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space


#Function named readstates 
readstates <- function(){
  dfStates <- read.csv("states.csv") #reading the csv file into dfStates
  View(dfStates) #view in another tab
  
  #Step B: Clean the dataframe
  
  
  #2) Use View( ), head( ), and tail( ) to examine the data frame. Remove unneeded columns and rows by using the minus sign
  #in the rows or columns of the [ , ] accessor. 
  head(dfStates) #look at the top
  tail(dfStates) #look at the bottom
  str(dfStates) #structure of data types
  summary(dfStates) #descriptive statistics
  
  #Remove unnecessary columns and rows by using the minus sign in the rows/columns
  nrow(dfStates) #Validate 53 rows
  dfStates <- dfStates[-1,] #Removes USA in first row
  
  #Remove the last row (for Peurto Rico) and make sure there are 51 rows
  nrow(dfStates) #validate 52 rows
  num.row <- nrow(dfStates) # look at the number of rows
  num.row
  dfStates <- dfStates[-num.row,] #subtracts the last row or the total number of rows
  View(dfStates) #Validate 51 rows
  
  #Make sure there are precisely five columns. 
  dfStates <- dfStates[,-1:-4]
  View(dfStates)
  
  #3.	Rename the columns with the following names: stateName, population, popOver18, percentOver18. Hint: use colnames( )
  colnames(dfStates) <- c("stateName","population","popover18","percentover18")
  View(dfStates)
  return(dfStates)
}

#Function 2
printVecinfo <- function(vector) {
  print(paste("The mean is ",mean(vector)))
  print(paste("The median is ",median(vector)))
  print(paste("The minimum value is ",min(vector)))
  print(paste("The maximum value is ",max(vector)))
  print(paste("The standard deviation is ",sd(vector)))
  print(paste("The 0.05 quantile value is ",quantile(vector,probs=0.05)))
  print(paste("The 0.95 quantile value is ",quantile(vector,probs=0.95)))
  hist(vector)
}
# Creating a test vectot
testVector <- 1:10
#3)Test the function with this vector: testVector <- 1:10. 
avg <- mean(testvector) 
med <- median(testvector) 
mini <- min(testvector)
maxi <- max(testvector)
std <- sd(testvector)
quantile(testvector, probs=0.05)
quantile(testvector, probs=0.95)
#Through function required characteristics of testVector are printed along with histogram
printVecinfo(testVector)
#Calling readststates function 
states <- readstates()

#Sample 20 observations from states$population and use printVecInfo( ) to display the characteristics of the resulting sample, and then display the results as a histogram.
#Observing the characteristics of 6 random samples 
s1 <- sample(states$population,20)
printVecinfo(s1)
s2 <- sample(states$population,20)
printVecinfo(s2)
s3 <- sample(states$population,20)
printVecinfo(s3)
s4 <- sample(states$population,20)
printVecinfo(s4)
s5 <- sample(states$population,20)
printVecinfo(s5)
s6 <- sample(states$population,20)
printVecinfo(s6)

#Each result is different because, sample function creates random samples from the data. Hence the characteristics of each sample is different. So is its histogram.

#Part D: Replicate the sampling
a1 <- rep(s1,2000)
printVecinfo(a1)
a2 <- rep(s2,1000)
printVecinfo(a2)
#After replicating for 2000 times the characteristics like mean,mode,standard deviation changes resulting in a different histogram
# Also the sample is being replicated many times (2000 here). After such high replications, the resulting vector would look/pertend more of the parent (original data from which sample is taken)
#Hence the  histogram of vectors in part C and D differ
