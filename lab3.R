# IST687
# # Student name: Kartheek Sunkara
# Homework number: 3
# Date submitted: 5 Feb 2019 
# Date due: 6 Feb 2019  
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

#set working directory

#.....Cleaning data frames.......

#Cannot get them with the help of the url. 
 #Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame
 #csvFile <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv")
 #dfStates <- read.csv(url(csvFile),stringsAsFactors=FALSE)

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
num.rows
dfStates <- dfStates[-num.row,] #subtracts the last row or the total number of rows
View(dfStates) #Validate 51 rows

#Make sure there are precisely five columns. 
dfStates <- dfStates[,-1:-4]
View(dfStates)

#3.	Rename the columns with the following names: stateName, population, popOver18, percentOver18. Hint: use colnames( )
colnames(dfStates) <- c("stateName","population","popover18","percentover18")
View(dfStates)

#Step C: Explore the dataframe
#4)	Calculate the average population of the states

avgpop <- mean(dfStates$population)
avgpop

#5)	Find the state with the highest population  (use which.max)
x <- which.max(dfStates$population)
highpop <- dfStates$stateName[x]

#Generate a histogram of populations, what u observe
#Many of the states are leess populated. Around 3-4 countries have a population greater than 20 million
hist(dfStates$population,breaks=20)

#7)	Sort the data frame by population (hint the use 'order' function)
sortOrder <- order(dfStates$population)
dfStates <- dfStates[order(dfStates$population),]
View(dfStates)

#8)	Show the 10 states with the lowest populations
head(dfStates,10)

#9)	Use barplot( ) to create a plot of each of the population from the sorted dataframe.  What do you observe?
barplot(dfStates$population)
#The bar plot is a J shaped curve. It implies that many states have less than 10 million population

