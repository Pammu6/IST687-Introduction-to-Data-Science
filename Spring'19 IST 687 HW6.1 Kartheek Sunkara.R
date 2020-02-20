# IST687
# # Student name: Kartheek Sunkara
# Homework number: 6
# Date submitted: 26 Feb 2019 (1:28PM)
# Date due: 28 Feb 2019 (9:00AM) 

# Attribution statement: (choose the one statement that is true)

# 1. I did this homework by myself, with help from the book and professor


# Run these three functions to get a clean test of homework code

dev.off() # Clear the graph window

cat('\014')  # Clear the console

rm(list=ls()) # Clear all user objects from the environment!!!



# Set working directory 

# Change to the folder containing your homework data files

#setwd("~/MyDesktop/IST687/Homework")



# Homework specific code goes below here

#Step A: Load and Merge datasets

#1)	Read in the census dataset (using the function created in HW 3)

dfStates <- read.csv("states.csv")
View(dfStates)
str(dfStates)  # structure of data types
summary(dfStates)
nrow(dfStates) #validates 53 rows
dfStates <-dfStates[-1,] #Removes United States in the first row

#remove last row
nrow(dfStates) # validate 52 rows
num.row <-nrow(dfStates) #look at the number of rows
num.row
dfStates <- dfStates[-num.row,] # subtracts the last row or the total number of rows
View(dfStates)# Validate 51 rows
nrow(dfStates)
#row.names(dfStates)
dfStates <-dfStates[,-1:-4] # remove the 4 columns specified
View(dfStates) # Validate the columns removed
row.names(dfStates)<- NULL
View(dfStates)

# Rename the columns with the following names: stateName, population, popOver18, percentOver18
colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")
#colnames(dataset)[i]<-name (eg : colnames(dataset)[3]<-"Ethnic") for changing the name of ith column in a table
View(dfStates)

#2)	Copy the USArrests dataset into a local variable (similar to HW 2)

data()
arrests <- USArrests # copying dataset into a variable
View (arrests)

str(arrests) # gives the structure of arrests
summary(arrests) # gives the summary of arrests i.e min, max, mean, 1st and 3rd quantile

#3)	Create a merged dataframe -- with the attributes from both dataset
#find the names of the columns to pick one to merge on

arrests$stateName <- rownames(arrests)
View(arrests)

#merge the dataframes by statename
mergeDF<-merge(dfStates, arrests, by = "stateName")
View(mergeDF)
str(mergeDF)
summary(mergeDF)

#Step B: Explore the Data - Understanding distributions
#4)	Create separate histograms using ggplot2() for the population, murder rate, assault and rape columns

install.packages("ggplot2") #insatlling packages
library("ggplot2")

#histogram for population
myPlotPop <- ggplot(mergeDF, aes(x=population)) #assigning the column of data to be plotted
myPlotPop <- myPlotPop + geom_histogram(binwidth=500000) #creating an histogram
myPlotPop <- myPlotPop + ggtitle("Histogram of population") #adding a title
myPlotPop

#histogram for Murder
myPlotPop <- ggplot(mergeDF, aes(x=Murder)) #assigning the column of data to be plotted
myPlotPop <- myPlotPop + geom_histogram(binwidth=0.5) #creating a histogram
myPlotPop <- myPlotPop + ggtitle("Histogram of Murder Rate") #adding a title
myPlotPop

#histogram for Assault
myPlotPop <- ggplot(mergeDF, aes(x=Assault)) #assigning the column of data to be plotted
myPlotPop <- myPlotPop + geom_histogram(binwidth=10) #creating an histogram
myPlotPop <- myPlotPop + ggtitle("Histogram of Assault") #adding a title
myPlotPop

#histogram for Rape
myPlotPop <- ggplot(mergeDF, aes(x=Rape)) #assigning the column of data to be plotted
myPlotPop <- myPlotPop + geom_histogram(binwidth=1) #creating a histogram
myPlotPop <- myPlotPop + ggtitle("Histogram of Rape") #adding a title
myPlotPop
#Another way to do this
ggplot(mergeDF, aes(x=population)) + geom_histogram(binwidth=500000, color="white", fill="black")
ggplot(mergeDF, aes(x=Murder)) + geom_histogram(binwidth=0.5, color="white", fill="black")
ggplot(mergeDF, aes(x=Assault)) + geom_histogram(binwidth=10, color="white", fill="black")
ggplot(mergeDF, aes(x=Rape)) + geom_histogram(binwidth=1, color="white", fill="black")

#5)	Create a boxplot for the population, and a different boxplot for the murder rate
#boxplot for the population
boxp_pop <- ggplot(mergeDF ,aes(x=factor(0),y = population)) + geom_boxplot()
boxp_pop
#boxplot for the murder rate
boxp_pop <- ggplot(mergeDF ,aes(x=factor(1),y = Murder)) + geom_boxplot()
boxp_pop

#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)

# Boxplot is more helpful for me than a histogram because , with boxplot its possible to view the median and quantile.

#Step C: Which State had the Most Murders - bar charts
#7)	Calculate the number of murders per state

mergeDF$numMurders <- mergeDF$population*mergeDF$Murder/10000 #For the easier calculation dividing it by 10000.
View(mergeDF)

# 8)	Generate a bar chart, with the number of murders per state
#Hint: use the geom_col() function

g <- ggplot(mergeDF , aes(x=stateName , y=numMurders)) + geom_col()  #Creating a barchart
g

# 9)	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels, also add a title named "Total Murders".
# Hint: use theme(axis.text.x = element_text(angle = 90, hjust = 1))

g <- ggplot(mergeDF , aes(x=stateName , y=numMurders)) + geom_col()
g <- g + ggtitle("Total Murders")
g <- g + theme(axis.text.x=element_text(angle = 90 , hjust = 1))#Rotating the text on x-axis for better visiblity
g

# 10) Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate from low to high

g <- ggplot(mergeDF , aes(x=reorder(stateName , numMurders) , y = numMurders)) + geom_col() #Reordering the numMurders for gradual chart
g <- g + theme(axis.text.x = element_text(angle = 90 , hjust=1))
g <- g + ggtitle("Murders by State")
g

#11)Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar

g <- ggplot(mergeDF , aes(x=reorder(stateName , Murder) , y = numMurders , fill=percentOver18)) + geom_col() # using fill to show percentOver18 as the color of the bar
g <- g + theme(axis.text.x = element_text(angle = 90 , hjust=1))
g

#Step D: Explore Murders - scatter chart
#12)	 Generate a scatter plot - have population on the X axis, the percent over 18 on the y axis, and the size & color represent the number of murders.

#Creating a scatterplot and macthing the size and colour with numMurders
scatterplot <- ggplot(mergeDF , aes(x=population , y = percentOver18)) + geom_point(aes(size = numMurders, color = numMurders)) + scale_color_gradient(low="white",high="red")
scatterplot














