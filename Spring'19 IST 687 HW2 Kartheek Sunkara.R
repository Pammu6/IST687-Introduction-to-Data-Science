# IST687
# # Student name: Kartheek Sunkara
# Homework number: 1
# Date submitted: 18 Jan 2019 
# Date due: 22 Jan 2019  
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself,and the professor 
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

#Step A: Initialize an 'arrests' dataframe
#1	Copy USArrests into a new variable (called 'arrests')
arrests <- USArrests

#View data 
View(arrests)

#Step B: Explore the assault rate
#2)	Write a comment: Is a higher or lower assault rate best?
#Lower

#3)	Which state has the best assault rate? 
#Taking index
min.arrests.index <- which.min(arrests$Assault) #$ is a way to call column
min.arrests.index
#Which state has the best assasault rate? State?
arrests[min.arrests.index,]

#Step C: Explore the murder rate  
#4)Which state has the highest murder rate?
max.arrests.index <- which.max(arrests$Murder) #$ is a way to call column
max.arrests.index 
#Which state has the best assasault rate? State?
arrests[max.arrests.index,] #Georgia

#5)Create a sorted dataframe, based on descending murder rate
sorted.arrests <- arrests[order(-arrests$Murder),] #order to sort
View(sorted.arrests)

#6)Show the ten states with the highest murder rate
head(sorted.arrests,10)

#7)What is the value of the 20'th row, third column (in the sorted dataframe)? Use R code (not visual inspection)
sorted.arrests[20,3] #Referencing the array.. X and Y

#Step D: Which state is the least safe? Explain your logic
#North Carolina

#9)Write a comment to explain your logic
#I gave weightages to 3 different crimes. So murder is multiplied by 3, rape is multiplied by two and assault by 1
#Now average of all these three are taken and divided by their corresponding urban population 
#Here murde (more severe crime) > rape (severe crime) > assault (considerable crime)
#Meaning rape is less severe than murder, assault is less severe than rape
x <- c()
col <- c(1)
#8)Write the R code to determine your answer
for (row in 1:nrow(arrests))
    x[row] <- ((arrests[row,col]*3+arrests[row,col+1]*1+arrests[row,col+3]*2)/arrests[row,col+2])
    
#Now the maximum of the vector x gives the maximum crime rate
#Its corresponding state will be found by using the index of max of x
row.names(arrests[a,]) #North Carolina





         




