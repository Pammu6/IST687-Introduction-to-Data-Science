# IST687
# # Student name: Kartheek Sunkara
# Homework number: 1
# Date submitted: 18 Jan 2019 
# Date due: 22 Jan 2019  
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself,and the professor 
rm(list=ls())
dev.off()
cat('\014')

#STEP A
#1.Define a vector 'grades', which contains the numbers 4.0, 3.3 and 3.7 (i.e., three numbers in the vector 'grades').  
grades <- c(4.0,3.3,3.7)

#2.Define a vector 'courseName', which contain the strings "Bio", "Math", "History". 
courseName <- c("Bio","Math","History")

#3.Defining a variable 'BetterThanB' with a value 3
BetterThanB <- 3

#STEP B
#4.Computing the average of the grades vector with the mean() function
mean(grades)  #[1] 3.666667

#5.Calculate the number of observations in the grades vector with the length() 
total.length <- length(grades)

#6.Output the value of 'total.length'
total.length  #[1] 3

#7.Calculate the sum of  'grades' with the sum() function, store the result in 'total'
total <- sum(grades)

#8.Recompute the average of all the grades by combining questions 5 and 7
newavg <- total/total.length

#STEP C: 
#9.Using the max/min functions in R Compute the max grades, store the result in 'maxG'
maxG <- max(grades)

#10.Compute the min grades, store the results in 'minG'
minG <- min(grades)

#STEP D
#11.Create a new vector called betterGrades, which is the grades + 0.3 (each grade improved each grade by  0.3 points)
betterGrades <- grades+0.3
#12.Compute the average of betterGrades
average <- mean(betterGrades)

#Step E: Using Conditional if statements
#13.Test if maxG is greater than3.5 (output "yes" or "no")
if (maxG > 3.5) "yes" else "no" #[1] "yes" 

#14)	 Test if minG is greater than the variable 'BetterThanB'' (output "yes" or "no")
if (minG > 'BetterThanB') "yes" else "no" #[1] "no"

#Step F: Accessing an element in a vector
#15)  Output the name of the second class, in the 'courseName' vector
#Syllabus Highlights
courseName[2] #[1] "Math"






