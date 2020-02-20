# IST687
# # Student name: Kartheek Sunkara
# Homework number: 4
# Date submitted: 5 Feb 2019 
# Date due: 6 Feb 2019  
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself and with the help of professor 
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

#set working directory...
testvector <- 1:10

print(paste("The mean is ",mean(testvector)))
print(paste("The median is ",median(testvector)))
print(paste("The minimum value is ",min(testvector)))
print(paste("The maximum value is ",max(testvector)))
print(paste("The standard deviation is ",sd(testvector)))
print(paste("The 0.05 quantile value is ",quantile(testvector,probs=0.05)))
print(paste("The 0.95 quantile value is ",quantile(testvector,probs=0.95)))

avg <- mean(testvector) 
med <- median(testvector) 
mini <- min(testvector)
maxi <- max(testvector)
std <- sd(testvector)
quantile(testvector, probs=0.05)
quantile(testvector, probs=0.95)

printVecinfo(testvector)

printVecinfo <- function(vector) {
  print(paste("The mean is ",mean(vector)))
  print(paste("The median is ",median(vector)))
  print(paste("The minimum value is ",min(vector)))
  print(paste("The maximum value is ",max(vector)))
  print(paste("The standard deviation is ",sd(vector)))
  print(paste("The 0.05 quantile value is ",quantile(vector,probs=0.05)))
  print(paste("The 0.95 quantile value is ",quantile(vector,probs=0.95)))

}