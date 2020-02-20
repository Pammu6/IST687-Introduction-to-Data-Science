## IST687
## Student name: Kartheek Sunkara
## Project
## Date submitted: 21 Feb 2019 (3:03AM)
## Date due: 21 Feb 2019 (9:00AM) 
## 
## Attribution statement: (choose the one statement that is true) 
## 1. I did this homework by myself and some websites: tutorialspoint, r bloggers, rdocumentation, sthda, stackoverflow and rfunction
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

##Setting working directory 
##setwd("C:\\Users\\KARTHEEK SP\\Desktop\\IST 687")

##importing the dataset Spring19Survey.csv 
df <- read.csv('C:/Users/KARTHEEK SP/Desktop/IST 687/spring19survey.csv')
##Viewing the dataframe to have an overview
#View(df)

##Summary of the columns 
#summary(df)

##Know the column names 
colnames(df)


# [A] Data Cleaning (Finding rows and columns with all NA's and cleaning them)

##Since we can perform analysis having NA's in the dataframe it is not sugegstible to delete the columns or rows that have NA's 
##it would lead to the data loss 
##But we can delete the rows/coolumns with all NA values 

###
#is.na(x)
#df <- complete.cases(df)
#newdata <- na.omit(mydata)
###


# ### (a.1) Removing columns with all NA values
# ##Need dpylr library
# library(dplyr)
# ##Get the logic index, where TRUE corresponding to the columns with all NAs
# onlyNAcolumns_idx <- df %>%
#   is.na() %>%
#   apply(MARGIN = 2, FUN = all)
# 
# ##Get the table without columns containing only NAs:
# #df[,!onlyNAcolumns_idx]
# 
# ##You get true if you execute this:
# length(onlyNAcolumns_idx) == ncol(df)
# 
# ## (a.2) Removing rows with all NA values
# onlyNArows_idx <- df %>%
#   is.na() %>%
#   apply(MARGIN = 1, FUN = all)
# onlyNArows_idx
# #df[!onlyNArows_idx,]
# 
# ##You get true if you execute this:
# length(onlyNArows_idx) == nrow(df)
# ###

## (a.1) Checking for the columns that have all NA values
onlyNAcolumns_idx  <- colSums( !is.na(df) ) == 0 # avoid apply loop

## (a.2) Checking for the rows that have all NA values
onlyNArows_idx  <- rowSums( !is.na(df) ) == 0 # avoid apply loop

#Counting the masked values of df[onlyNArows_idx] so that the rows with all NA values are filtered out 
nrow(df[onlyNArows_idx])==nrow(df)

#To find the mean of Satisfaction and replace the NA values with it
summary(df['Satisfaction'])

#Finding the mean
m1 <- mean(df[,"Satisfaction"],na.rm=TRUE)
#replacing NA's with mean

# for(i in 1:nrow(df)){
#   df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
# }

# #install.packages('zoo')
# library(zoo)
# na.aggregate(df)