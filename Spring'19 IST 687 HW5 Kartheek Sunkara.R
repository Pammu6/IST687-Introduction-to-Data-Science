# IST687
# # Student name: Kartheek Sunkara
# Homework number: 5
# Date submitted: 21 Feb 2019 (3:03AM)
# Date due: 21 Feb 2019 (9:00AM) 
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself and some websites: tutorialspoint, r bloggers, rdocumentation, sthda, stackoverflow and rfunction
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

#Before step A
#installing the required jsonlite and curl packages to read json and to read json from web url respectively
install.packages("jsonlite")
library("jsonlite")
install.packages("curl")
library("curl")

#Step A: Load the data
accidents_data <- fromJSON("https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD") #getting the json file from the given url

#Step B: Clean the data

accidents <- data.frame(accidents_data[['data']]) #Extracting data from the json file and storing it as a data frame
#the accidents_data has both metadata and data, but we only need data
#2) Removing the 8 columns
accident_analysis <- accidents[,-(1:8)] #removing the first 8 columns and storing it in another variable

#3) creating a vector for the required column names of the data frame
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
View(namesOfColumns)
names(accident_analysis) <- namesOfColumns #naming the columns of dataframe with namesOfColumns
View(accident_analysis)#Viewing accident_analysis
head(accident_analysis) #viewing the first few values of data frame to check for the reflection of column names


#installing sqldf package
install.packages("sqldf")
library("sqldf")

#removing extra space from the day of week using gsub function, and as it removes the space, the factor gets converted to character, so next step makes it a factor again
accident_analysis$DAY_OF_WEEK <- gsub('\\s+','',accident_analysis$DAY_OF_WEEK)
accident_analysis$DAY_OF_WEEK<-as.factor(accident_analysis$DAY_OF_WEEK)

#Step C: Explore the data - using the dataframe you created

#4) countimg the number of accidents that had injuries
sqldf("select count(*) from accident_analysis where INJURY == 'YES' AND INJURY IS NOT NULL")

#5) counting the number of accidents that took place on Sunday
sqldf("select count(*) from accident_analysis where DAY_OF_WEEK == 'SUNDAY'")



#6) The total count of injuries according to the day of week 
sqldf("select DAY_OF_WEEK, count(*) from accident_analysis where INJURY == 'YES'  group by DAY_OF_WEEK")


#Step D: Explore the data - using dplyr

#importing package for the count function
install.packages("plyr")
library(plyr)

#7) count of accidents which had injuries. The difference in output is that it gives the count of INJURY=NO under FALSE and for INJURY=YES under TRUE, as I have given the group by condition specifying the value of INJURY. 
acc_with_injury <- tapply(accident_analysis$INJURY,accident_analysis$INJURY == "YES",count)
acc_with_injury


#8) counting the number of accidents that happened on Sunday. The difference in output is that is shows the count for other days under FALSE and for Sunday under TRUE as I have given the group by condition specifying the value of DAY_OF_WEEK.
acc_on_sunday <- tapply(accident_analysis$DAY_OF_WEEK,accident_analysis$DAY_OF_WEEK =="SUNDAY",count)
acc_on_sunday


#9) count of injuries according to the days, with output showing count of INJURY=NO under FALSE and count of INJURY=YES under TRUE as my group by condition contains a specific value of INJURY. 
injury_list_day <- tapply(accident_analysis$DAY_OF_WEEK,accident_analysis$INJURY=="YES",count)
injury_list_day

#10 Function chaining is possible with dpylr, table dataframes are smarter. 
#dpylr is faster in its calculations and code is not complex to write.

#11 number of vehicles in accidents on Friday
#Creating empty vector
p <- c()
# Loop over accident_analysis rows
#only if that day is FRIDAY then append the corresponding v_count value to p (empty vector) (includes NA's)
for (row in 1:nrow(accident_analysis)) {
  v_count <- accident_analysis[row,"VEHICLE_COUNT"]
  day  <- accident_analysis[row, "DAY_OF_WEEK"]
  
  if(day == "FRIDAY") {
    p <- c(p,v_count)
  } 
}
#Omitting NA's from p since for fridays that have vehicle count as NA were also appended
j <- na.omit(p)

#Histogram or distribution of the number of vehicles in accidents on Friday
acf <- hist(j,12)
acf


#12.	How does this distribution compare with the distribution of the number of vehicles in accidents on Sunday?  (use a histogram and quantile)
#On a given sunday it is more likely that the number of vehicles in accidents fall below 2
#whereas on a given friday it is more likely that the number of vehicles in accidents fall from 2 to 3
#Also the vehicles in accidents on sundays is less than that of on fridays by around 600 
#Creating empty vector
k <- c()
# Loop over accident_analysis rows
#only if that day is SUNDAY then append the corresponding v_count value to p (empty vector) (includes NA's)
for (row in 1:nrow(accident_analysis)) {
  v_count <- accident_analysis[row,"VEHICLE_COUNT"]
  day  <- accident_analysis[row, "DAY_OF_WEEK"]
  
  if(day == "SUNDAY") {
    k <- c(k,v_count)
  } 
}
#Omitting NA's from p since for fridays that have vehicle count as NA were also appended
l<- na.omit(k)

#Histogram or distribution of the number of vehicles in accidents on Friday
acs=hist(l,12)
#displaying graph
acs

quantile(l,probs=0.75)
quantile(j,probs=0.75)
#The quantile tells that the nth percentile of both cases (Friday and Sunday) are same
#End of homework 5


