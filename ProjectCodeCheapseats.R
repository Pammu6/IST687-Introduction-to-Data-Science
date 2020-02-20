
flightData <- read.csv("spring19survey.csv")
dataset <-flightData
#converting all integer columns to numeric
dataset$Age <- as.numeric(dataset$Age)
dataset$Flight.time.in.minutes <- as.numeric(dataset$Flight.time.in.minutes)
dataset$Day.of.Month <- as.numeric(dataset$Day.of.Month)
dataset$Flight.Distance <- as.numeric(dataset$Flight.Distance)
dataset[5:8] <- lapply(dataset[5:8],as.numeric)
dataset[10:12] <- lapply(dataset[10:12],as.numeric)
dataset[22:24] <- lapply(dataset[10:12],as.numeric)


#checking for any rows which are not complete
sum(!complete.cases(dataset)) #4113
ncol(dataset) #29
nrow(dataset) #194833


#Taking only the data whose flights are not cancelled
Airdata <- dataset[dataset$Flight.cancelled=="No",]
nrow(Airdata)#191230   ->>>>>>  3603 flights cancelled
str(Airdata)
#Airdata <- data.frame(lapply(Airdata, trimws), stringsAsFactors = FALSE)
#str(Airdata)
#checking for any rows which are not complete for the new dataset
sum(!complete.cases(Airdata)) #510
sum(is.na(Airdata$Arrival.Delay.in.Minutes)) #0
sum(is.na(Airdata$Flight.time.in.minutes)) #502
sum(is.na(Airdata$Departure.Delay.in.Minutes)) #0
is.null(Airdata)
#View(Airdata)
nrow(Airdata)
nrow(dataset)
str(Airdata)


#converting all integer columns to numeric
Airdata$Age <- as.numeric(Airdata$Age)
Airdata$Flight.time.in.minutes <- as.numeric(Airdata$Flight.time.in.minutes)
Airdata$Day.of.Month <- as.numeric(Airdata$Day.of.Month)
Airdata$Flight.Distance <- as.numeric(Airdata$Flight.Distance)
Airdata[5:8] <- lapply(Airdata[5:8],as.numeric)
Airdata[10:12] <- lapply(Airdata[10:12],as.numeric)
Airdata[22:24] <- lapply(Airdata[10:12],as.numeric)
View(Airdata)


#replacing na values with mean

for(i in 1:ncol(Airdata)){
  Airdata[is.na(Airdata[,i]), i] <- mean(Airdata[,i], na.rm = TRUE)
}


#Validating that the data is cleaned
# complete.cases: Return a logical vector indicating which cases are complete, i.e., have no missing values.
sum(!complete.cases(Airdata)) # reduced from 510 to zero
is.null(Airdata)
#View(Airdata)
nrow(Airdata)
nrow(dataset)
str(Airdata)


#removing unwanted columns from the Survey dataset

# 6 Year.of.First.Flight
# 14 Day.of.Month
#19 Origin.State (coz we have origin city and destination)
#21 Destination.State
#25 Flight.cancelled
#27 Flight.Distance
#28 Arrival.Delay.greater.5.Mins

Airdata <- Airdata[-c(6,14,19,21,25,27,28)]
str(Airdata) # 22 variables
summary(Airdata)
View(Airdata)


#########################RUN ONLY WHEN NEEDED############################
#Converting the Satisfaction Column to numeric - Run this line only when required
Airdata$Satisfaction <- as.numeric(as.character(Airdata$Satisfaction))
str(Airdata)


#trimming the Airline name column
Airdata$Airline.Name <- trimws(Airdata$Airline.Name)
Airdata$Airline.Name <- as.factor(Airdata$Airline.Name)

################################RUN ABOVE CODE ONLY WHEN NEEDED###########################




#####################MAP CODE STARTS###########################
us <- map_data("state")
us1<- map_data("city")

str(us) #lat, long, regoin..)
View(us) 
mapArea <- ggplot(Airdata, aes(map_id=Airdata$Orgin.City )) +geom_map(map=us, aes(fill=Airdata$Partner.Name))
+geom_map(map=us, aes(fill=mergedf$area))

mapArea
ggplot(Airdata, aes(map_id=Orgin.City )) 
counts <- table(Airdata$Partner.Name)

barplot(counts)

######################MAP CODE ENDS###########################



######################CORRELATION START########################################
# Draw correlation matrix and visualize it
Airdata_new1 <- Airdata
Airdata_new1$Gender <- 0
Airdata_new1$Gender[Airdata$Gender == "Male"] <- 1
Airdata_new1$Airline.Status <- as.character(Airdata_new1$Airline.Status)
Airdata_new1$Airline.Status <- 0
Airdata_new1$Airline.Status[Airdata$Airline.Status == "Silver"] <- 1
Airdata_new1$Airline.Status[Airdata$Airline.Status == "Gold"] <- 2
Airdata_new1$Airline.Status[Airdata$Airline.Status == "Platinum"] <- 3
Airdata_new1$Class <- as.character(Airdata_new1$Class)
Airdata_new1$Class <- 0
Airdata_new1$Class[Airdata$Class == "Eco Plus"] <- 1
Airdata_new1$Class[Airdata$Class == "business"] <- 2
Airdata_new1 <- Airdata_new1[,c(-8,-13,-14,-15,-16,-17)]
View(Airdata_new1)
cor_Sati <- cor(Airdata_new1)
cor_Sati
#visualize it
install.packages("corrplot")
library(corrplot)
corrplot(cor_Sati, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.5)



# Seems not obivious at all
# Normalize it before cor
newSati_2 <- newSati[,c(-8,-13,-14,-15,-16,-17)]
newSati_2$Gender <- newSati_1$Gender
newSati_2$Airline.Status <- newSati_1$Airline.Status
newSati_2$Class <- newSati_1$Class
for( i in 1:15){
  newSati_2[,i] <- as.numeric(newSati_2[,i])
}
new.cor_Sati <- cor(newSati_2)
corrplot(new.cor_Sati, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 1)


hist(Airdata$Satisfaction)
install.packages("psych")
library(psych)
pairs.panels(Airdata_new1)
######################CORRELATION ENDS########################################

Airdata_lm<- Airdata


df<- data.frame(sort(table(Airdata_lm$Partner.Name)))
df1 <- Airdata_lm[Airdata_lm$Partner.Name == "Cheapseats Airlines Inc.",]
table(df1$Satisfaction)
library(ggplot2)
ggplot(data = df1, aes(x = Age, y = Satisfaction)) +
  geom_point()
table(df1$Age, df1$Satisfaction)

# Linear Modeling on Cust Sat - 1,2,3


model_lm0 <- lm(formula = Satisfaction ~ Age + Price.Sensitivity +
                  Flights.Per.Year + Loyalty + Total.Freq.Flyer.Accts + 
                  Shopping.Amount.at.Airport + Eating.and.Drinking.at.Airport 
                + Scheduled.Departure.Hour + Departure.Delay.in.Minutes +
                  Flight.time.in.minutes+ Arrival.Delay.in.Minutes+ Airline.Status+Price.Sensitivity+  Type.of.Travel+Class +Flight.date+Orgin.City + Destination.City+  Long.Duration.Trip +Gender,
                data = df1)
summary(model_lm0) #Adjusted R-squared:  0.4292 

####Significant variables are:::  Age,Total.Freq.Flyer.Accts, Airline.Status,  Type.of.Travel ,Class 

model_lm1 <- lm(formula = Satisfaction ~ Age + Price.Sensitivity +
                  Flights.Per.Year + Flight.time.in.minutes  + Airline.Status + 
                  Type.of.Travel+Class+Flight.date,
                data = df1)
summary(model_lm1) #Adjusted R-squared:  .4201 
# attributes with 3 stars of significance::: Age,Price.Sensitivity,Flights.Per.Year, Loyalty, Total.Freq.Flyer.Accts, Shopping.Amount.at.Airport


model_lm2a <- lm(formula = Satisfaction ~ Age,
                 data = df1)
summary(model_lm2a) #Adjusted R-squared:  0.02819 


model_lm2b<- lm(formula = Satisfaction ~ Price.Sensitivity,
                 data = df1)
summary(model_lm2b) #	Adjusted R-squared:  9.733e-05


model_lm2c <- lm(formula = Satisfaction ~ Flights.Per.Year,
                 data = df1)
summary(model_lm2c) #Adjusted R-squared:  0.009052 


model_lm2d <- lm(formula = Satisfaction ~ Flight.time.in.minutes,
                 data = df1)
summary(model_lm2d) #Adjusted R-squared:  0.003476 


model_lm2e <- lm(formula = Satisfaction ~ Airline.Status,
                 data = df1)
summary(model_lm2e) #Adjusted R-squared:  0.001806 


model_lm2f <- lm(formula = Satisfaction ~ Type.of.Travel,
                 data = df1)
summary(model_lm2f) #Adjusted R-squared:  0.0001945 

   

model_lm2g <- lm(formula = Satisfaction ~ Flight.date,
                 data = df1)
summary(model_lm2g) #Adjusted R-squared:  0.08568


model_lm2h <- lm(formula = Satisfaction ~ Class,
                 data = df1)
summary(model_lm2h) #Adjusted R-squared:  0.0008464 



########################SUMMARY FROM LM: Type.of.Travel, Airline.Status, Age


######################Association rules start########################################


createBucket1 <- function(vec){
  
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3] <- "High"
  vBuckets[vec < 3] <- "Low"
  return(vBuckets)
}

happyCust <- createBucket1(df1$Satisfaction) 
View(happyCust)

price_sen <- createBucket1(df1$Price.Sensitivity)
View(price_sen)

freq_flyer <- createBucket1(df1$Total.Freq.Flyer.Accts)
View(freq_flyer )

dep_hour <- createBucket1(df1$Scheduled.Departure.Hour)
View(dep_hour )


createBucket2<- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 60] <- "High"
  vBuckets[vec < 20] <- "Low"
  return(vBuckets)
}

flight_perYear <- createBucket2(df1$Flights.Per.Year)
View(flight_perYear )

age <- createBucket2(df1$Age)
View(age)

shopping<- createBucket2(df1$Shopping.Amount.at.Airport)
View(shopping)


eat_drink <- createBucket1(df1$Eating.and.Drinking.at.Airport)
View(eat_drink )

dep_delay <- createBucket1(df1$Departure.Delay.in.Minutes)
View(dep_delay )


flight_time <- createBucket1(df1$Flight.time.in.minutes)
View(flight_time )


arrival_delay <- createBucket1(df1$Arrival.Delay.in.Minutes)
View(arrival_delay )

createBucket3 <- function(vec){
  
  q <- quantile(vec, c(0.4, 0.6)) # consideriong quantiles of 40% and 60%
  vBuckets <- replicate(length(vec), "Average") #values greater than 40% and less than 60% quantile are average 
  vBuckets[vec <= q[1]] <- "Low" # values with less than or equal to 40%quantile are marked low
  vBuckets[vec > q[2]] <- "High" #values greater than 60%quantile are marked high
  return(vBuckets)
} 

loyalty <- createBucket3(df1$Loyalty)
View(loyalty )




ruleDF <- data.frame(happyCust, loyalty, age, flight_perYear, shopping, eat_drink, price_sen, freq_flyer, dep_hour, arrival_delay, flight_time,dep_delay )
View(ruleDF)
df2Arule1 <- as(ruleDF, "transactions")
itemFrequency(df2Arule1 ) 
itemFrequencyPlot(df2Arule1) 
View(df2Arule1)
ruleSet1 <- apriori(df2Arule1, 
                    parameter = list(support =0.1, confidence=.29),
                    appearance = list(default="lhs", rhs=("happyCust=Low"))
)
ruleSet1 #2rules
inspect(ruleSet1)

ruleSet1_sorted <-ruleSet1[order(-quality(ruleSet1)$lift),]
inspect(head(ruleSet1_sorted,15))


######################Association rules  ENDS########################################

######################SVM  Starts########################################
#Using the SVM model to predict the data
install.packages("kernlab")
library(kernlab)
View(df1)
str(df1)
trainindex <- sample(c(1,2,3), nrow(df1),replace= T,prob = c(0.15,0.45,0.4))
traindata <- df1[trainindex==1,] #17659 obs
testdata <- df1[trainindex==2,] # 5801 obs
svmOutput <- ksvm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity +
                    Flights.Per.Year + Type.of.Travel+Loyalty+ Total.Freq.Flyer.Accts + 
                    Shopping.Amount.at.Airport+  Eating.and.Drinking.at.Airport+Class+ 
                    + Flight.date + Partner.Code +Partner.Name+ Orgin.City +Destination.City
                  +Scheduled.Departure.Hour+ Departure.Delay.in.Minutes
                  + Arrival.Delay.in.Minutes+Flight.time.in.minutes+ Long.Duration.Trip    ,
                  data=traindata,kernel="rbfdot", kpar="automatic",C=40,cross=4, prob.model=TRUE)


svmOutput
#OBSERVATION::
#Number of Support Vectors : 4987 
#Training error : 0.084237 ~ 8%
#Cross validation error : 0.689985 ~ 68%

svmresult <- predict(svmOutput,testdata,type="votes")
View(svmresult)
str(svmresult) #Observation -- num [1:17659, 1] 
head(svmresult)


happyPred <- svmresult[,1]
View(happyPred)
happyPred[happyPred>.8] <- 1
happyPred[happyPred<.8] <- 0
View(testdata)
str(testdata)
ctable <- data.frame(testdata$Satisfaction, happyPred)
table(ctable)

# Observation
#                        happyPred
#testdata.Satisfaction    0    1
#                    1    0  447
#                    2   12 3440
#                    3    3 4984
#                    4    0 7122
#                    5    0 1651


#11. Calculate an error rate based on what you see in the confusion matrix. See pages 243-244 for more information.
errorrate <- ((table(ctable)[1,2] + table(ctable)[2,1])/(table(ctable)[1,1]+ table(ctable)[1,2] + table(ctable)[2,1] +table(ctable)[2,2]))*100
errorrate #We find that error rate is around 11.72% which is  good  since it means more than only 10% of our predictions are wrong

# SVM model for variables in lm and arules::
svmOutput2<- ksvm(Satisfaction ~  Age  + Price.Sensitivity +
                    Flights.Per.Year + Type.of.Travel+Loyalty+ Total.Freq.Flyer.Accts + 
                    Eating.and.Drinking.at.Airport+Class+ 
                    + Flight.date+Scheduled.Departure.Hour
                  + Arrival.Delay.in.Minutes   ,
                  data=traindata,kernel="rbfdot", kpar="automatic",C=40,cross=4, prob.model=TRUE)


svmOutput2
#Number of Support Vectors : 5059 
#Training error : 0.161649 
#Cross validation error : 0.790994 

svmresult2 <- predict(svmOutput2,testdata,type="votes")
View(svmresult2)
str(svmresult2) #Observation -- num [1:17659, 1] 
head(svmresult2)


happyPred2 <- svmresult2[,1]
View(happyPred2)
happyPred2[happyPred2>.8] <- 1
happyPred2[happyPred2<.8] <- 0
View(testdata)
str(testdata)
ctable2 <- data.frame(testdata$Satisfaction, happyPred2)
table(ctable2)

#                       happyPred2
#testdata.Satisfaction    0    1
#                    1    0  447
#                    2    9 3443
#                    3    9 4978
#                    4    0 7122
#                    5    0 1651

errorrate2 <- ((table(ctable2)[1,2] + table(ctable2)[2,1])/(table(ctable2)[1,1]+ table(ctable2)[1,2] + table(ctable2)[2,1] +table(ctable2)[2,2]))*100
errorrate2 #We find that error rate is around 11.69% which is  good  since it means more than only 10% of our predictions are wrong


######################SVM ends#######################################
