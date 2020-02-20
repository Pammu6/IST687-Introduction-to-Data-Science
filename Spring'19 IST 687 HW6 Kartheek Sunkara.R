# IST687
# # Student name: Kartheek Sunkara
# Homework number: 6
# Date submitted: 21 Feb 2019 (3:03AM)
# Date due: 21 Feb 2019 (9:00AM) 
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself and some websites: tutorialspoint, r bloggers, rdocumentation, sthda, stackoverflow and rfunction
rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

# Set working directory 
# Change to the folder containing your homework data files
#setwd("~/MyDesktop/IST687/Homework")

#Step A: Load and Merge datasets
#1)	Read in the census dataset (using the function created in HW 3)
csvFile <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv")

#2)	Copy the USArrests dataset into a local variable (similar to HW 2)
arrests <- USArrests


rnames <- row.names(arrests)
#3)	Create a merged dataframe -- with the attributes from both dataset
mergeDF <- merge(cns,arrests,by=0,all=TRUE)

install.packages('ggplot2')
library(ggplot2)
#histogram for population
myPlotPop <- ggplot(mergeDF,aes(x=population))
myPlotPop <- myPlotpop + geom_histogram(binwidth=500000)
myPlotPop <- myPlotPop + ggtitle("Histogram of population")
myPlotPop

#Histogram for murder rate
myPlotmurder <- ggplot(mergeDF,aes(x=Murder))
myPlotmurder <- myPlotmurder + geom_histogram(binwidth=500000)
myPlotmurder <- myPlotmurder + ggtitle("Histogram of Murder rate")
myPlotmurder

#Histogram for assault 
myPlotaslt <- ggplot(mergeDF,aes(x=Assault))
myPlotaslt <- myPlotaslt + geom_histogram(binwidth=500000)
myPlotaslt <- myPlotaslt + ggtitle("Histogram of Assault")
myPlotaslt

#Histogram for Rape
myPlotRape <- ggplot(mergeDF,aes(x=Rape))
myPlotRape <- myPlotRape + geom_histogram(binwidth=500000)
myPlotRape <- myPlotRape + ggtitle("Histogram of Rape")
myPlotRape


#Boxplot for population
myboxPlotpop <- ggpot(mergeDF,aes(x=factor(0),y=population))+geom_boxplot()
myboxPlotpop

#5) Boxplot for Murder
myboxPlotmurder <- ggpot(mergeDF,aes(x=factor(0),y=Murder))+geom_boxplot()
myboxPlotmurder

#7)	Calculate the number of murders per state
mergeDF$numMurders <- mergeDF$population*mergeDF$Murder/10000 #Adding new column with 
View(mergeDF)

#8)	Generate a bar chart, with the number of murders per state
myboxPlotnms <- ggpot(mergeDF,aes(x=stateName,y=numMurders))+geom_col()
myboxPlotnms

myboxPlotnms <- ggplot(mergeDF,aes(x=reorder(stateName,numMurders),y=numMurders))+geom_col()
myboxPlotnms
myboxPlotnms <- myboxPlotnms + theme(axis.test.x=element_test(angle=90,hjust=1))
myboxPlotnms <- myboxPlotnms + ggtitle("Total Murders")
myboxPlotnms

#11)	 Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
myboxPlotnms <- ggplot(mergeDF,aes(x=reorder(stateName,numMurders),y=numMurders,fill=percentOver18))+geom_col()
myboxPlotnms

#Step D: Explore Murders - scatter chart

#12)	 Generate a scatter plot - have population on the X axis, the percent over 18 on the y axis, and the size & color represent the number of murders.
scatter <- ggpot(mergeDF,aes(x=population,y=percentOver18))+geom_point(aes(s))
scatter_color_gradient(low='white',high='red')
