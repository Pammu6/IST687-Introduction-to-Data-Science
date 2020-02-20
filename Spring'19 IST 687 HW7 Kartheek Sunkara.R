# IST687
# # Student name: Kartheek Sunkara
# Homework number: 7
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

#Step A: Load and Merge datasets

#Read in the census and the USArrests datasets and merge them. 

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

#Rename the columns with the following names: stateName, population, popOver18, percentOver18
colnames(dfStates) <- c("stateName", "population", "popOver18", "percentOver18")
#colnames(dataset)[i]<-name (eg : colnames(dataset)[3]<-"Ethnic") for changing the name of ith column in a table
View(dfStates)

#Copy the USArrests dataset into a local variable (similar to HW 2)
#data()
arrests <- USArrests # copying dataset into a variable
View (arrests)

str(arrests) # gives the structure of arrests
summary(arrests) # gives the summary of arrests i.e min, max, mean, 1st and 3rd quantile

#Create a merged dataframe -- with the attributes from both dataset
#find the names of the columns to pick one to merge on

arrests$stateName <- rownames(arrests)
View(arrests)

#merge the dataframes by statename
mergeDF<-merge(dfStates, arrests, by = "stateName")
View(mergeDF)
str(mergeDF)
summary(mergeDF)

#2)	 Create a new Data frame that has the area of each state (state.area), and the center of each state (state.center), and then merge (by stateName) it with your final data frame in step #1

stateName <- state.name
area <- state.area
center <- state.center
otherDF <- data.frame(stateName,area,center)
View(otherDF)

#Merging otherDF with mergeDF by stateName from step 1

mergeDF <- merge(mergeDF,otherDF,by='stateName')
mergeDF$popPerAres <- mergeDF$population/mergeDF$area
View(mergeDF)
str(mergeDF)
summary(mergeDF)

#install.packages("ggplot2")
library(ggplot2)
#nstall.packages("ggmap")
library(ggmap)
#install.packages("maps")
library(maps)
#install.packages("mapdata")
library(mapdata)
#install.packages("gdata")
library("gdata")
#install.packages('mapproj')
library("mapproj")
mergeDF$stateName <- tolower(mergeDF$stateName)
View(mergeDF)

us <- ggplot2::map_data("state")
us <- map_data("state")
View(us)

#Step B: Generate a color coded map
#)	Create a color coded map, based on the area of the state 

mapArea	<- ggplot()		

#the new dataset to create map
mapArea <- ggplot(mergeDF,aes(map_id=stateName))
mapArea

mapArea <- mapArea + geom_map(map=us,aes(fill=mergeDF$area))
mapArea

mapArea <- mapArea + expand_limits(x=us$long,y=us$lat)
mapArea

mapArea <- mapArea + coord_map() + ggtitle("Area of the U.S")

#Final plot of the area map
mapArea

#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
#4)	Repeat step B, but color code the map based on the murder rate of each state.

mapMurder <- ggplot(mergeDF,aes(map_id=stateName))
#mapMurder

mapMurder <- mapMurder + geom_map(map=us,aes(fill=mergeDF$Murder))
#mapArea

mapMurder <- mapMurder + expand_limits(x=us$long,y=us$lat)
#mapMurder

mapMurder <- mapMurder + coord_map() + ggtitle("Map of US based on Murder rate")
mapMurder

#5)	 Show the population as a circle per state (the larger the population, the larger the circle), using the location defined by the center of each state

mapMurder1 <- mapMurder + geom_point(data=mergeDF,aes(x=mergeDF$x,y=mergeDF$y,size=mergeDF$population))
mapMurder1 <- mapMurder1 + coord_map() + ggtitle("Population and Murder rate per state")
mapMurder1


## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 

#  https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
#install.packages("jsonlite")
library(jsonlite)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("dplyr")
library(dpylr)

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

#dplyr will be used to stack lists together into a data.frame and to get the pipe operator '%>%'
#install.packages("dplyr")
suppressPackageStartupMessages(library(dplyr))

#input addresses
addresses <- c("syracuse University, syracuse")

#
d <- suppressWarnings(lapply(addresses, function(address) {
  #set the elapsed time counter to 0
  t <- Sys.time()
  #calling the nominatim OSM API
  api_output <- nominatim_osm(address)
  #get the elapsed time
  t <- difftime(Sys.time(), t, 'secs')
  #return data.frame with the input address, output of the nominatim_osm function and elapsed time
  return(data.frame(address = address, api_output, elapsed_time = t))
}) %>%
  #stack the list output into data.frame
  bind_rows() %>% data.frame())
#output the data.frame content into console
d
d[2]
d[3]

NewLatLon<-function(addresses){
  d <- suppressWarnings(lapply(addresses, function(address) {
    #set the elapsed time counter to 0
    t <- Sys.time()
    #calling the nominatim OSM API
    api_output <- nominatim_osm(address)
    #get the elapsed time
    t <- difftime(Sys.time(), t, 'secs')
    #return data.frame with the input address, output of the nominatim_osm function and elapsed time
    return(data.frame(address = address, api_output, elapsed_time = t))
  }) %>%
    #stack the list output into data.frame
    bind_rows() %>% data.frame())
  #output the data.frame content into console
  return(d)
}

NewLatLon(addresses)
latlon<-NewLatLon(addresses)
latlon$lon
latlon$lat  

# New create the first zoomed map based on "mapZip", and plot a point representing NYC
mapZoomed <-  mapMurder1 + geom_point(x = latlon$lon, 
                                      y = latlon$lat, color="darkgreen", size = 3)

mapZoomed

# zoom into the region arount NYC with 10 degrees lat and long fluctuation (+/- 10)
mapZipZoomed <-  mapZoomed + xlim(latlon$lon-10, latlon$lon+10) + 
  ylim(latlon$lat-10,latlon$lat+10) + coord_map()

# plot the map
mapZipZoomed
