
# IST 687
# Due Date: October 18, 2018
# Homework 2 - Submitted by Chris Karasinski on October 18, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.



#Step A
#Create a function called MyData that pulls census data from a URL and and cleans the rows and columns
MyData <- function()
{
  #Create a variable for the URL link
  URL <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  #Use read.csv to read in the CSV file from the URL above
  myData <- read.csv(url(URL))
  #Take the data imported from the CSV file and convert it into a dataframe
  myFrame <- data.frame(myData, stringsAsFactors = FALSE)
  #Delete rows 1 through 8
  myFrame <- myFrame[-(1:8),]
  #Only keep the first 5 columns
  myFrame <- myFrame[,1:5]
  #Rename the rownames as NULL so that they are correctly numbered starting from 1
  rownames(myFrame) <- NULL
  #Delete rows 52 through 58
  myFrame <- myFrame[-52:-58,]
  #Create a variable cnames that represents the column names of myFrame
  cnames <- colnames(myFrame)
  #Set the first column name to StateName
  cnames[1] <- "StateName"
  #Set the second variable name to april10census
  cnames[2]<- "april10census"
  #Set the third variable name to april10base
  cnames[3] <- "april10base"
  #Set the fourth variable name to july10pop
  cnames[4] <- "july10pop"
  #Set the fifth column name to july11pop
  cnames[5] <- "july11pop"
  #Reset the column names using the cnames variable
  colnames(myFrame) <- cnames
  
  #Remove the dots at the beginning of every state within the StateName column
  myFrame$StateName <- gsub("\\.","", myFrame$StateName)
  #Remove the commas within every number for the april10census column
  myFrame$april10census <- gsub(",","",myFrame$april10census)
  #Remove the commas within every number for the april10base column
  myFrame$april10base <- gsub(",","", myFrame$april10base)
  #Remove the commas within every number for the july10pop column
  myFrame$july10pop <- gsub(",","", myFrame$july10pop)
  #Remove the commas within ever number for the july11pop column
  myFrame$july11pop <- gsub(",","", myFrame$july11pop)
  
  #Remove the spaces and covert every observation to a number in the april10census column
  myFrame$april10census <- as.numeric(gsub(" ","",myFrame$april10census))
  #Remove the spaces and covert every observation to a number in the april10base column
  myFrame$april10base <- as.numeric(gsub(" ","", myFrame$april10base))
  #Remove the spaces and covert every observation to a number in the july10pop column
  myFrame$july10pop <- as.numeric(gsub(" ","", myFrame$july10pop))
  #Remove the spaces and covert every observation to a number in the july11pop column
  myFrame$july11pop <- as.numeric(gsub(" ","", myFrame$july11pop))
  
  
  #Return the clean dataframe back into the MyData variable
  return(myFrame)
  
}
#Run the myData function and store it in dfStates
dfStates <- MyData()

#Create a new variable called arrests that contains the USArrests data
arrests <- USArrests

#Create a new variable called StateName that is the same as the current row names of the arrests database. We need to do this in order to merge
arrests$StateName <- rownames(arrests)
#Reset the rownames of arrests as NULL so they are numbered from 1-50
rownames(arrests) <- NULL
#Drop the District of Columbia because it is not a state and not included in the arrests data
dfStates <- dfStates[-9,]
#Reset the row names so they are numbered correctly
rownames(dfStates)<- NULL

#Merge the dfStates dataset and arrests dataset based off the column StateName
dfStateInfo <- merge(dfStates,arrests, by= "StateName")

#Load in the ggplot2 package
library(ggplot2)
#Load in the ggpmap package
library(ggmap)
#read in the map data for the US states from the ggmap package
us <- map_data("state")
#Create a dataframe that contains the state, the longitude and latitude of the state's centers, and the area of each state
ggStateDF <- data.frame(state.name,state.center,state.area, stringsAsFactors = FALSE)
#Rename the state.name column for merging purposes
colnames(ggStateDF)[1] <- "StateName"

#Merge the newly created dataframe with the dfStateInfo dataframe based off StateName
dfStateInfo <- merge(dfStateInfo,ggStateDF, by= "StateName")
#Rename the latitude center coordinates of each state
colnames(dfStateInfo)[11] <- "LatCenter"
#Rename the longitude center coordinates of each state
colnames(dfStateInfo)[10]<- "LongCenter"
#Rename the area column for each state
colnames(dfStateInfo)[12]<- "Area"
#Make all of the state names lower case for ggplot purposes
dfStateInfo$StateName <- tolower(dfStateInfo$StateName)



#Step B
#Create a variable called map.area that reads in the dfStateInfo data and makes a map ID based off State name
map.area <- ggplot(dfStateInfo, aes(map_id = StateName))  
#Use the geom_map feature to base the map off the US and color fill it based off the area of each state
map.area <- map.area + geom_map(map = us, aes(fill=Area)) 
#Set the limits of the map to the latitude and longitude that the United States stretches across
map.area <- map.area + expand_limits(x = us$long, y = us$lat)
#Create a title for the map and make sure the map does not become distorted or stretched
map.area <- map.area+ coord_map() + ggtitle("State Area")
#Save map of US with states filled in based off area into a varible
mapArea <- map.area



#Step C
#Create a variable called map.murderRate that reads in the dfStateInfo data and makes a map ID based off state murder rate
map.murderRate <- ggplot(dfStateInfo, aes(map_id = StateName))
#Use the geom_map feature to base the map off the US and color fill it based off the murder rate of each state
map.murderRate <- map.murderRate + geom_map(map = us, aes(fill=Murder)) 
#Set the limits of the map to the latitude and longitude that the United States stretches across
map.murderRate <- map.murderRate + expand_limits(x = us$long, y = us$lat)
#Create a title for the map and make sure the map does not become distorted or stretched
map.murderRate <- map.murderRate+ coord_map() + ggtitle("State Murder Rate")
#Save the map of US with states filled in based off murder rate into a variable
mapMurderRate<- map.murderRate

#Create a map of the US that has states color filled based off murder rate, and displays a circle in the center of each state.
#The size of the circle represents the population of each state. The bigger the circle, the larger the population.
murderPopulation <- map.murderRate + geom_point(data=dfStateInfo,aes(x = dfStateInfo$LongCenter, y = dfStateInfo$LatCenter, size = july11pop), shape=1)



#Step D
#Create a variable for the latitude of NYC
nycLat <- 40.7128
#Create a variable for the longitude of NYC
nycLong <- -74.0060

#Create a map of the US that shows the states color filled based off murder rate and displays a circle in the center based off the poulation size of each state.
#We want to display only the northeast, so set an x limit for the longitude and a y limit for the latitude.
#These limits are based off the coordinates of NYC +/- 10.
murderPopulationNorth <- map.murderRate + ggtitle("State Murder Rate NorthEast") + geom_point(data=dfStateInfo,aes(x = dfStateInfo$LongCenter, y = dfStateInfo$LatCenter, size = july11pop), shape=1)  + xlim(nycLong-10, nycLong+10) + ylim(nycLat-10, nycLat + 10)

