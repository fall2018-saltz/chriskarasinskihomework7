

#Create a variable called map.area that reads in the dfStateInfo data and makes a map ID based off State name
map.area <- ggplot(dfStateInfo, aes(map_id = StateName))  
#Use the geom_map feature to base the map off the US and color fill it based off the area of each state
map.area <- map.area + geom_map(map = us, aes(fill=Area)) 
#Set the limits of the map to the latitude and longitude that the United States stretches across
map.area <- map.area + expand_limits(x = us$long, y = us$lat)
#Create a title for the map and make sure the map does not become distorted or stretched
map.area <- map.area+ coord_map() + ggtitle("State Area")
#Display map of US with states filled in based off area
map.area
