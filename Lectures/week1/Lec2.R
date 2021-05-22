setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
Census.Data =read.csv("Censusdata.csv") #  data stored as Comma Separated Values (.csv) file

head(Census.Data) # to view the first few lines of data 

# Census Data:  percentage of the White British population, 
# Occupancy rating (number of bedrooms and central heating), 
# percentage of homes with a low occupation rating , 
# Highest level of qualification (Higher Education or Certificate)

summary(Census.Data)
boxplot(Census.Data[,2:5])
hist(Census.Data$Unemployed)
plot(Census.Data$Unemployed, Census.Data$Qualification, 
     xlab="% in full time employment", ylab="% With a Qualification")# explore a relationship between two variables

library("ggplot2")

p = ggplot(Census.Data, aes(Unemployed,Qualification)) 
p + geom_point(aes(colour = White_British, size = Low_Occupancy))

library("rgdal")     # Bindings for the Geospatial Data Abstraction Library
library("rgeos")    # Interface to Geometry Engine - Open Source

# Load the output area shapefile : need to download the folder for shapefiles first.
Output.Areas = readOGR(".", "Camden_oa11") 
# Explore the shape file by plotting it as a map to view the spatial dimensions of the shapefile mapped out.
plot(Output.Areas) 

# We now need to join Census.Data to the shapefile so the census attributes can be mapped. 
# As the census data contains unique names of each of the output areas, this can be used a key to merge 
# the data to the output area file (which also contains unique names of each output area).
# need to use by.x and by.y so the merge function uses the correct columns to join the data.
OA.Census = merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

# Mapping areal (polygon) data in R

# Install and load packages
library(tmap)
library(leaflet)
qtm(OA.Census, fill = "Qualification")   # quickly create a map with a legend 

# Creating more advanced maps in tmap involves you binding together several 
# functions that comprise different aspects of the graphic. For instance:
# polygon + polygon's symbology + borders + layout

# We enter shapefiles (or R spatial data objects) followed by a command to set their symbologies. 

# The objects are then layered in the visualisation in order. The object entered first 
# appears at the bottom of the graphic.

# Create a simple choropleth map of the 'qualification' variable: load in the shapefile 
# with the tm_shape() function then add in the tm_fill() function which is where we can 
# decide how the polygons are filled in. 
tm_shape(OA.Census) + tm_fill("Qualification")

# 'tmap' allows you to use colour ramps either defined by the user or a set of predefined 
# colour ramps from the ColorBrewer() function. To explore the predefined colour ramps in 
# ColourBrewer enter the following code

library(RColorBrewer)
display.brewer.all()
# Changing the intervals
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", palette = "Reds")
# Change the number of levels
tm_shape(OA.Census) + tm_fill("Qualification", style = "quantile", n = 7, palette = "Greens")
# Editing the layout of the map
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification")+ 
                      tm_borders(alpha=.4) +
                      tm_compass() +
                      tm_layout(title = "Camden, London", 
                                legend.text.size = 1.1, 
                                legend.title.size = 1.4, 
                                legend.position = c("right", "top"), 
                                frame = FALSE)


