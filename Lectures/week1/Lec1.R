setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
o3 = read.table("ozone.txt")

library(maps)

# First, plot only the frame with x,y axes and lables:  
# xlim & ylim are set using the min and max values for the Longitude and Latitude 

plot(o3$lon, o3$lat, xlim=c(-125,-114), ylim=c(32, 43), 
     xlab="Longitude",  ylab="Latitude", main="Ozone locations in California", "n")
map("county", "ca",add=TRUE)   # map of counties in California 
points(o3$lon, o3$lat, cex=o3$o3/0.06)   # plot the points relative to their (ozone) values

# Try to match data location with the Air Quality Index:  
# Green for good quality, moderate, ..., red for 'very unhealthy' 
AQI_colors = c("green", "yellow", "orange", "dark orange", "red") 
AQI_levels = cut(o3$o3, c(0, 0.06, 0.075, 0.104, 0.115, 0.374))
as.numeric(AQI_levels)
plot(o3$lon, o3$lat, xlim=c(-125,-114), ylim=c(32,43), 
     xlab="Longitude", ylab="Latitude", main="Ozone locations in California", "n")
map("county", "ca",add=TRUE)
points(o3$lon, o3$lat, cex=o3$o3/mean(o3$o3), col=AQI_colors[as.numeric(AQI_levels)], pch=19)
