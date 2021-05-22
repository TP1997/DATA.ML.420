# Mapping point data
# House price sales data
setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
houses = read.csv("Camdenhouse15.csv")
houses = houses[,c(1,2,8,9)]  # Keep the data simple 
plot(houses$oseast1m, houses$osnrth1m)

library("sp")

# create a House.Points spatial points data frame
House.Points = SpatialPointsDataFrame(houses[,3:4], houses, proj4string = CRS("+init=EPSG:27700"))

# This plots a blank base map, we have set the transparency of the borders to 0.4
tm_shape(OA.Census) + tm_borders(alpha=.4)

# We can now add on the points as an additional tm_shape layer in our map.
# To do this, we copy in the same code to make the base map, add on a plus symbol, 
# then enter the details for the points data. 
# The additional arguments for the points data can be summarised as:
#   tm_shape(polygon file) +
#   tm_borders(transparency = 40%) +
#   tm_shape(spatial points data frame) +
#   tm_dots(what variable is coloured, the colour palette and interval style)
# which is entered into R like this:

# creates a coloured dot map
tm_shape(OA.Census) + 
  tm_borders(alpha=.4) +
  tm_shape(House.Points) +
  tm_dots(col = "Price", palette = "Reds", style = "quantile")

# Creating proportional symbol maps
  # We can also create proportional symbols in tmap by replacing the tm_dots() function 
  # with the tm_bubbles() function. In the example below, the size and colours are 
  # both set as the price column.
tm_shape(OA.Census) +
  tm_borders(alpha=.4) +
  tm_shape(House.Points) +
  tm_bubbles(size = "Price", col = "Price", palette = "Blues", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)") +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)

# Combine the polygon map and the points 
  # We can also make the polygon shapefile display one of our census variables
  # as a choropleth map as shown below. Some more parameters can be added within the tm_bubbles()
  # function to create thin borders around the bubbles.
tm_shape(OA.Census) +
  tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% Qualification") +
  tm_borders(alpha=.4) +tm_shape(House.Points) +
  tm_bubbles(size = "Price", col = "Price", palette = "Blues", style = "quantile", 
             legend.size.show = FALSE, title.col = "Price Paid (£)", border.col = "black", 
             border.lwd = 0.1, border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)