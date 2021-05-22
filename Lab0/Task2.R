setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
data = read.csv("Censusdata.csv")
summary(data)
attach(data)

# i)
library("rgdal")

Output.Areas = readOGR(".", "Camden_oa11") 
OA.Census = merge(Output.Areas, data, by.x="OA11CD", by.y="OA")

library(tmap)

tm_shape(OA.Census) + 
  tm_fill("Unemployed", style='quantile', n=7, palette='Purples')

# ii)
houses = read.csv("Camdenhouse15.csv")
houses = houses[,c(1,2,8,9)]

House.Points = SpatialPointsDataFrame(houses[,3:4], houses, proj4string = CRS("+init=EPSG:27700"))
tm_shape(OA.Census) + 
  tm_fill("Unemployed", style='quantile', n=7, palette='Purples')+
  tm_borders(alpha=.4) +
  tm_shape(House.Points) +
  tm_bubbles(size = "Price", col = "Price", palette = "Blues", style = "quantile", 
             legend.size.show = FALSE, title.col = "Price Paid (£)", border.col = "black", 
             border.lwd = 0.1, border.alpha = 0.1)
  
