setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
data = read.table("RainIowa.txt")
summary(data)
attach(data)

colors = c('yellow','orange','red','green','black')
levels = cut(mm, c(700,770,789.25,908.50,975.75,1043))
as.numeric(levels)

library(maps)
plot(Longitude, Latitude, xlim=c(-97,-90), ylim=c(40, 44), 
     xlab="Longitude",  ylab="Latitude", main="Rainfall measures in Iowa", "n")
map("county", "iowa",add=TRUE)
points(Longitude, Latitude, col=colors[as.numeric(levels)], pch=19)
