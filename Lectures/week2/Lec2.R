# Exploratory (Spatial) Data Analysis :
setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
coal.ash = read.table('coalash.txt', head=T)

####################################### Ordinary Exploratory Analysis 

summary(coal.ash$coal)
sqrt(var(coal.ash$coal))
stem(coal.ash$coal)   # stem-and-leaf plot (spatial information ignored!) 
boxplot(coal.ash$coal)

par(mfrow=c(1,1))

plot(coal.ash$x, coal.ash$y, type='n', lab=c(16,23,7))
text(coal.ash$x, coal.ash$y, round(coal.ash$coal,1), col=3)
title('Coal Ash', col=6, cex=1.75)

####################################### Trend analysis

par(mfrow=c(2,2)) 
plot(coal.ash$x, coal.ash$y, axes=F, xlab=" ", ylab=" ") 

# Compare column (row) means and medians 
plot(1:23,tapply(coal.ash$coal, coal.ash$y, median),ylim=c(8,12),pch="o",xlab="Coal Ash",ylab="Rows",col=3)
points(1:23,tapply(coal.ash$coal, coal.ash$y, mean),pch='x', col=4)

plot(1:16,tapply(coal.ash$coal, coal.ash$x, median),ylim=c(7,11),pch="o",xlab="Coal Ash",ylab="Rows",col=2)
points(1:16,tapply(coal.ash$coal, coal.ash$x, mean),pch='x')

plot(coal.ash$x, coal.ash$y, axes=F, xlab=" ", ylab=" ") 
text(1,22, "o= median % coal ash", adj=0)   # plot the legend 
text(1,19, "x= mean % coal ash",  adj=0)

# Convert the vectors of original data to matrices to use some plotting functions 
coal.mat = tapply(coal.ash$coal, list(factor(coal.ash$x),factor(coal.ash$y)),function(x)x)
contour(coal.mat) 
image(coal.mat)

####################################### LOESS fit 
coal.loe = loess(coal.ash$coal~x*y, data=coal.ash, normalize=F, span=.25)

coal.grid = as.data.frame(cbind(x=coal.ash$x, y=coal.ash$y))
loe.pred = predict(coal.loe, coal.grid)
coalloe.mat = tapply(loe.pred, list(factor(coal.grid$x), factor(coal.grid$y)),function(x)x)

# Surface plot of original data
persp(coal.mat)
# Surface plot of smoothed data (predicted by the model)
persp(coalloe.mat)

####################################### Median polish
library(gstat)

coal.mp = medpolish(coal.mat, na.rm=T)
coal.trend = coal.mat - coal.mp$residuals # To extract the trend of large-scale variation
par(mfrow=c(1,3))

# Computes the minimum and maximum of all coal ash precentages and trend values, not including
# missing values (!is.na)
zmin = min(coal.mat[!is.na(coal.mat)], coal.trend[!is.na(coal.trend)])
zmax = max(coal.mat[!is.na(coal.mat)], coal.trend[!is.na(coal.trend)])
# Creates a grayscale map of the coal ash %'s without interpolation
image(x=1:max(coal.ash$x), y=1:max(coal.ash$y), coal.mat, zlim=c(zmin,zmax), cex.axis=1.5,
      xlab="Columns", ylab="Rows", cex.lab=1.6, col=gray.colors(12))
title("Original Coal Ash %’s", cex.main=1.5)

image(x=1:max(coal.ash$x), y=1:max(coal.ash$y), coal.trend, zlim=c(zmin,zmax), cex.axis=1.5,
      xlab="Columns", ylab="Rows", cex.lab=1.6, col=gray.colors(12)) # coal ash trend values w/o interpolation.
title("Median Polish Trend",cex.main=1.5)

image(x=1:max(coal.ash$x), y=1:max(coal.ash$y), coal.mp$resid, zlim=range(coal.mp$resid, na.rm=T), cex.axis=1.5,
      xlab="Columns", ylab="Rows", cex.lab=1.6, col=gray.colors(12)) # coal ash residuals w/o interpolation. 
title("Median Polish Residuals",cex.main=1.5)

####################################### Small-scale variation: lag-1 h-scatterplots 

par(pty="s", mfcol=c(1,2))    # pty="s" : plot in a square frame
grid.mat = tapply(coal.ash$coal, list( factor(coal.ash$x),factor(coal.ash$y)), function(x)x)

# Plot columns 2 through 23 vs. columns 1 through 22:  h=(0,1) 
plot(grid.mat[,-1], grid.mat[,-23], xlab="%coal ash in column Z",  ylab="%coal ash in column Z-1") 
abline( c(0,1))    # adding the line with the intercept=0 & slope= 1

#Identify interesting points on the screen - click the outliers yourself &  right-click (or esc) to stop
identify(grid.mat[,-1], grid.mat[,-23],label= grid.mat[,-1])   # Interactive hands-on function

# h=(1,0) 
plot(grid.mat[-1,], grid.mat[-16,], xlab="%coal ash in row Z",  ylab="%coal ash in row Z-1") 
abline(c(0,1)) 
identify(grid.mat[-1,], grid.mat[-16,],label= grid.mat[-1,])

# Try this! 

library(geoR)
coal.d = as.data.frame(cbind(coal.ash$x,coal.ash$y, coal.ash$coal))
coal.geo = as.geodata(coal.d)   # create an object of the class 'geodata' 
plot(coal.geo)
