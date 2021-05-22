setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
soil.data = read.table('soilph.txt', head=F)

soil.data = data.frame('r'=soil.data[,1], 'c'=soil.data[,2], 'ph'=soil.data[,3])
# 11 x 11
# Column 1 is the row number of the grid
# Column 2 is the column number of the grid
# Column 3 is the soil pH

# i)
par(mfrow=c(1,1))
summary(soil.data)
stem(soil.data$ph)
boxplot(soil.data$ph)

plot(soil.data$c, soil.data$r, type='n', lab=c(11,11,7))
text(soil.data$c, soil.data$r, round(soil.data$ph,1), col=3)
title('Soil pH-levels', col=6, cex=1.75)

ph.mat = tapply(soil.data$ph, list(factor(soil.data$c),factor(soil.data$r)),function(x)x)
image(x=1:11, y=1:11, ph.mat, cex.axis=1.0, xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title('Soil pH-levels', col=6, cex=1.75)

# ii)
# Fit a quadratic surface to model the trend of data first. Obtain the residuals from the 
# above fit and make lag-1 h-scatterplots of residuals. Describe your findings in words.
par(mfrow=c(2,2))
model.ii = lm(soil.data$ph~soil.data$r*soil.data$c+I(soil.data$r^2)+I(soil.data$c^2), data=soil.data)
names(model.ii)
res.mat = tapply(model.ii$residuals, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
# h=(1,0), columns 1-10 vs 2-11 
plot(res.mat[,-11], res.mat[,-1], xlab="pH-residual at col z",  ylab="pH-residual at col z+1")
abline(c(0,1))
title('h=(1,0), columns 1-10 vs 2-11')
# h=(0,1), rows 1-10 vs 2-11
plot(res.mat[-11,], res.mat[-1,], xlab="pH-residual at row z",  ylab='pH-residual at row z+1')
abline(c(0,1))
title('h=(0,1), rows 1-10 vs 2-11')

#############################################
model.i = lm(soil.data$ph~soil.data$r+soil.data$c, data=soil.data)
names(model.i)
res.mat = tapply(model.i$residuals, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
# h=(1,0), columns 1-10 vs 2-11 
plot(res.mat[,-11], res.mat[,-1], xlab="pH-res at col z",  ylab="pH-res at col z+1")
abline(c(0,1))
title('h=(1,0)??, columns 1-10 vs 2-11')
# h=(0,1), rows 1-10 vs 2-11
plot(res.mat[-11,], res.mat[-1,], xlab="pH-res at row z",  ylab='pH-res at row z+1')
abline(c(0,1))
title('h=(0,1)??, rows 1-10 vs 2-11')
#######################################

# iii)
# Fit a loess surface to model the trend of data. Obtain the residuals and make lag-1 
# h-scatterplots. Compare the results with those from ii)
model.iii = loess(soil.data$ph~soil.data$r*soil.data$c+I(soil.data$r^2)+I(soil.data$c^2), 
                  data=soil.data, normalize=F, span=1.0)
names(model.iii)
# Residual analysis
resloess.mat = tapply(model.iii$residuals, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
# h=(1,0), columns 1-10 vs 2-11 
plot(resloess.mat[,-11], resloess.mat[,-1], xlab="pH-residual at col z",  ylab="pH-residual at col z+1")
abline(c(0,1))
title('Loess (span=1) \nh=(1,0), columns 1-10 vs 2-11')
# h=(0,1), rows 1-10 vs 2-11
plot(resloess.mat[-11,], resloess.mat[-1,], xlab="pH-residual at row z",  ylab='pH-residual at row z+1')
abline(c(0,1))
title('Loess (span=1) \nh=(0,1), rows 1-10 vs 2-11')

##################################################################################3
par(mfrow=c(3,2))

# Original
ph.mat = tapply(soil.data$ph, list(factor(soil.data$c),factor(soil.data$r)),function(x)x)
# lag1-h - Original
plot(ph.mat[,-11], ph.mat[,-1], xlab="pH at col z",  ylab="pH at col z+1",
     xlim=c(4.0,5.0),ylim=c(4.0,5.0))
abline(c(0,1))
title('Original \nh=(1,0), columns 1-10 vs 2-11')
# h=(0,1), rows 1-10 vs 2-11
plot(ph.mat[-11,], ph.mat[-1,], xlab="pH at row z",  ylab='pH at row z+1',
     xlim=c(4.0,5.0),ylim=c(4.0,5.0))
abline(c(0,1))
title('Original (span=1) \nh=(0,1), rows 1-10 vs 2-11')

# Fitted values - quadratic
fitquad.mat = tapply(model.ii$fitted, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
# lag1-h - quadratic
plot(fitquad.mat[,-11], fitquad.mat[,-1], xlab="pH at col z",  ylab="pH at col z+1",
     xlim=c(4.0,5.0),ylim=c(4.0,5.0))
abline(c(0,1))
title('Quadratic \nh=(1,0), columns 1-10 vs 2-11')
# h=(0,1), rows 1-10 vs 2-11
plot(fitquad.mat[-11,], fitquad.mat[-1,], xlab="pH at row z",  ylab='pH at row z+1',
     xlim=c(4.0,5.0),ylim=c(4.0,5.0))
abline(c(0,1))
title('Quadratic (span=1) \nh=(0,1), rows 1-10 vs 2-11')

# Fitted values - Loess
fitloess.mat = tapply(model.iii$fitted, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
# lag1-h - quadratic
plot(fitloess.mat[,-11], fitloess.mat[,-1], xlab="pH at col z",  ylab="pH at col z+1",
     xlim=c(4.0,5.0),ylim=c(4.0,5.0))
abline(c(0,1))
title('Loess \nh=(1,0), columns 1-10 vs 2-11')
# h=(0,1), rows 1-10 vs 2-11
plot(fitloess.mat[-11,], fitloess.mat[-1,], xlab="pH at row z",  ylab='pH at row z+1',
     xlim=c(4.0,5.0),ylim=c(4.0,5.0))
abline(c(0,1))
title('Loess (span=1) \nh=(0,1), rows 1-10 vs 2-11')
#########################################################################3

# iv) 
# Compare the fitted surface from the quadratic fit and the loess fit. 
# Make the grayscale plots of each fit.
quadfit.mat = tapply(model.ii$fitted, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
loessfit.mat = tapply(model.iii$fitted, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
linfit.mat = tapply(model.i$fitted, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)

par(mfrow=c(2,2))
persp(quadfit.mat)
title('Quadratic fit')
persp(loessfit.mat)
title('Loess fit')
  
# Grayscale plots
image(x=1:11, y=1:11, quadfit.mat, cex.axis=1.0, xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title('Quadratic fit', col=6, cex=1.75)
image(x=1:11, y=1:11, loessfit.mat, cex.axis=1.0, xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title('Loess fit', col=6, cex=1.75)

#####################################3
par(mfrow=c(1,2))
persp(linfit.mat)
title('Lin fit')
image(x=1:11, y=1:11, linfit.mat, cex.axis=1.0, xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title('Lin fit', col=6, cex=1.75)
######################################

# v)
# Make the grayscale plots of the median polish surface and residuals. 
# Briefly comment comparing with the results in iv).
library(gstat)
ph.med = medpolish(ph.mat, na.rm=T)
ph.trend = ph.mat - ph.med$residuals
zmin = min(ph.mat, ph.trend)
zmax = max(ph.mat, ph.trend)
# Grayscale map
par(mfrow=c(1,2))
image(x=1:11, y=1:11, ph.trend, zlim=c(zmin,zmax), cex.axis=1.0,
      xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title("Median Polish Surface",cex.main=1.5)

image(x=1:11, y=1:11, ph.med$residuals, zlim=range(ph.med$residuals, na.rm=T), cex.axis=1.0,
      xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title("Median Polish Residuals",cex.main=1.5)

image(x=1:11, y=1:11, ph.mat, zlim=c(zmin,zmax), cex.axis=1.0,
      xlab="Columns", ylab="Rows", cex.lab=1.2, lab=c(11,11,7))
title("Original pH %’s", cex.main=1.5)
