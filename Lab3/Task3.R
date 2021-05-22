setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
soil.data = read.table('soilph.txt', head=F)
soil.data = data.frame('r'=soil.data[,1], 'c'=soil.data[,2], 'ph'=soil.data[,3])
library(geoR)

# i)
# Empirical semivariogram
soil.geodata = as.geodata(cbind(soil.data$r, soil.data$c, soil.data$ph))
soil.variog = variog(soil.geodata, trend='2nd', max.dist=14.14214/2, pairs.min=30)

par(mfrow=c(1,1))
plot(soil.variog)

# Covariance models
model.1 = variofit(soil.variog, cov.model='matern')
model.2 = variofit(soil.variog, cov.model='exponential')
model.3 = variofit(soil.variog, cov.model='gaussian')
model.4 = variofit(soil.variog, cov.model='spherical')

lines(model.1, col='red') 
lines(model.2, col='blue') 
lines(model.3, col='green') 
lines(model.4, col='black') 
# Select matern model

model.quad = lm(soil.data$ph~soil.data$r*soil.data$c+I(soil.data$r^2)+I(soil.data$c^2), data=soil.data)
res.mat = tapply(model.quad$residuals, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
soil.resdata = as.geodata(cbind(soil.data$r, soil.data$c, model.quad$residuals))
# Obtain kriging predictors and variances at...
# e1. 
# Halfway between the first and second columns and halfway 
# between the first and second rows.
new.loc = c(1.5, 1.5)
soil.kre1 = krige.conv(soil.geodata, locations=new.loc, krige=krige.control(cov.pars = model.1$cov.pars, cov.model='matern'))
soil.kre1$predict
soil.kre1$krige.var
# e2. 
# Halfway between the fifth and sixth columns and halfway 
# between the fifth and sixth rows.
new.loc = c(5.5, 5.5)
soil.kre2 = krige.conv(soil.geodata, locations=new.loc, krige=krige.control(cov.pars = model.1$cov.pars, cov.model='matern'))
soil.kre2$predict
soil.kre2$krige.var

# e2. 
# In the eighth row, one-fourth of the way from the third column 
# to the fourth column.
new.loc = c(3.25, 8)
soil.kre3 = krige.conv(soil.geodata, locations=new.loc, krige=krige.control(cov.pars = model.1$cov.pars, cov.model='matern'))
soil.kre3$predict
soil.kre3$krige.var

# ii)
soil.wnls = variofit(soil.variog, cov.model='matern')
soil.reml = likfit(soil.geodata, trend='2nd', ini.cov.pars=model.1$cov.pars, lik.method='REML',cov.model='matern')

cross.wnls = xvalid(soil.resdata, model=soil.wnls)
cross.reml = xvalid(soil.resdata, model=soil.reml)

mean(cross.wnls$error^2) 
mean(cross.wnls$std) 
sqrt(mean(cross.wnls$std^2))

mean (cross.reml$error^2) 
mean(cross.reml$std) 
sqrt(mean(cross.reml$std^2))
