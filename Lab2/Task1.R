setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
soil.data = read.table('soilph.txt', head=F)
soil.data = data.frame('r'=soil.data[,1], 'c'=soil.data[,2], 'ph'=soil.data[,3])
library(geoR)

model.quad = lm(soil.data$ph~soil.data$r*soil.data$c+I(soil.data$r^2)+I(soil.data$c^2), data=soil.data)
res.mat = tapply(model.quad$residuals, list(factor(soil.data$c),factor(soil.data$r)), function(x)x)
soil.resdata = as.geodata(cbind(soil.data$r, soil.data$c, model.quad$residuals))

# i)
# Compare the classical Matheron's empirical semivariogram and the 
# robust empirical semivariogram by Cressie and Hawkin's assuming isotropy.
soil.variog.M = variog(soil.resdata, max.dist=14.14214/2, pairs.min=30)
soil.variog.CH = variog(soil.resdata, estimator.type='modulus', max.dist=14.14214/2, pairs.min=30)

par(mfrow=c(1,1))
plot(soil.variog.M)
title("Matheron's empirical semivariogram")
plot(soil.variog.CH)
title("Cressie and Hawkin's empirical semivariogram")

# ii) 
# Compute the empirical semivariogram in each of the 4 directions 
# E-W, NE-SW, N-S, and NW-SE. Comment on the nature of each empirical semivariogram 
# regarding nugget effects, sills and the apparent behavior near the origin, etc.
soil.variog.dir = variog4(soil.resdata, max.dist=14.14214/2, pairs.min=30)
plot(soil.variog.dir, col=1:4)
title('Emipirical semivariogram in 4 directions')

# iii)
# Fit an isotropic semivariogram model to the semivariogram estimates (from part i)
# by OLS first. Then fit by weighted nonlinear least squares (WNLS) and  compare
# the results.
ini.vals = expand.grid(seq(0.5,1,l=5), seq(0,1,l=5))
soil.variog.ols = variofit(soil.variog.M, ini=ini.vals, fix.nugget=T, weights='equal')
plot(soil.variog.M)
lines(soil.variog.ols, col=4)

soil.variog.wnls = variofit(soil.variog.M, ini=ini.vals, fix.nugget=T)
lines(soil.variog.wnls, col=2)
title('Fitted isotropic semivariogram models')
legend(5.5,0.02,legend=c('ols','wnls'), col=c(4,2), lty=1:1)

# iv)
# Fit the same isotropic semivariogram model (as in iii) to the semivariogram 
# estimates by ML and REML. Compare the results with those in iii).
soil.geodata = as.data.frame(cbind(soil.data$r, soil.data$c, soil.data$ph))
soil.geodata = as.geodata(soil.geodata)

soil.ml = likfit(soil.geodata, trend='2nd', ini=c(1.5,1.2), fix.nugget=T)
soil.reml = likfit(soil.geodata, trend="2nd", ini=c(1.5,1.2), fix.nug=T, lik.method="REML") 

plot(soil.variog.M)
lines(soil.ml, col=4)
lines(soil.reml, col=3)
legend(5,0.02,legend=c('ML','REML'), col=c(4,3), lty=1:1)

# v)
soil.fit = model.quad
ols.dat = as.data.frame(cbind(soil.data$r, soil.data$c, resid=soil.fit$resid))
soil.ols = as.geodata(ols.dat) 

dist.mat = matrix(scan("distance.txt"), ncol =121 ,byrow = T) 
Vhat = cov.spatial(dist.mat, cov.model="exp", cov.pars = c(1,1))
X = model.matrix(soil.fit)
Z = data.matrix(soil.data$ph)
beta.hat = solve(t(X)%*%solve(Vhat)%*%X)%*%(t(X)%*%solve(Vhat)%*%Z)

beta.hat.reml = soil.reml$beta

