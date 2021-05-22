setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
load('scotland.Rdata')
library(sp)
library(spdep)

# a)
spplot(scotland, 'logratio', col.regions=heat.colors(16))

# b)
scotland.nb = poly2nb(scotland)
par(mar=c(3,3,0,0)+0.2, mgp=c(2,0.8,0))
plot(scotland)

plot(scotland.nb, coordinates(scotland), add=T, col=4)

scotlad.mat.w = nb2listw(scotland.nb, style='B', zero.policy=T)

# c)
scot.mori = moran.test(scotland$logratio, scotlad.mat.w, alternative='two.sided')
scot.mori
scot.geac = geary.test(scotland$logratio, scotlad.mat.w)
scot.geac
# Moran I statistic = 0.491199227
# Geary C statistic = 0.43800580

# Monte Carlo:
scot.mori.mc = moran.mc(scotland$logratio, scotlad.mat.w, nsim=99)
scot.mori.mc
# Moran I statistic = 0.4912
scot.geac.mc = geary.mc(scotland$logratio, scotlad.mat.w, nsim=99)
scot.geac.mc
# Geary C statistic = 0.43801

# d)
scot.lm = lm(scotland$logratio~scotland$percentAFF, data=scotland)
scot.resid = residuals(scot.lm)
# ?lm.morantest
scot.res.mori = moran.test(scot.resid, scotlad.mat.w, alternative='two.sided')
scot.res.mori
# Moran I statistic for residuals: 0.267703766
# There seems to be positive spatial autocorrelation whitin residuals
scot.res.geac = geary.test(scot.resid, scotlad.mat.w)
scot.res.geac
# Geary C statistic for residuals: 0.69766602
# There seems to be positive spatial autocorrelation whitin residuals as well

#scot.res.mori.mc = moran.mc(scot.resid, scotlad.mat.w, nsim=99)
#scot.res.mori.mc
#scot.geac.mc = geary.mc(scot.resid, scotlad.mat.w, nsim=99)
#scot.geac.mc

# e)
library(spatialreg)
scotlad.rsw = nb2listw(scotland.nb, style='W')
scot.sar = errorsarlm(scotland$logratio~scotland$percentAFF, listw=scotlad.rsw, data=scotland, method="eigen", quiet=FALSE)
summary(scot.sar)

scot.res.sar.mori = moran.test(scot.sar$residuals, listw=scotlad.rsw)

# f)
scot.sar.res = residuals(scot.sar, type='response')
scotland$sar.res = scot.sar.res
scot.sar.res.mori = moran.test(scot.sar.res, scotlad.mat.w, alternative='two.sided')
scot.sar.res.mori
# Moran I statistic for residuals: -0.045980812
# There seem not to be spatial autocorrelation whitin residuals
scot.sar.res.geac = geary.test(scot.sar.res, scotlad.mat.w)
scot.sar.res.geac
# Geary C statistic for residuals: 1.05371997
# There seem not to be spatial autocorrelation whitin residuals

# e)
spplot(scotland, 'sar.res', col.regions=heat.colors(16),main='sar residuals')

scot.sar.fit = fitted.values(scot.sar, type='response')
scotland$sar.fit = scot.sar.fit
spplot(scotland, 'sar.fit', col.regions=heat.colors(16), main='sar fitted')

scot.lm.res = residuals(scot.lm, type='response')
scotland$lm.res = scot.lm.res
spplot(scotland, 'lm.res', col.regions=heat.colors(16), main='LM residuals')
