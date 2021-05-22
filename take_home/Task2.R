setwd('/home/tuomas/R/Projects/DATA.ML420/take_home')
library(sp)
load('iowa.Rdata')

# a)
spplot(iowa, 'logapr13', col.regions=heat.colors(16),
       main='Log counts of Covid cases, 13 April')

# b)
iowa.nb = poly2nb(iowa)
par(mar=c(3,3,1.5,1.5)+0.5, mgp=c(2,0.8,0))
plot(iowa)
plot(iowa.nb, coordinates(iowa), add=T, col=4,)
title('Neighbour system for each county')

# c)
iowa.weights.b = nb2listw(iowa.nb, style='B', zero.policy=T)
# Moran's I & Geary's C statistic
iowa.mori.b = moran.test(iowa$logapr13, iowa.weights.b, alternative='two.sided')
iowa.mori.b
iowa.geac.b = geary.test(iowa$logapr13, iowa.weights.b, alternative ='two.sided')
iowa.geac.b

# Moran I = 0.500678025, p-value < 2.2e-16
# Geary C = 0.490158317, p-value = 2.791e-14
# There seems to be (?) spatial autocorrelation between counties

iowa.weights.rs = nb2listw(iowa.nb, style='W', zero.policy=T)
iowa.mori.rs = moran.test(iowa$logapr13, iowa.weights.b, alternative='two.sided')
iowa.mori.rs
iowa.geac.rs = geary.test(iowa$logapr13, iowa.weights.b, alternative ='two.sided')
iowa.geac.rs

# Same results as above.

# d)
library(spatialreg)
# Linear model
iowa.lm = lm(iowa$logapr13~iowa$logmar22, data=iowa)
summary(iowa.lm)
lm.resid = residuals(iowa.lm)
# SAR - binary
iowa.sar.b = errorsarlm(iowa$logapr13~iowa$logmar22, listw=iowa.weights.b, method='eigen', quiet=F)
summary(iowa.sar.b)
#iowa.sar.b2 = spautolm(iowa$logapr13~iowa$logmar22, data=iowa, listw=iowa.weights.b, method='eigen', family='SAR')
#summary(iowa.sar.b2)
# SAR - rs
iowa.sar.rs = errorsarlm(iowa$logapr13~iowa$logmar22, listw=iowa.weights.rs, method='eigen', quiet=F)
summary(iowa.sar.rs)
# CAR - binary
iowa.car.b = spautolm(iowa$logapr13~iowa$logmar22, data=iowa, listw=iowa.weights.b, method='eigen', family='CAR')
summary(iowa.car.b)
# CAT - rs
iowa.car.rs = spautolm(iowa$logapr13~iowa$logmar22, data=iowa, listw=iowa.weights.rs, method='eigen', family='CAR')
summary(iowa.car.rs)

# AIC values
# Independence: 287.14
# SAR - binary: 251.3
# SAR - rs    : 244.63
# CAR - binary: 252.53
# CAR - rs    : 244.59
# Best fitter model(s) based on AIC: SAR-rs , CAR-rs

# Testing spatial autocorrelation on the residuals
# Moran's I statistic
lm.res.mori = moran.test(lm.resid, iowa.weights.b, alternative='two.sided')
lm.res.mori
sar.b.res.mori = moran.test(iowa.sar.b$residuals, iowa.weights.b, alternative='two.sided')
sar.b.res.mori
sar.rs.res.mori = moran.test(iowa.sar.rs$residuals, iowa.weights.rs, alternative='two.sided')
sar.rs.res.mori
car.b.res.mori = moran.test(residuals(iowa.car.b), iowa.weights.b, alternative='two.sided')
car.b.res.mori
car.rs.res.mori = moran.test(residuals(iowa.car.rs), iowa.weights.rs, alternative='two.sided')
car.rs.res.mori
# Values:
# Independent: 0.360572013, p-value = 4.501e-11
# SAR-binary : -0.016970166, p-value = 0.9044
# SAR-rs     : -0.027720182, p-value = 0.7624
# CAR-binary : -0.136213752, p-value = 0.02537
# CAR-rs     : -0.165443918, p-value = 0.007372
# Based on observed values, SAR models best the association of logapr13
# and logmar22

# Geary's C statistic
lm.res.geac = geary.test(lm.resid, iowa.weights.b, alternative='two.sided')
lm.res.geac
sar.b.res.geac = geary.test(iowa.sar.b$residuals, iowa.weights.b, alternative='two.sided')
sar.b.res.geac
sar.rs.res.geac = geary.test(iowa.sar.rs$residuals, iowa.weights.rs, alternative='two.sided')
sar.rs.res.geac
car.b.res.geac = geary.test(residuals(iowa.car.b), iowa.weights.b, alternative='two.sided')
car.b.res.geac
car.rs.res.geac = geary.test(residuals(iowa.car.b), iowa.weights.rs, alternative='two.sided')
car.rs.res.geac
# Values:
# Independent: 0.598772716, p-value = 2.307e-09
# SAR-binary : 0.9785676, p-value = 0.7464
# SAR-rs     : 1.017196524, p-value = 0.7725
# CAR-binary : 1.113323530, p-value = 0.08685
# CAR-rs     : 1.10303075, p-value = 0.08367
# Based on observed values, SAR models best the association of logapr13
# and logmar22

# e) Adding extra variable
# Linear model
iowa.lm2 = lm(iowa$logapr13~iowa$logmar22+iowa$log10pop, data=iowa)
summary(iowa.lm)
lm.resid = residuals(iowa.lm)
# SAR - binary
iowa.sar.b2 = errorsarlm(iowa$logapr13~iowa$logmar22+iowa$log10pop, listw=iowa.weights.b, method='eigen', quiet=F)
summary(iowa.sar.b)
# SAR - rs
iowa.sar.rs2 = errorsarlm(iowa$logapr13~iowa$logmar22+iowa$log10pop, listw=iowa.weights.rs, method='eigen', quiet=F)
summary(iowa.sar.rs)
# CAR - binary
iowa.car.b2 = spautolm(iowa$logapr13~iowa$logmar22+iowa$log10pop, data=iowa, listw=iowa.weights.b, method='eigen', family='CAR')
summary(iowa.car.b)
# CAR - rs
iowa.car.rs2 = spautolm(iowa$logapr13~iowa$logmar22+iowa$log10pop, data=iowa, listw=iowa.weights.rs, method='eigen', family='CAR')
summary(iowa.car.rs)

# Anova tests between models
anova(iowa.lm, iowa.lm2)
# Updated model (H1) should be selected
anova(iowa.sar.b, iowa.sar.b2)
# Updated model (H1) should be selected
anova(iowa.sar.rs, iowa.sar.rs2)
# Updated model (H1) should be selected
anova(iowa.car.b, iowa.car.b2)
anova(iowa.car.rs, iowa.car.rs2)

# AIC values
# Independence: 247.36
# SAR - binary: 218.66
# SAR - rs    : 217.31
# CAR - binary: 219.38
# CAR - rs    : 216.29
# Best fitter model(s) based on AIC: CAR-rs , SAR-rs
# The extra variable seems to help to obtain better models

# Moran's I statistic
# Values:
# Independent: 0.296890284, p-value = 3.578e-08
# SAR-binary : -0.007611224, p-value = 0.963
# SAR-rs     : -0.010705689, p-value = 0.993
# CAR-binary : -0.125880663, p-value = 0.03889
# CAR-rs     : -0.151490384, p-value = 0.01418

# Geary's C statistic
# Values:
# Independent: 0.667303117, p-value = 7.97e-06
# SAR-binary : 0.950123535, p-value = 0.4899
# SAR-rs     : 0.994997892, p-value = 0.9341
# CAR-binary : 1.074792218, p-value = 0.2919
# CAR-rs     : 1.103043683, p-value = 0.08807

# f)
# Selected bets model(s): CAR-rs , SAR-rs with extra variable
# Car - rs
iowa$carfit = iowa.car.rs2$fit$fitted.values
iowa$carres = iowa.car.rs2$fit$residuals
spplot(iowa, 'carfit', col.regions=heat.colors(16),
       main='CAR-rs, fitted')
spplot(iowa, 'carres', col.regions=heat.colors(16),
       main='CAR-rs, residuals')

# SAR - rs
iowa$sarfit = iowa.sar.rs2$fitted.values
iowa$sarres = iowa.sar.rs2$residuals
spplot(iowa, 'sarfit', col.regions=heat.colors(16),
       main='SAR-rs, fitted')
spplot(iowa, 'carres', col.regions=heat.colors(16),
       main='SAR-rs, residuals')
