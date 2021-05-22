library(spatstat)
setwd('/home/tuomas/R/Projects/DATA.ML420/take_home')
swamp = read.csv('swamp.csv', as.is=T)
cypress = subset(swamp, live == 1 & sp=='TD')

# a)
cypress.ppp = as.ppp(cypress[,c('x','y')], W=c(0,50, 0, 200))
plot(cypress.ppp)

# b)
# Global intensity
npoints(cypress.ppp) / area(cypress.ppp)
intensity(cypress.ppp)
bw = bw.ppl(cypress.ppp)

plot(density(cypress.ppp, sigma=bw))
plot(density(cypress.ppp, sigma=bw.diggle))
plot(density(cypress.ppp, sigma=bw.CvL))
plot(density(cypress.ppp, sigma=bw.CvL, adjust=.7))

# c)
plot(Kest(cypress.ppp))
r = seq(0,12,by=0.005)
env = envelope(cypress.ppp, fun=Kest, r=r, nrank=2, nsim=99)
plot(env)

# d) Homogenous Poisson Process fitting
cypress.fit.hpp = ppm(Q=cypress.ppp, ~1)
cypress.fit.hpp
# Standard error = 0.1048285
plot(predict(cypress.fit.hpp), main='Constant intensity (0.0091)')

# e) Inhomogenous Poisson Process fitting
cypress.fit.ipp = ppm(cypress.ppp, ~x+y)
summary(cypress.fit.ipp)
# Standard errors:
# beta_0: 0.274971311
# beta_1: 0.007265246
# beta_2: 0.001817664
plot(predict(cypress.fit.ipp), main='IPP with linear intensity')

# f)
library(splancs)
wt = subset(swamp, live == 1 & sp=='FX')
wt.spp = as.points(cbind(wt$x, wt$y))
cypress.spp = as.points(cbind(cypress$x, cypress$y))

poly = cbind(c(0,50,50,0),c(0,0,200,200))
mK12 = k12hat(cypress.spp, wt.spp, poly, r)
plot(r,sqrt(mK12/pi)-r)

m12env = Kenv.tor(cypress.spp,wt.spp,poly,199,r) 
lines(r,sqrt(m12env$upper/pi)-r)
lines(r,sqrt(m12env$lower/pi)-r)

plot(r,sqrt(m12env$upper/pi)-r)
lines(r,sqrt(m12env$lower/pi)-r)
