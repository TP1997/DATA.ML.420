library(spatstat)
setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
swamp = read.csv('swamp.csv', as.is=T) 

cypress = subset(swamp, live == 1 & sp=='TD') # keep live cypress (Taxodium distichum) locations

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
plot(density(cypress.ppp, sigma=bw.CvL, adjust=1.5))

# c)
plot(Kest(cypress.ppp))
r = seq(0,12,by=0.005)
env = envelope(cypress.ppp, fun=Kest, r=r, nrank=2, nsim=99)
plot(env)

#poly = spoints(c(0,0,50,0,50,200,0,50))
#cypress.khat = khat(cypress.ppp, poly=poly, r)

# d)
cypress.fit1 = ppm(cypress.ppp, ~1)
cypress.fit1
# Standard error = 0.1048285
plot(predict(cypress.fit1), main='Constant intensity')

# e)
cypress.fit2 = ppm(cypress.ppp, ~x+y)
# Standard errors:
# B0 = 0.274971311
# B1 = 0.007265246
# B2 = 0.001817664
plot(predict(cypress.fit2), main='Linear inhomogenous intensity')

# f)
library(splancs)
wt = subset(swamp, live == 1 & sp=='FX')
wt.spp = as.points(cbind(wt$x, wt$y))
cypress.spp = as.points(cbind(cypress$x, cypress$y))

#poly = spoints(c(0,0,50,0,50,200,0,200))
#poly = spoints(c(0,0,50,0,50,200,0,200))
poly = cbind(c(0,0,200,200),c(0,50,50,0))
mK12 = k12hat(cypress.spp, wt.spp, poly, r)
plot(r,sqrt(mK12/pi)-r)

m12env = Kenv.tor(cypress.spp,wt.spp,poly,99,r) 
lines(r,sqrt(m12env$upper/pi)-r)
lines(r,sqrt(m12env$lower/pi)-r)
