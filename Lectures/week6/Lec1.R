# The Lansing Woods data with two types of trees: hickory and maple trees. 
# The whole data set includes locations of 6 species of trees in a forest in Michigan
library(splancs)
library(spatstat)

data(lansing)
summary(lansing)
plot(split(lansing))

# The Lansing Woods data with two types of trees: hickory and maple trees
hick <- lansing[lansing$marks=="hickory",]
hick.spp <- as.points(cbind(hick$x,hick$y))  # a point pattern object for 'splancs'
maple<- lansing[lansing$marks=="maple",] 
maple.spp<- as.points(cbind(maple$x,maple$y))

# Plot two species separately 
par(mfrow=c(1,2)) 
par(pty="s") 
pointmap(hick.spp,main="Hickories") 
pointmap(maple.spp,main="Maples")

# Overlay one species with the other 
par(mfrow=c(1,1)) 
pointmap(hick.spp,col=3) 
points(maple.spp,col=2, pch='x')

r <- 1:70/100   # vector of distances 
poly <- cbind(c(0,0,1,1),c(0,1,1,0))

# K12 function :  Cross K-function to test independence
mK12 <- k12hat(hick.spp,maple.spp, poly, r)
plot(r,sqrt(mK12/pi)-r,xlim=c(0,0.7),ylim=c(-.1,.1))
m12env <- Kenv.tor(hick.spp,maple.spp,poly,99,r)  # Toroidal shifts for simulation envelopes 

lines(r,sqrt(m12env$upper/pi)-r)     # L- function ( -L in the note)
lines(r,sqrt(m12env$lower/pi)-r) 

unitsquare <- spoints(c(0,0,1,0,1,1,0,1))
hick.khat <- khat(hick.spp,unitsquare, seq(0,1,.01))    # Ripley's K function for hickory
maple.khat <- khat(maple.spp,unitsquare,seq(0,1,.01)) # Ripley's K function for maple

par(mfrow=c(1,2)) 
mK <- Kenv.csr(514, poly,nsim=25,r)    # Simulation Envelope for Khat
plot(maple.khat)
lines(mK$lower)     # lower bound of simulation envelope
lines(mK$upper)

# Test for random labelling
khat.diff <- hick.khat-maple.khat
plot(seq(0,1,.01),khat.diff,xlab="distance",ylab="K 11 - K 22",type="l")
diff.lab <- Kenv.label(hick.spp,maple.spp,unitsquare,nsim=99,seq(0,1,.01)) # simulation envelopes for Random Labelling

lines(seq(0,1,.01),diff.lab$upper,lty=2)
lines(seq(0,1,.01),diff.lab$lower,lty=2)


# Model fitting with intensity function :  Poisson,  Inhomogeneous Poisson or inhibition processes
library(spatstat)
m.ppp <- ppp(maple$x, maple$y,W=c(0,1, 0,1 ))

npoints(m.ppp) / area(m.ppp)  #global intensity
intensity(m.ppp)
bw.ppl(m.ppp) # to find an appropriate bandwidth, sigma

#kernel smoothing by different methods
par(mfrow=c(2,2)) 
plot(density(m.ppp, sigma=0.05))
plot(density(m.ppp, sigma=bw.diggle)) #Diggle's bandwidth
plot(density(m.ppp, sigma=bw.CvL))  #Cambell's formula 
plot(density(m.ppp, sigma=bw.CvL, adjust=1.5)) #multiplicative adjustment to smoothing parameter
par(mfrow=c(1,1))

# Modeling the process via intensity 
m.fit1 <- ppm(Q = m.ppp, ~1)    # Fitting a HPP
AIC(m.fit1)  # [1] -5387.006
coef(m.fit1)

m.fit2 <- ppm(Q = m.ppp, ~x + y) # Fitting an IPP
plot(m.fit2) 
AIC(m.fit2)    #[1] -5488.886
m.fit <- ppm(Q = m.ppp, trend = ~x + y + I(x^2) + I(y^2) + I(x*y))
AIC(m.fit)     #[1] -5544.653

plot(predict(m.fit), main = "density of prediction") 
plot(density(m.ppp), main = "density of data")

plot(predict(m.fit, se=TRUE)$se, main = "SE")

#The likelihood ratio test that compares the HPP to the IPP 
anova(m.fit1, m.fit, test='Chi')



# Fit Poisson Cluster Process by minimum contrast

m.cfit <-kppm(m.ppp,method="mincon")
summary(m.cfit)
plot(m.cfit)
plot(simulate(m.cfit, drop = TRUE), main = "")

plot(envelope(m.cfit, Gest, nsim = 39, global = TRUE))

# Fit Log Gaussian Poisson cluster process to simulated data
lgcp.mod1<-kppm(m.ppp,clusters="LGCP",method="mincon")
