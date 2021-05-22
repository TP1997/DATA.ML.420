library(splancs)
# Simulation of spatial point patterns

# Homogeneous Poisson Process (HPP) 
hpp100 <- rpoispp(100,win=owin(c(0,1),c(0,1))) # HPP with intensity 100 in the unit square
intensity(hpp100)
plot(hpp100, main="HPP intensity = 100")
#plot(pp.100.unif, main="HPP intensity = 100")

# Generates a CSR point process from random uniform point process conditionally 
# on a fixed number N of points
par(mfrow=c(1,1))
pp100.unif <-runifpoint(100,win=owin(c(0,1),c(0,1)))    ## Let N = 100
intensity(pp100.unif)
plot(pp100.unif, main="100 uniform points")  

# Inhomogeneous Poisson Process  (IPP) with different intensity functions 
lamb1 <- function(x,y) {100 * exp(10*x-5*y)}    # lambda1 : intensity function
Ipp1 <- rpoispp(lamb1, 100)
plot(Ipp1, main="IPP, intensity(x,y)=100*exp(10x-5y)",pch=20,xlab='x',ylab='y')

Ipp2 <- rpoispp(function(x,y) {100 * exp(-3*x)}, 100)
plot(Ipp2, pch=20,xlab='x',ylab='y')

# Poisson Cluster Process (PCP)
spoly<-as.points(c(0,1,1,0),c(0,0,1,1)) # need to define polygonal area
pcp1 <-pcp.sim(25,4,0.005,spoly) # Simulate homogeneous pcp 
plot(pcp1)

# intensity of parents is 15, average number of offspring is 7, and variance 
# of distance traveled by offspring (away from parent) in 0.0025
pcp2<-pcp.sim(15,7,0.00025,spoly)
plot(pcp2,main="PCP, (P,O,Spread)=(25,7,.00025)",pch=20,xlab='x',ylab='y')


# Simple Inhibition Process (SIP)   (click it to see the code)

#code to simulate a hard-core sequential inhibition point process.
sipgen <- function( npts=npts, delta){
  spatpts <- matrix(0,npts,2)
  
  npts <- 100
  spatpts <- matrix(0,npts,2)
  inpts <- 1
  
  while(inpts <= npts){ok <- 1
  xpt <- runif(1)
  ypt <- runif(1)
  chk <- inpts-1
  ichk <- 1
  while(ichk <= chk){
    distpt <- sqrt((xpt-spatpts[ichk,1])^2+(ypt-spatpts[ichk,2])^2)
    if(distpt < delta){
      ok <- 0
      ichk <- chk}
    ichk <- ichk+1}
  if(ok == 1){spatpts[inpts,1:2] <- cbind(xpt,ypt)
  inpts <- inpts+1
  }
  }
  
  list (pts=spatpts)
}

r.sip <- sipgen(100,.04)         # no. of points=100, delta (permissible distance)= .04 
pointmap( as.points(r.sip$pts) ) # library(splancs) is needed for this function
