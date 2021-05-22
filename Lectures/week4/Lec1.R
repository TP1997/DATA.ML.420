#
#                  Ordinary Kriging
#
#
library(geoR)

# Simulate a random field with a Matern covariance model with sill=1, 
# range parameter=0.25, kappa=0.8
xy10 = expand.grid( x=seq(0,10,by=1), y=seq(0,10,by=1) ) # Coordinates of simulated data
s100 = grf(100, grid=xy10, cov.pars = c(1, .25), kappa=0.8) # Simulate observations

# Kriging with exponential model with true sill and range.
# Usually these parameters should be estimated first. 
loci = expand.grid(seq(0,5,l=20), seq(0,5,l=20)) # Prediction locations on a grid: 20x20 over x, y in (0,5). Use kriging to predict values for these locations.  
kc = krige.conv(s100, loc=loci, krige=krige.control(cov.pars=c(1, .25))) # Perform ordinary kriging

# Graphic comparison
par(mfrow=c(1,3))    # 3 plots in one row

image(s100, xlim=c(0,5), ylim=c(0,5) )  #original data on a smaller 5x5 grid 
image(kc,  main="kriging estimates")  # grayscale plot of interpolated (predicted) values 
image(kc, val= kc$krige.var, main="kriging variances")

# Let's take a look at what happens when we use different covariance models
kc.s = krige.conv(s100, loc=loci, krige=krige.control(cov.pars=c(1, 3*.25), cov.model= "spherical")) 
kc.g = krige.conv(s100, loc=loci, krige=krige.control(cov.pars=c(1, .25), cov.model="gaussian")) 

# Note that 3*.25 was used for range parameter in spherical model to match 
# the effective range for exponential and Gaussian models
image(s100, xlim=c(0,5), ylim=c(0,5) ) 
image(kc.s,  main="Estimates with spherical model") 
image(kc.g,  main="Estimates with gaussian model")  

image(kc.s, val= sqrt(kc$krige.var), main="kriging std errors from spherical model")
image(kc.g, val= sqrt(kc$krige.var), main="kriging std errors from gaussian model")

