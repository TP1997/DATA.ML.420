# Simulation of Gaussian random fields

# Simulated data can be quite useful to study various estimation and prediction methods 
# not only for evaluating sampling strategies. Gaussian random fields can be generated
# using the function called 'grf'.

# Random field on a regular grid 20 by 20 (mean function is zero but you can add your own 
# trend, default covariance function= exponential, based on cholesky decomposition):
par(mfrow=c(1,1))
library(geoR)
xy20 = expand.grid(x=seq(0.5,10,len=20), y=seq(0.5,10,len=20))
set.seed(321)
z.sim1 = grf(100, grid=xy20, cov.pars=c(1,0.01))

zg.sim1 = as.geodata(cbind(z.sim1$coords, z.sim1$data))
points.geodata(zg.sim1)
plot(zg.sim1)

# Realizations of random processes with different range (and nugget) parameters:
z.sim2 = grf(100, grid=xy20, cov.pars=c(1, 2))       # effective range= 3*2=6 
z.sim3 = grf(100, grid=xy20, cov.pars=c(1,8))        # effective range= 3*8= 24 
z.sim4 = grf(100, grid=xy20, cov.pars=c(1,8), nugget=0.25)

# Simulated realization by 3D-surface plots:
par(mfrow=c(2,2))
persp(z.sim1)
persp(z.sim2)
persp(z.sim3)
persp(z.sim4)

# Which model gives the smoothest (most correlated) data surface? 
image(z.sim1)     # Simulated realization by grayscale plots:
image(z.sim2)                         
image(z.sim3)                         
image(z.sim4)

# Check the spatial dependence with their semivariogram estimates
z1.var = variog(z.sim1) # Check what output/attributes the function 'variog' produces!

z1.var = variog(z.sim1, max.dist=14.14214/2, min.pair=30)
# Recall the rules of thumb: plot the empirical sample variograms up to 
# half the max distance and for those with more than 30 pairs
z2.var = variog(z.sim2, max.dist=7.07168, min.pair=30)                
z3.var = variog(z.sim3, max.dist=7.07168, min.pair=30)                       
z4.var = variog(z.sim4, max.dist=7.07168, min.pair=30)

plot(z1.var)  # check the effective range from each plot 
plot(z2.var)
plot(z3.var)
plot(z4.var)

cf. > plot(z.sim1)

# On an irregular grid, matern model
z.sim5 = grf(144, xlims= c(0,10), ylims=c(0,10), kappa=1, cov.pars=c(1,0.1))
z5 = as.geodata(cbind(z.sim5$coords,z.sim5$data))                       
plot(z5)











