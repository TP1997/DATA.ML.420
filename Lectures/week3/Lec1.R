setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
coal.ash = read.table('coalash.txt', head=T)
library(geoR) 

# Coal Ash data from the previous lab
# We fit a linear function of the coordinates as a drift (trend) to the data. 
# Then, compute the isotropic sample semivariogram of the 'lm' residuals 
coal.lm = lm(coal~x+y, data= coal.ash)
summary(coal.lm) 
c.resid = coal.lm$resid

# We need to make an 'geoR' object to apply 'variog' function
coal = as.geodata( cbind(coal.ash$x, coal.ash$y, c.resid) ) 

# Empirical (or sample) semivariogram estimation (Matheron's method of moment type):
# First compute the empirical semivariogram without any option. 
# Then compute empirical semivariogram estimates upto the half the maximum distance 
# (Recall the rules of thumb in the notes!)
coal.var = variog(coal)   # get the max distance  =24.16609 # check also 'n' for number of pairs per lag (pairs.min= 30)
coal.var = variog(coal, option="cloud",max.dist =12.08305)     # compute semivariogram cloud 
plot(coal.var)

coal.var = variog(coal, max.dist =12.08305)    # omnidirectional estimates assuming isotropy
plot(coal.var)

# You can also specify the lags at which you want sample semivariogram estimates.
coal.uvec = c(1.201634, 2.000000, 2.236068, 3.036036, 3.605551, 4.234139, 5.039712, 
              5.472889, 6.063483, 6.552482, 7.147503, 7.894487, 8.459312, 9.094676,
              9.624306, 10.174499, 10.843929, 11.400797, 12.08305) 
coal.uvar = variog(coal, uvec= coal.uvec, max.dist =12.08305) 
plot(coal.uvar)   # compare this with the sample variogram plot. 

# Robust sample semivariogram (Cressie/Hawkins' estimator): 
coal.var1 = variog(coal, estimator.type="modulus", uvec=coal.uvec, max.dist =12.08305) 
lines(coal.var1, col=6)   # overlay plots

# Directional semivariograms: Check anisotropy!
coal.var2 = variog4(coal, max.dist =12.08305)  # for 4 different directions(0,45,90,135 degrees) 
plot(coal.var2) 
# For specific other directions, e.g. pi/6 = 30 degrees with tolerance angle 15 degrees 
coal.var3 = variog(coal, direction=pi/6, tolerance=pi/12, max.dist =12.08305 ) 
plot(coal.var) 
lines(coal.var3, col=4)


# Semivariogram of residuals with polynomial trends: 
coal.d = as.data.frame(cbind( coal.ash$x,coal.ash$y, coal.ash$coal))   #From the previous lab

coal.geo = as.geodata(coal.d) 
coal.var4 = variog(coal.geo, trend="2nd", max.dist =12.08305)  # quadratic trend is fit
plot(coal.var) 
lines(coal.var4,col=3)


# Fitting valid semivariogram models

# i) OLS : Ordinary Least Squares
# Need to specify initial values for nugget, range, partial sill 
# (and the smoothing parameter for matern model). The range parameter in R (called phi)
# is the same as 1/theta_2 in classnotes.

# Note that I have ignored the anisotropic feature shown in semivariogram estimates 
# for simple illustration. For the usual data analyses, it is advised to incorporate 
# such feature in your variogram model-fitting. (If we check the direction variogram
# for N-S and E-W only, residual auto-correlation does not seem to depend
# much on direction.)
ini.vals = expand.grid(seq(0.5,2,l=5), seq(0,2,l=5)) # nugget and kappa (smoothing par.) are fixed 
coal.ols = variofit(coal.var, ini=ini.vals, fix.nug=TRUE, wei="equal") 
plot(coal.var) 
lines(coal.ols,col=4) 
summary(coal.ols) # estimates of partial sill and (effective) range parameter: 1.21704 0.32867 

# ii) WNLS: Weighted Nonlinear Least Squares
coal.wnls = variofit(coal.var, ini=ini.vals, fix.nug=TRUE) 
lines(coal.wnls, lty=2) 
summary(coal.wnls) # estimates of partial sill and (effective) range parameter: 1.2170271 0.3245302 
# Initial values for nugget and kappa added: (kappa is for the Matern model)
coal.wnls2 = variofit(coal.var, ini=ini.vals, fix.nug=FALSE, nugget=0.1, fix.kappa=FALSE, kappa=0.3) 
lines(coal.wnls2, lty=2,col=6) # 1.21711627  0.06704351 25.29470167 : not an exponential model 

# iii) ML/REML
coal.d = as.data.frame(cbind( coal.ash$x,coal.ash$y, coal.ash$coal)) #From the previous lab
coal.geo = as.geodata(coal.d) # create an object of the class 'geodata' # need to use the original data, not residuals
# This procedure is somewhat time demanding! 
coal.ml <- likfit(coal.geo, trend="2nd", ini=c(1.5,1.2), fix.nug=TRUE)   # default is ML estimation
coal.ml
summary(coal.ml)

coal.reml = likfit(coal.geo, trend="2nd", ini=c(1.5,1.2), fix.nug=TRUE, met="REML") 
coal.reml

plot (coal.var4) # empirical semivariogram estimates of residuals 
lines(coal.ml, col=4) 
lines(coal.reml, col=3)

# Envelopes for empirical semivariogram estiamtes given estimated model parameter 
coal.env = variog.model.env(coal.geo, obj.v=coal.var4, model.pars= coal.reml) 
plot(coal.var4, env=coal.env)
