#
#                  Cross validation
#
#

# To perform model validation, the most commonly used technique is the leaving-one-out 
# cross validation where each data location is removed from the data set in turn and the value 
# of the variable is predicted based on the remaining locations for the given model in turn.
set.seed(1569) 
z.sim = grf(144, xlims= c(0,10), ylims=c(0,10), cov.pars = c(3, 2))

# Restricted Maximum likelihood estimation 
zsim.reml = likfit(z.sim, ini = c(3.2, 1.5), fix.nug = TRUE, met="REML")

# Weighted nonlinear least squares estimation 
zsim.var = variog(z.sim, max.dist = 12.4319909/2 ) 
zsim.wnls = variofit(zsim.var, ini = c(3.2, 1.5), fix.nug = TRUE)

# Now, performing cross-validation to compare models selected by 2 methods 
# Here, the default is leaving-one-out method but you can do cross validation 
# on other grid sets. 
zcross.reml = xvalid(z.sim, model = zsim.reml) 
zcross.wnls = xvalid(z.sim, model = zsim.wnls)

# Compute the average PRESS, mean of standardized PRESS residuals, root mean squared 
# PRESS residuals and compare 2 estimates for the given data:
  
mean(zcross.reml$error^2) 
mean(zcross.reml$std) 
sqrt( mean(zcross.reml$std^2) )

mean (zcross.wnls$error^2) 
mean(zcross.wnls$std) 
sqrt( mean(zcross.wnls$std^2) )

# The estimated model by REML seems to be better here - why? 