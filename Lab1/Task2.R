
# i) 
# Simulate random fields with no trend on a 16 by 16 regular grid having the spatial 
# dependence according to Gaussian covariance with variance 1 and phi (theta_2)= 1.
library(geoR)
xy16 = expand.grid(x=seq(0.5,10,len=16), y=seq(0.5,10,len=16))
set.seed(321)

par(mfrow=c(2,2))
z.sim1 = grf(100, grid=xy16, cov.pars=c(1,1))
persp(z.sim1)
title("Variance=1, phi=1")

z.sim2 = grf(100, grid=xy16, cov.pars=c(1,3))
persp(z.sim2)
title("Variance=1, phi=3")

z.sim3 = grf(100, grid=xy16, cov.pars=c(1,5))
persp(z.sim3)
title("Variance=1, phi=5")

z.sim4 = grf(100, grid=xy16, cov.pars=c(1,5), nugget=6)
persp(z.sim4)
title("Variance=1, phi=5, nugget=6")

# ii)
# Compute the empirical semivariograms for each random fields and make the plots.
# Give brief comments regarding the efficient range from each covariance model.
z1.var = variog(z.sim1, max.dist=13.43503/2, min.pair=30)
z2.var = variog(z.sim2, max.dist=13.43503/2, min.pair=30)
z3.var = variog(z.sim3, max.dist=13.43503/2, min.pair=30)
z4.var = variog(z.sim4, max.dist=13.43503/2, min.pair=30)

par(mfrow=c(2,2))
plot(z1.var, type='l') 
title('Grf with variance=1, phi=1')
plot(z2.var, type='l')
title('Grf with variance=1, phi=3')
plot(z3.var, type='l')
title('Grf with variance=1, phi=5')
plot(z4.var, type='l')
title('Grf with variance=1, phi=5, nugget=6')

# iii)
# Simulate two random fields on a 16 by 16 grid with spherical covariance (var=1 , phi=3) 
# and (1, 5). Make the  surface plots  and plots of empirical  variograms for each random field. 
# Give brief comments. 

par(mfrow=c(1,2))
z.sim1 = grf(100, grid=xy16, cov.pars=c(1,3), cov.model='spherical')
z.sim2 = grf(100, grid=xy16, cov.pars=c(1,5), cov.model='spherical')
persp(z.sim1)
title("Variance=1, phi=3")
persp(z.sim2)
title("Variance=1, phi=5")

par(mfrow=c(1,2))
z1.var = variog(z.sim1, max.dist=13.43503/2, min.pair=30)
z2.var = variog(z.sim2, max.dist=13.43503/2, min.pair=30)
plot(z1.var, type='l') 
title("Variance=1, phi=3")
plot(z2.var, type='l')
title("Variance=1, phi=5")
