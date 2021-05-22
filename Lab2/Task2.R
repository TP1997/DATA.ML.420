setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
data = read.table("RainIowa.txt")
attach(data)
library(geoR)

iowa.geodata = as.geodata(cbind(Latitude, Longitude, mm))
# i) 
# Compute the empirical and robust semivariogram estimates of the raw data 
# assuming isotropy. 
iowa.variog.M = variog(iowa.geodata, max.dist=6.254918/2, pairs.min=30)
iowa.variog.CH = variog(iowa.geodata, estimator.type='modulus', max.dist=6.254918/2, pairs.min=30)

par(mfrow=c(1,1))
plot(iowa.variog.M)
title("Matheron's empirical semivariogram\n(raw data)")
plot(iowa.variog.CH)
title("Cressie and Hawkin's empirical semivariogram\n(raw data)")

# ii)
# ii) Fit a linear surface to the data and obtain empirical and robust semivariogram 
# estimates of the residuals. Compare the results in i) and ii). What do you find?
model.linear = lm(mm~Latitude+Longitude, data=data)
iowa.resdata = as.geodata(cbind(Latitude, Longitude, model.linear$residuals))

iowa.variog.M2 = variog(iowa.resdata, max.dist=6.254918/2, pairs.min=30)
iowa.variog.CH2 = variog(iowa.resdata, estimator.type='modulus', max.dist=6.254918/2, pairs.min=30)

par(mfrow=c(1,1))
plot(iowa.variog.M2)
title("Matheron's empirical semivariogram\n(residuals)")
plot(iowa.variog.CH2)
title("Cressie and Hawkin's empirical semivariogram\n(residuals)")

# iii) 
# Check if there is any anisotropy using 4 standard directions. 
iowa.variog.dir = variog4(iowa.resdata, max.dist=6.254918/2)
plot(iowa.variog.dir, col=1:4)
title('Emipirical semivariogram in 4 directions')


# iv)
# Find the best Matern model fitted to the empirical semivariogram of the residuals
# in ii) using the WNLS and REML. Compare the results. 
iowa.geodata = as.data.frame(cbind(Latitude, Longitude, mm))
iowa.geodata = as.geodata(iowa.geodata)

iowa.variog.wnls = variofit(iowa.variog.M, cov.model='matern')
iowa.reml = likfit(iowa.geodata, trend='1st', ini=c(0.5, 0.5), lik.method='REML',cov.model='matern')
plot(iowa.variog.M)
lines(iowa.variog.wnls, col=6) 
lines(iowa.reml, col=3) 
legend(0.0,3000,legend=c('wnls','reml'), col=c(6,3), lty=1:1)
