#
#                  Universal Kriging
#
#
setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
coal.ash = read.table('coalash.txt', head=T)
library(geoR)

# user defined locations 
newdata1<-c(13.5, 22.5)

# Coal ash data 
coal.lm = lm(coal~x+y, data= coal.ash)
summary(coal.lm) 
c.resid = coal.lm$resid
coal = as.geodata( cbind(coal.ash$x, coal.ash$y, c.resid) ) 

# Kriging with external trend and universal kriging can be defined by setting 
# `type.krige = "OK"' and specifying the trend model.  
# The arguments  for trend: `trend.d' and `trend.l'. 
coal.kr1 = krige.conv(coal, locations=newdata1, krige=krige.control(trend.d="2nd", trend.l="2nd", cov.pars=c(0.41, 4.31), nugget=0.89, cov.model="spherical" ) )

# Prediction on a new data grid 
newloc = expand.grid( seq(1,16,0.5), seq(1,23,0.5) ) 
par( mfrow=c(1,2) ) 
plot(coal$coords) 
plot(newloc)

# coal.newloc<- expand.grid( seq(0,16,1), seq(0,23,1) ) 
coal.kr2 = krige.conv (coal, locations=newloc, krige=krige.control (type.krige="ok",  trend.d="2nd", trend.l="2nd", cov.pars=c(0.41, 4.31), nugget=0.89, cov.model="spherical" ) )

coal.mat = tapply(coal.ash$coal, list(factor(coal.ash$x),factor(coal.ash$y)),function(x) x ) 
image(coal.mat )
image(coal.kr,  val=coal.kr2$predict) 
image(coal.kr,  val=coal.kr2$krige.var)
