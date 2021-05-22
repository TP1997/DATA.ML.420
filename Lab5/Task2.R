library(splancs)
library(spatstat)

# i) & ii)
lambda1 = function(x,y){exp(-2*x-y)}
lambda2 = function(x,y){exp(-6*abs(x-0.5)-2*abs(y-0.5))}
lambda3 = function(x,y){exp(-50*((x-0.25)^2)+(y-0.25)^2)+
                        exp(-50*((x-0.8)^2)+(y-0.3)^2)+
                        exp(-50*((x-0.4)^2)+(y-0.7)^2)}

getn = function(n=100, fun){
  Ipp = rpoispp(fun, n, nsim=n*5)
  Ipp.x = c()
  Ipp.y = c()
  for (ipp in Ipp) {
    if (ipp$n > 0){
      Ipp.x = c(Ipp.x, ipp$x)
      Ipp.y = c(Ipp.y, ipp$y)
    }
  }
  return (list('x'=Ipp.x[1:n], 'y'=Ipp.y[1:n]))
}

Ipp1 = getn(100, lambda1)
intensity(ppp(Ipp1$x, Ipp1$y, c(0,1), c(0,1)))
Ipp2 = getn(100, lambda2)
intensity(ppp(Ipp2$x, Ipp2$y, c(0,1), c(0,1)))
Ipp3 = getn(100, lambda3)
intensity(ppp(Ipp3$x, Ipp3$y, c(0,1), c(0,1)))

plot(Ipp1$x, Ipp1$y, main="IPP, intensity(x,y)=lambda1\n100 points",pch=20,xlab='x',ylab='y')
plot(Ipp2$x, Ipp2$y, main="IPP, intensity(x,y)=lambda2\n100 points",pch=20,xlab='x',ylab='y')
plot(Ipp3$x, Ipp3$y, main="IPP, intensity(x,y)=lambda3\n100 points",pch=20,xlab='x',ylab='y')

Ipp1 = getn(500, lambda1)
Ipp2 = getn(500, lambda2)
Ipp3 = getn(500, lambda3)

plot(Ipp1$x, Ipp1$y, main="IPP, intensity(x,y)=lambda1\n500 points",pch=20,xlab='x',ylab='y')
plot(Ipp2$x, Ipp2$y, main="IPP, intensity(x,y)=lambda2\n500 points",pch=20,xlab='x',ylab='y')
plot(Ipp3$x, Ipp3$y, main="IPP, intensity(x,y)=lambda3\n500 points",pch=20,xlab='x',ylab='y')

# iii)
# Based on the plots, the generated point data seems to be heterogenous in all cases. Also
# when generating the data with labmda3, there might also be two separate point clusters.
# iv)
sip = sipgen(100, 0.02)
pointmap( as.points(sip$pts) )
