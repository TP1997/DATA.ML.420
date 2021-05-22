setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
acex1 = read.table('acex1.txt', header=F) 
acex2 = read.table('acex2.txt', header=F) 
acex3 = read.table('acex3.txt', header=F) 

library(spdep)
# a)
acex1.map = t(array(data=acex1$V2, dim=c(5,5)))
acex2.map = t(array(data=acex2$V2, dim=c(5,5)))
acex3.map = t(array(data=acex3$V2, dim=c(5,5)))

# Rook's weighting
weight = function(i,j){
  n=5
  neighs = c(((i%%n)!=1)*(i-1), ((i%%n)>0)*(i+1), i-n, i+n)
  w = as.numeric(j %in% neighs)
  return(w)
}
S0 = function(){
  n=5
  sw = 0
  for (i in 1:(n*n)) {
    for (j in 1:(n*n)) {
      sw = sw + weight(i,j)
    }
  }
  return(sw)
}
Moran.I = function(data){
  n=5
  z.bar = mean(data)
  num = 0
  denom = 0
  for (i in 1:(n*n)) {
    for (j in 1:(n*n)) {
      w = weight(i,j)
      if (i==j){
        w = 0
      }
      num = num + w*(data[i]-z.bar)*(data[j]-z.bar)
    }
    denom = denom + (data[i]-z.bar)^2
  }
  s0 = S0()
  
  return(((n*n)/s0)*(num/denom))
}

Geary.c = function(data){
  n=5
  z.bar = mean(data)
  num = 0
  denom = 0
  for (i in 1:(n*n)) {
    for (j in 1:(n*n)) {
      w = weight(i,j)
      if (i==j){
        w = 1
      }
      num = num + w*(data[i]-data[j])^2
    }
    denom = denom + (data[i]-z.bar)^2
  }
  s0 = S0()
  
  return(((n-1)/s0)*(num/denom))
}

Ei = -1/24
mori.1 = Moran.I(acex1.map)
mori.1 > Ei
# Positive spatial autocorrelation in map 1
mori.2 = Moran.I(acex2.map)
mori.2 > Ei
# Slightly negative spatial autocorrelation in map 2. Maybe even 
# completely random distribution of values
mori.3 = Moran.I(acex3.map)
mori.3 > Ei
# Negative spatial autocorrelation in map 3

gc.1 = Geary.c(acex1.map)
gc.1
gc.2 = Geary.c(acex2.map)
gc.2
gc.3 = Geary.c(acex3.map)
gc.3

# All Geary's statistics suggest positive spatial autocorrelation.
# There might be some implementation issues or otherwise the statistic
# is just not that powerful.

