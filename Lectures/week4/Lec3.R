#
#                  Ordinary Kriging system of equations 
#
#
library(geoR)

# Recall that the kriging weights provide us the effect of spatial locations 
# on the predictor. Also, the kriging variances which depend on the kriging weights, 
# but  not on the data can be used to answer sampling design questions, 
# such as where to take one more observation to minimize the maximum (or average) 
# value of the kriging variances over the entire spatial domain.

# However, there are no written R functions for kriging which give the kriging weights:
# Kriging system of equations is set up manually  (or tricks can be done to compute these
# weights).

# Example given in lecture :   gamma(|| h ||)= 1 - exp(-|| h ||/2)

#      s1  s2  s3  s4  s5  s6  s0:  data locations + prediction site (s0)  
xc = c(0,  1,  2,  3,   4,  3,  2) 
yc = c(0,  1,  3,  2,   1,  0,  1) 

dd = cbind(xc,yc)   # coordinates to compute distances between observations
m = length(xc)

dist <- matrix( 0, nrow=m, ncol=m)      # distance matrix
for (i in 1:m){  
  for (j in 1:m){ 
    dist[i,j] = ( (dd[i,1]-dd[j,1])^2 + ( dd[i,2]-dd[j,2])^2 )^.5  
  } 
} 

c0 = 0    # nugget effect 
c1 = 1    # partial sill  
phi = 2   # range parameter 

# Compute the G (or Gamma) matrix for kriging system of equations:   G w=  D
G = matrix(NA, nrow=m, ncol=m) 
for (i in 1:m-1){
  for (j in 1:m-1){
    G[i,j] = c1 * exp(-dist[i,j]/phi)  
  }
}

G[m,] = rep(1,m)      # for Lagrange multiplier
G[,m] = rep(1,m) 
G[m,m] = 0 

# Repeat with other sites for weights at the remaining sites: 
D = rep(0,m) 
for (j in 1:m){ 
  D[j] = c1 * exp (-dist[m,j]/phi) 
}
D[m] = 1

# Obtain the weights and the Lagrange multiplier (keep 3 decimal points): 
wt = solve(G) %*% D 
wt = round( solve(G) %*% D, digits=3 ) 
# Compute the prediction variance of the estimate: 
var_z_hat = round( c0+c1-t(wt) %*% D, digits=3)
#> z_hat <- round(wt[-m] %*% Z, digits=3)      # if observed Z values are given ( here Z is not known)

plot(xc[-7], yc[-7], xlab="X coordinate", ylab="Y coordinate", main="Kriging weights", pch=16)  # plot the locations

points(xc[7], yc[7], pch=13)

text(0,0, label= wt[1], pos=4)    #plot the weights at each location
text(1,1, label= wt[2], pos=3) 
text(2,3, label= wt[3], pos=1) 
text(3,2, label= wt[4], pos=3)  
text(4,1, label= wt[5], pos=2) 
text(3,0, label= wt[6], pos=3) 






