library(geoR)

get_dm = function(xc, yc){
  dd = cbind(xc,yc)   # coordinates to compute distances between observations
  m = length(xc)
  
  dist <- matrix( 0, nrow=m, ncol=m)      # distance matrix
  for (i in 1:m){  
    for (j in 1:m){ 
      dist[i,j] = ( (dd[i,1]-dd[j,1])^2 + ( dd[i,2]-dd[j,2])^2 )^.5  
    } 
  }
  
  return(dist)
}

sv_a = function(h){
  return(1-exp(-h / 4))
}
sv_b = function(h){
  return(0.25 + 0.75*(1-exp(-h / 2)))
}

get_G = function(m, dist, sv_model){
  # Compute the G (or Gamma) matrix for kriging system of equations:   G w=  D
  G = matrix(NA, nrow=m, ncol=m) 
  for (i in 1:m-1){
    for (j in 1:m-1){
      G[i,j] = sv_model(dist[i,j]) #c1 * exp(-dist[i,j]/phi)  
    }
  }
  G[m,] = rep(1,m)      # for Lagrange multiplier
  G[,m] = rep(1,m) 
  G[m,m] = 0 
  
  return(G)
}

get_g = function(m, dist, sv_model){
  D = rep(0,m) 
  for (j in 1:m){ 
    D[j] = sv_model(dist[m,j]) #c1 * exp (-dist[m,j]/phi) 
  }
  D[m] = 1
  
  return(D)
}

# i)
# to  see  the  effect  that  semivariogram  parameters  may  have  on  the  ordinary
# kriging predictor and the ordinary kriging variance, compute the ordinary kriging weights 
# and thekriging variance for each of the following semivariograms at site s0.

#      s1  s2  s3  s4  s5  s6  s0:  data locations + prediction site (s0)  
xc = c(0,  1,  2,  3,   4,  3,  2) 
yc = c(0,  1,  3,  2,   1,  0,  1) 
dist = get_dm(xc, yc)
m = length(xc)

# a) gamma(|h|) = 1 - exp(-|h|/4)
Gamma.a = get_G(m, dist, sv_a)
gamma.a = get_g(m, dist, sv_a)
# Obtain the weights and the Lagrange multiplier
wt.a = solve(Gamma.a) %*% gamma.a
wt.a.round = round( wt.a, digits=3 )
# Compute the prediction variance of the estimate: 
var_z_hat.a = t(wt.a)%*%gamma.a
var_z_hat.a.round = round(var_z_hat.a, digits=3)

# b) gamma(|h|) = 0.25 + 0.75 * ( 1 - exp(-|h|/2) )
Gamma.b = get_G(m, dist, sv_b)
gamma.b = get_g(m, dist, sv_b)
# Obtain the weights and the Lagrange multiplier
wt.b = solve(Gamma.b) %*% gamma.b
wt.b.round = round( wt.b, digits=3 )
# Compute the prediction variance of the estimate: 
var_z_hat.b = t(wt.b)%*%gamma.b
var_z_hat.b.round = round(var_z_hat.b, digits=3)

# plotting
par(mfrow=c(1,2))
plot(xc[-7], yc[-7], xlab="X coordinate", ylab="Y coordinate", main="OK, Kriging weights (a)", pch=16)
points(xc[7], yc[7], pch=13)
text(0,0, label= wt.a.round[1], pos=4)    #plot the weights at each location
text(1,1, label= wt.a.round[2], pos=3) 
text(2,3, label= wt.a.round[3], pos=1) 
text(3,2, label= wt.a.round[4], pos=3)  
text(4,1, label= wt.a.round[5], pos=2) 
text(3,0, label= wt.a.round[6], pos=3) 

plot(xc[-7], yc[-7], xlab="X coordinate", ylab="Y coordinate", main="OK, Kriging weights (b)", pch=16)
points(xc[7], yc[7], pch=13)
text(0,0, label= wt.b.round[1], pos=4)    #plot the weights at each location
text(1,1, label= wt.b.round[2], pos=3) 
text(2,3, label= wt.b.round[3], pos=1) 
text(3,2, label= wt.b.round[4], pos=3)  
text(4,1, label= wt.b.round[5], pos=2) 
text(3,0, label= wt.b.round[6], pos=3)


# ii)
get_G_uk = function(m, si, dist, sv_model){
  # Compute the G (or Gamma) matrix for kriging system of equations:   G w=  D
  G = matrix(0, nrow=m+dim(si)[2], ncol=m+dim(si)[2]) 
  for (i in 1:m-1){
    for (j in 1:m-1){
      G[i,j] = sv_model(dist[i,j]) #c1 * exp(-dist[i,j]/phi)  
    }
  }
  G[m,0:(m-1)] = si[-7,1] #rbind(si[,1], rep(0,3))
  G[0:(m-1),m] = si[-7,1]
  G[m+1,0:(m-1)] = si[-7,2] #rbind(si[,2], rep(0,3))
  G[0:(m-1),m+1] = si[-7,2]
  G[m+dim(si)[2],0:(m-1)] = rep(1,m-1) #rbind(rep(1,m), rep(0,3))    # for Lagrange multiplier
  G[0:(m-1),m+dim(si)[2]] = rep(1,m-1) 
  #G[m:,m:] = 0 
  
  return(G)
}

get_g_uk = function(m, s0, dist, sv_model){
  D = rep(0,m+length(s0)) 
  for (j in 1:m){ 
    D[j] = sv_model(dist[m,j]) #c1 * exp (-dist[m,j]/phi) 
  }
  #D[m] = 1
  #D[-(0:m)] = s0
  D[m:(m+1)] = s0
  D[m+length(s0)] = 1
  return(D)
}

#      s1  s2  s3  s4  s5  s6  s0:  data locations + prediction site (s0)  
xc = c(0,  1,  2,  3,   4,  3,  2) 
yc = c(0,  1,  3,  2,   1,  0,  1) 
dist = get_dm(xc, yc)
m = length(xc)
si = cbind(xc, yc)
# a)
# Obtain the universal kriging predictor weights and universal kriging variance
# for prediction at s0. Compare the results obtained by ordinary kriging.

# gamma(|h|) = 1 - exp(-|h|/4)
Gamma.a2 = get_G_uk(m, si, dist, sv_a)
gamma.a2 = get_g_uk(m, si[7,], dist, sv_a)
# Obtain the weights and the Lagrange multiplier
wt.a2 = solve(Gamma.a2) %*% gamma.a2
wt.a2.round = round( wt.a2, digits=3 )
# Compute the prediction variance of the estimate: 
var_z_hat.a2 = t(wt.a2)%*%gamma.a2
var_z_hat.a2.round = round(var_z_hat.a2, digits=3)

# gamma(|h|) = 0.25 + 0.75 * ( 1 - exp(-|h|/4) ) [0.25 = nugget]
Gamma.b2 = get_G_uk(m, si, dist, sv_b)
gamma.b2 = get_g_uk(m, si[7,], dist, sv_b)
# Obtain the weights and the Lagrange multiplier
wt.b2 = solve(Gamma.b2) %*% gamma.b2
wt.b2.round = round( wt.b2, digits=3 )
# Compute the prediction variance of the estimate: 
var_z_hat.b2 = t(wt.b2)%*%gamma.b2
var_z_hat.b2.round = round(var_z_hat.b2, digits=3)

# plotting
par(mfrow=c(1,2))
plot(xc[-7], yc[-7], xlab="X coordinate", ylab="Y coordinate", main="UK, Kriging weights (a)", pch=16)
points(xc[7], yc[7], pch=13)
text(0,0, label= wt.a2.round[1], pos=4)    #plot the weights at each location
text(1,1, label= wt.a2.round[2], pos=3) 
text(2,3, label= wt.a2.round[3], pos=1) 
text(3,2, label= wt.a2.round[4], pos=3)  
text(4,1, label= wt.a2.round[5], pos=2) 
text(3,0, label= wt.a2.round[6], pos=3) 

plot(xc[-7], yc[-7], xlab="X coordinate", ylab="Y coordinate", main="UK, Kriging weights (b)", pch=16)
points(xc[7], yc[7], pch=13)
text(0,0, label= wt.b2.round[1], pos=4)    #plot the weights at each location
text(1,1, label= wt.b2.round[2], pos=3) 
text(2,3, label= wt.b2.round[3], pos=1) 
text(3,2, label= wt.b2.round[4], pos=3)  
text(4,1, label= wt.b2.round[5], pos=2) 
text(3,0, label= wt.b2.round[6], pos=3) 