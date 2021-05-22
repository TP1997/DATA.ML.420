x = c(61,63,64,68,71,73,75,              65)
y = c(139,140,129,128,140,141,128,       137)
z = c(477,696,227,646,606,791,783)

plot(x,y)
c0 = 0    # nugget
c1 = 10   # partial sill
a = 3.33

# c)
dd = cbind(x, y)
n = length(x)
# Distance matrix
dist= matrix(0, nrow=n, ncol=n)
for (i in 1:n) {
  for (j in i:n) {
    dist[i,j] = ((dd[i,1]-dd[j,1])^2 + (dd[i,2]-dd[j,2])^2)^.5
  }
}
# Gamma matrix
G = matrix(NA, nrow=n, ncol=n)
for (i in 1:n-1) {
  for (j in 1:n-1) {
    G[i,j] = c0 + c1* exp (-dist[i,j]/a)
  }
}
G[n,] = rep(1,n)
G[,n] = rep(1,n)
G[n,n] = 0

# gamma vector
D = rep(0,n) 
for (j in 1:n){ 
  D[j] = c0 + c1* exp (-dist[i,j]/a)
}
D[n] = 1

# Ordinary kriging weights
wt = solve(G)%*%D
wt
#              [,1]
#[1,]  1.000000e+00
#[2,]  3.608225e-16
#[3,] -1.249001e-16
#[4,]  1.942890e-16
#[5,] -2.220446e-16
#[6,]  0.000000e+00
#[7,]  0.000000e+00
#[8,]  0.000000e+00
# Predicted value
z.pred = t(wt)%*%cbind(c(z,0)) 
z.pred
# [1,]  477
# Prediction variance
pred.var = t(wt) %*% D
pred.var
# [1,]   10

# d)
c0 = 10 
# Gamma matrix
G.new = matrix(NA, nrow=n, ncol=n)
for (i in 1:n-1) {
  for (j in 1:n-1) {
    G.new[i,j] = c0 + c1* exp (-dist[i,j]/a)
  }
}
G.new[n,] = rep(1,n)
G.new[,n] = rep(1,n)
G.new[n,n] = 0

# gamma vector
D.new = rep(0,n) 
for (j in 1:n){ 
  D.new[j] = c0 + c1* exp (-dist[i,j]/a)
}
D.new[n] = 1

# Ordinary kriging weights
wt.new = solve(G.new)%*%D.new
wt.new
#[,1]
#[1,]  1.000000e+00
#[2,] -3.330669e-16
#[3,] -2.498002e-16
#[4,]  2.775558e-16
#[5,] -8.881784e-16
#[6,]  0.000000e+00
#[7,]  0.000000e+00
#[8,] -7.771561e-16

# Predicted value
z.pred.new = t(wt.new)%*%cbind(c(z,0)) 
z.pred.new
# [1,]  477

# Prediction variance
pred.var.new = t(wt.new)%*%D.new
pred.var.new
# [1,]   20

# The range of kriging weights seems to increase a little bit
# The predicted value stays the same
# The prediction variance increases. Since the added nugget effect increased the small scale
# variation, this seems reasonable behaviour.

# e)
get_G_uk = function(m, si, dist, sv_model){
  # Compute the G (or Gamma) matrix for kriging system of equations:   G w=  D
  G = matrix(0, nrow=m+dim(si)[2], ncol=m+dim(si)[2]) 
  for (i in 1:m-1){
    for (j in 1:m-1){
      G[i,j] = c0 + c1* exp (-dist[i,j]/a)
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

get_g_uk = function(m, s0, dist){
  D = rep(0,m+length(s0)) 
  for (j in 1:m){ 
    D[j] = c0 + c1* exp (-dist[i,j]/a)
  }
  #D[m] = 1
  #D[-(0:m)] = s0
  D[m:(m+1)] = s0
  D[m+length(s0)] = 1
  return(D)
}

x = c(61,63,64,68,71,73,75,              65)
y = c(139,140,129,128,140,141,128,       137)
dist
n = length(x)
si = cbind(x, y)

G.u = get_G_uk(n, si, dist)
D.u = get_g_uk(n, si[6,], dist)

# Universal kriging weights
wt.u = solve(G.u)%*%D.u
wt.u
#[,1]
#[1,]   0.3623349
#[2,]   0.4893181
#[3,]  -0.5353322
#[4,]   0.7068194
#[5,]   0.5485118
#[6,]   0.5601394
#[7,]  -1.1317914
#[8,]  -1.1448752
#[9,]  -0.4852382
#[10,] 140.8945246

# Predicted value
z.pred.u = t(wt.u)[1:n-1]%*%cbind(c(z)) 
z.pred.u
# [1,] 737.7598

# Prediction variance
pred.var.u = t(wt.u)%*%D.u
pred.var.u
# [1,] 8.900046








