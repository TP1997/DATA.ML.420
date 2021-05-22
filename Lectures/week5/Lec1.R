library(spatstat)

data(japanesepines)
data(redwood)
data(cells)  

summary(japanesepines)  # data information 
J = japanesepines
plot(J)

plot(redwood)
plot(cells)

J.q = quadratcount(J, nx=3, ny=3)     # counts in 3x3 quadrat
J.q 
plot(J)
plot(J.q, add=TRUE)

J.qt  <- quadrat.test(J, nx=3, ny=3)    # index of dispersion test: X^2 
plot(J)
plot(J.qt, add=TRUE)

# Estimation of  the intensity (kernel estimator) 
J.den <- density(J, sigma=15)    # kernel smoothed estimate of intensity function
plot(J.den) 
plot(J, add=TRUE)

# Clark & Evans test 
# R: ratio of the observed mean nearest neighbour distance in the pattern to that 
# expected for a Poisson point process of the same intensity. 
# >1 suggests Regularity/ordering, while <1 suggests clustering CSR if close to 1
mean(nndist(J)) # Calculate average nearest-neighbour distance: Y_bar
clarkevans(J)
clarkevans.test(J) 

# G Function: Distance to the Nearest Event
plot(Gest(J))    # plot of empirical distribution of NND’s
plot(Fest(J))    # edf of point to nearest event distances
plot(Kest(J))    # edf of Ripley's K-function

r <- seq(0, sqrt(2)/6, by = 0.005)     #set up the distances to compute G function over # in Ghat(r)= #(Y_i <= r)/ N 
env.j <- envelope( as(J, "ppp"), fun = Gest, r = r, nrank = 2, nsim = 99)
env.red <- envelope( as(redwood, "ppp"), fun = Gest, r = r, nrank = 2, nsim = 99)
env.cells <- envelope( as(cells, "ppp"), fun = Gest, r = r, nrank = 2, nsim = 99)

par(mfrow=c(1,3))
plot(env.j)
plot(env.red)
plot(env.cells)  

#  F Function: Distance from a Point to the Nearest Event

s <- seq(0, sqrt(2)/6, by = 0.001)   # set up the distances for points (to nearest neighbors)
Fenv.j <- envelope( as(J, "ppp"), fun = Fest, r = s, nrank = 2, nsim = 99)
Fenv.red <- envelope( as(redwood, "ppp"), fun = Fest, r = s, nrank = 2, nsim = 99)
Fenv.cells <- envelope( as(cells, "ppp"), fun = Fest, r = s, nrank = 2, nsim = 99)

plot(Fenv.j)
plot(Fenv.red)
plot(Fenv.cells)

# The second-order intensity and Ripley's K-function

#The K-function measures the number of events found up to a given distance of any 
# particular event and the value of the K-function for an HPP is K(s) = pi s^2
Kenv.j <- envelope( as(J, "ppp"), fun = Kest, r = r, nrank = 2, nsim = 99)
Kenv.red <- envelope( as(redwood, "ppp"), fun = Kest, r = r, nrank = 2, nsim = 99)
Kenv.cells <- envelope( as(cells, "ppp"), fun = Kest, r = r, nrank = 2, nsim = 99) 

plot(Kenv.j)
plot(Kenv.red)
plot(Kenv.cells)


