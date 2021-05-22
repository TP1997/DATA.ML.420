library(splancs)

# Homogeneous Poisson Process (HPP) 
# A spatial point pattern from HPP on a polygon can be generated with the 
# R function 'csr':

# First, create a random set of a polygon(irregular boundary), r.poly: 
rx<- runif(20, min=0, max=3) 
ry<- runif(20, min=0, max=6) 
r.cl<- chull(as.points(rx,ry) ) 
r.poly <- cbind(x=rx[r.cl],y=ry[r.cl])

hp1<- csr( r.poly, 100)  # 100 points from HPP bounded by polygon 'r.poly' 
polymap(r.poly) 
points(hp1)


# Estimation of  the intensity (kernel estimator) 
data(bodmin)   # Locations of 35 granite tors (a peak of rocky mountain) on Bodmin Moor 
plot(bodmin$poly, asp=1, type="n") 
image(kernel2d(as.points(bodmin), bodmin$poly, h0=2, nx=100, ny=100), 
               add=T, col=terrain.colors(20)) 
pointmap(as.points(bodmin), add=T) 
polymap(bodmin$poly, add=T)

