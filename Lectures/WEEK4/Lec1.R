# Lattice Data Analysis

library(spdep)
library(sp)

# North Carolina SIDS (sudden infant death syndrom) data set:  Cressie (1991) 

# Data for 100 counties in North Carolina includes counts of live births and 
# sudden infant deaths for two periods: 
# July 1974-June 1978 and July 1979 to June 1984.

# 'st_read': Read the SIDS data, boundaries of North Carolina, and the adjacency structure of North Carolina counties in GAL format. 

# SpatialPolygonsDataFrame object= nc; the class is defined in sp

nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
st_crs(nc) <- "+proj=longlat +datum=NAD27"
row.names(nc) <- as.character(nc$FIPSNO)       # names for variables in the data
names(nc) 
plot(nc)

# Freeman-Tukey square root transformation of count variables
ft.SID74 <- sqrt(1000)*(sqrt(nc$SID74/nc$BIR74) + sqrt((nc$SID74 + 1)/nc$BIR74) )
ft.NWBIR74 <- sqrt(1000)*(sqrt(nc$NWBIR74/nc$BIR74) + sqrt((nc$NWBIR74 + 1)/nc$BIR74))

# Define the Neighbor Relationship

# If contiguity is defined as areas sharing more than one boundary point 
sids.nbr<-poly2nb(nc, queen=FALSE) 
sids.nbq<-poly2nb(nc, queen=TRUE)     # Neighbors= areas sharing any boundary point (QUEEN)

# convert sf to spatial to use spdep
nc_sp <- sf:::as_Spatial(nc$geom)

plot(nc_sp, main="Queen's")     # plot of neighbour relationship
plot(sids.nbq, coordinates(nc_sp),add=TRUE, col="Red")

# knearneigh() creates matrix with index for the regions belonging to knn
# knn2nb() creates neighbourhood list
sids_kn1<-knn2nb(knearneigh(coordinates(nc_sp), k=1) )

plot(nc_sp, main="knn-1") 
plot(sids_kn1, coordinates(nc_sp),add=TRUE,col="blue")

sids_kn2<-knn2nb(knearneigh(coordinates(nc_sp), k=2) )
plot(nc_sp, main="knn-2") 
plot(sids_kn2, coordinates(nc_sp),add=TRUE,col="green")

# distance neighbours (still in degrees)
ndist<-unlist(nbdists(sids_kn1, coordinates(nc_sp)))
summary(ndist)

# create row standardized weights 
sids_kn1_w<-nb2listw(sids_kn1, style="W")     # style='W' is for row standardization 
# convert neighbors to a binary weight list
sids_mat_w<-nb2listw(sids.nbq, style="B", zero.policy=TRUE)

temp <- listw2mat(sids_mat_w) # convert weight list to a matrix to illustrate what's going on
#temp <- listw2mat(sd.w1)

temp[1:10,1:10] # print only the first 10 rows and 10 columns - don't want to print the HUGE matrix 



# Moran's I / Geary's c

sids.mo <-moran.test(nc$SID79/nc$BIR79, sids_kn1_w, alternative="two.sided")
sids.mo
sids.gr <-geary.test(nc$SID79, listw=sids_kn1_w)  # default: alternative= greater
sids.gr

# Monte Carlo estimate of Moran's I # Permutations, can save each randomization I and plot distribution
sids_mc<- moran.mc(nc$SID79/nc$BIR79,sids_kn1_w, nsim=999) 
sids_mc$statistic 

sids_mc$p.value

# res are the 1000 simulations under randomization
distr999<- hist(sids_mc$res,freq=TRUE,col="light blue",main="Permutation Test for Moran's I - 999 permutations",breaks=75)
lines(sids_mc$statistic,max(distr999$counts),type="h",col="red",lwd=2)


# Check the association between No. of sids cases and No. of non-white births 
sids_lm<-lm(ft.SID74~ft.NWBIR74,data=nc)
summary(sids_lm)
moran.test(sids_lm$residuals, listw = sids_kn1_w)


library(spatialreg)
sids.sar <- errorsarlm( ft.SID74 ~ 1, data=nc, sids_kn1_w, method="eigen", quiet=FALSE)
summary(sids.sar)

sidsrace.sar <-errorsarlm(ft.SID74~ft.NWBIR74, data=nc, sids_kn1_w, method="eigen", quiet=FALSE)
summary(sidsrace.sar)

moran.test(sidsrace.sar$residuals, listw=sids_kn1_w) 

#Non-symmetric spatial weights in CAR model 
sidsrace.carN <-spautolm(ft.SID74~ft.NWBIR74, data=nc, sids_kn1_w, method="eigen", family="CAR")
summary(sidsrace.carN) 

# CAR model with symmetric weights
sidsrace.car <-spautolm(ft.SID74~ft.NWBIR74, data=nc, sids_mat_w, method="eigen", family="CAR")
summary(sidsrace.car) 

# Read in shape files to create the map of residuals 
library(maptools)
ncSH <- readShapeSpatial(system.file("shapes/sids.shp", package = "spData")[1])
#ncSH <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)

res_racesar <-residuals(sidsrace.sar, type="response") 
ncSH$res_racesar <-res_racesar
spplot(ncSH,"res_racesar", at=quantile(ncSH$res_racesar), col.regions=heat.colors(10), main="Resids from SAR Race model")

fit_racesar <- fitted.values(sidsrace.sar, type="response")
ncSH$fit_racesar <- fit_racesar 
spplot(ncSH,"fit_racesar", at=quantile(ncSH$fit_racesar), col.regions=heat.colors(10), main="fitted from SAR Race model")

#Identify an outlier

lm_nc <- lm(ft.SID74 ~ 1, data = nc)
outl <- which.max(rstandard(lm_nc))
as.character(nc$NAME[outl])   # [1] "Anson"  :county

W <- listw2mat(sids_mat_w)

W.4 <- W[-outl, -outl]
sids_mat_w4 <- mat2listw(W.4)
nc2 <- nc[!(1:length(nc$CNTY_ID) %in% outl), ] #set this as a new data and fit again!
