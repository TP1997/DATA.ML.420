library(spatstat)
setwd('/home/tuomas/R/Projects/DATA.ML420/datasets')
data = read.table('Rushes.txt', head=F)
rushes = ppp(data$V1, data$V2, c(0,1), c(0,1))

plot(rushes)

# a)
plot(Gest(rushes))
r = seq(0, sqrt(2)/6, by = 0.005) 
env = envelope(rushes, fun=Gest, r=r, nrank=2, nsim=99)
plot(env)

# b)
plot(Fest(rushes))
r = seq(0, 0.20, by= 0.001)
env = envelope(rushes, fun=Fest, r=r, nrank=2, nsim=99)
plot(env)

# c)
plot(Kest(rushes))
env = envelope(rushes, fun=Kest, r=r, nrank=2, nsim=99)
plot(env)

# d)
qt = quadrat.test(rushes, nx=3, ny=3) 
plot(rushes)
plot(qt, add=T)
qt$statistic
# No reason to reject H0 (completely random hypothesis)

# e)
clarkevans(rushes)
clarkevans.test(rushes)

# f)
# Due to the results, it seems that the rushes-dataset's point pattern is generated
# by completely random procedure.