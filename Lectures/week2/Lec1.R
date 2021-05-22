# Surface plotting with a toy example

x1 = 5:10
x2 = 1:6
f = outer(x1,x2,'+')
f = f + rnorm(36,0,0.2)

par(mfrow=c(2,2))
image(x1,x2,f)  # 2D grayscale plot
persp(x1,x2,f)  # 3D surface plot
contour(x1,x2,f)

# Fit a first degree polynomial trend
x12 = expand.grid(x1,x2)  # matrix of locations to match f 'matrix'
fit.f = lm(as.vector(f)~x12[,1]+x12[,2])
image(x1,x2,matrix(fit.f$resid,6,6))
persp(x1,x2,matrix(fit.f$fit,6,6))
