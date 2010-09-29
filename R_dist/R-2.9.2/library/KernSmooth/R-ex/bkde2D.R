### Name: bkde2D
### Title: Compute a 2D Binned Kernel Density Estimate
### Aliases: bkde2D
### Keywords: distribution smooth

### ** Examples

data(geyser, package="MASS")
x <- cbind(geyser$duration, geyser$waiting)
est <- bkde2D(x, bandwidth=c(0.7, 7))
contour(est$x1, est$x2, est$fhat)
persp(est$fhat)



