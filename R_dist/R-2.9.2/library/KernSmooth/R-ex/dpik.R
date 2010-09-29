### Name: dpik
### Title: Select a Bandwidth for Kernel Density Estimation
### Aliases: dpik
### Keywords: smooth

### ** Examples

data(geyser, package="MASS")
x <- geyser$duration
h <- dpik(x)
est <- bkde(x, bandwidth=h)
plot(est,type="l")



