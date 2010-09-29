### Name: bkde
### Title: Compute a Binned Kernel Density Estimate
### Aliases: bkde
### Keywords: distribution smooth

### ** Examples

data(geyser, package="MASS")
x <- geyser$duration
est <- bkde(x, bandwidth=0.25)
plot(est, type="l")



