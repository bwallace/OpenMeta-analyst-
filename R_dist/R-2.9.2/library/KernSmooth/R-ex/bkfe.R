### Name: bkfe
### Title: Compute a Binned Kernel Functional Estimate
### Aliases: bkfe
### Keywords: smooth

### ** Examples

data(geyser, package="MASS")
x <- geyser$duration
est <- bkfe(x, drv=4, bandwidth=0.3)



