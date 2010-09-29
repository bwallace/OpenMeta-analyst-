### Name: locpoly
### Title: Estimate Functions Using Local Polynomials
### Aliases: locpoly
### Keywords: smooth regression

### ** Examples

data(geyser, package = "MASS")
# local linear density estimate
x <- geyser$duration
est <- locpoly(x, bandwidth = 0.25)
plot(est, type = "l")

# local linear regression estimate
y <- geyser$waiting
plot(x, y)
fit <- locpoly(x, y, bandwidth = 0.25)
lines(fit)



