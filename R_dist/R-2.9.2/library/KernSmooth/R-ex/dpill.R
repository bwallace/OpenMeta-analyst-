### Name: dpill
### Title: Select a Bandwidth for Local Linear Regression
### Aliases: dpill
### Keywords: smooth

### ** Examples

data(geyser, package = "MASS")
x <- geyser$duration
y <- geyser$waiting
plot(x, y)
h <- dpill(x, y)
fit <- locpoly(x, y, bandwidth = h)
lines(fit)



