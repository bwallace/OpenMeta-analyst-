### Name: dpih
### Title: Select a Histogram Bin Width
### Aliases: dpih
### Keywords: smooth

### ** Examples

data(geyser, package="MASS")
x <- geyser$duration
h <- dpih(x)
bins <- seq(min(x)-0.1, max(x)+0.1+h, by=h)
hist(x, breaks=bins)



