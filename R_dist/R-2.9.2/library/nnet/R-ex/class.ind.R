### Name: class.ind
### Title: Generates Class Indicator Matrix from a Factor
### Aliases: class.ind
### Keywords: neural utilities

### ** Examples

# The function is currently defined as
class.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}



