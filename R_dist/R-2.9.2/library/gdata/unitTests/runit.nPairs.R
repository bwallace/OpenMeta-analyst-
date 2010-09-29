### runit.nPairs.R
###------------------------------------------------------------------------
### What: Number of variable pairs - unit tests
### $Id$
### Time-stamp: <2008-12-30 18:24:59 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ --- nPairs ---

test.nPairs <- function()
{
  ## 'x' must be a data.frame or a matrix
  x <- rpois(100, lambda=10)
  checkException(nPairs(x=x))
  checkException(nPairs(x=table(x)))

  test <- data.frame(V1=c(1, 2, 3, 4, 5),
                     V2=c(NA, 2, 3, 4, 5),
                     V3=c(1, NA, NA, NA, NA),
                     V4=c(1, 2, 3, NA, NA))
  testCheck <- matrix(as.integer(c(5, 4, 1, 3,
                                   4, 4, 0, 2,
                                   1, 0, 1, 1,
                                   3, 2, 1, 3)),
                      nrow=4, ncol=4, byrow=TRUE)

  testCheckNames <- testCheck
  colnames(testCheckNames) <- rownames(testCheckNames) <- colnames(test)

  checkIdentical(nPairs(x=test), testCheckNames)
  checkIdentical(nPairs(x=test, names=FALSE), testCheck)
  checkIdentical(nPairs(x=as.matrix(test)), testCheckNames)
  checkIdentical(nPairs(x=as.matrix(test), names=FALSE), testCheck)

  testCheck <- cbind(testCheckNames, as.integer(c(5, 4, 0, 0)))
  colnames(testCheck) <- c(colnames(test), "all")
  checkIdentical(nPairs(x=test, margin=TRUE), testCheck)
}

### }}}
### {{{ Dear Emacs
### Local variables:
### folded-file: t
### end:
### }}}

###------------------------------------------------------------------------
### runit.nPairs.R ends here
