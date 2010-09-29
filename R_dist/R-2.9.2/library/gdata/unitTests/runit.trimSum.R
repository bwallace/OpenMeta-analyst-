### runit.trimSum.R
###------------------------------------------------------------------------
### What: Unit tests for trimSum
### $Id$
### Time-stamp: <2008-12-20 11:58:50 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ --- trimSum ---

test.trimSum <- function()
{

  ## 'x' must be a vector - for now
  checkException(trimSum(matrix(1:10)))
  checkException(trimSum(data.frame(1:10)))
  checkException(trimSum(list(1:10)))

  ## 'x' must be numeric
  checkException(trimSum(letters))

  ## 'n' must be smaller than the length of x
  checkException(trimSum(x=1:10, n=11))
  checkException(trimSum(x=1, n=1))

  ## Default
  x <- trimSum(x=1:10, n=5)
  x2 <- c(1:4, 45)
  checkEquals(x, x2)  

  ## Left
  x <- trimSum(x=1:10, n=5, right=FALSE)
  x2 <- c(21, 7:10)
  checkEquals(x, x2)

  ## NA
  x <- trimSum(x=c(1:9, NA), n=5)
  x2 <- c(1:4, NA)
  checkEquals(x, x2)
  
  x <- trimSum(x=c(1:9, NA), n=5, na.rm=TRUE)
  x2 <- c(1:4, 35)
  checkEquals(x, x2)
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.trimSum.R ends here
