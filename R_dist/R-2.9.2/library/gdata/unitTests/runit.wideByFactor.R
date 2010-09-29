### runit.wideByFactor.R
###------------------------------------------------------------------------
### What: Reshape by factor levels - unit tests
### $Id$
### Time-stamp: <2008-12-30 11:58:50 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ --- wideByFactor ---

test.wideByFactor <- function()
{
  n <- 10
  f <- 2
  tmp <- data.frame(y1=(1:n)/2,
                    y2=(n:1)*2,
                    f1=factor(rep(letters[1:f], n/2)),
                    f2=factor(c(rep(c("M"), n/2), rep(c("F"), n/2))),
                    c1=1:n,
                    c2=2*(1:n))
  
  ## 'x' must be a data.frame
  checkException(wideByFactor(x=1:10))
  checkException(wideByFactor(x=matrix(1:10)))
  ## 'factor' can be only of length one
  checkException(wideByFactor(x=tmp, factor=c("f1", "f2")))
  ## column defined in 'factor' must be a factor
  checkException(wideByFactor(x=tmp, factor="c1"))

  tmp2 <- wideByFactor(x=tmp, factor="f1", common=c("c1", "c2"), sort=FALSE)
  checkEquals(tmp2[c("c1", "c2")], tmp[c("c1", "c2")])
  checkEquals(names(tmp2), c("c1", "c2", "f1", "y1.a", "y2.a", "f2.a", "y1.b", "y2.b", "f2.b"))
  checkEquals(tmp2$y1.a, c(0.5, NA, 1.5, NA, 2.5, NA, 3.5, NA, 4.5, NA))
  checkEquals(tmp2$f2.a, factor(c("M", NA, "M", NA, "M", NA, "F", NA, "F", NA)))
  tmp2 <- wideByFactor(x=tmp, factor="f1", common=c("c1", "c2"), sort=TRUE, keepFactor=FALSE)
  checkEquals(tmp2$f2.a, factor(c("M", "M", "M", "F", "F", NA, NA, NA, NA, NA)))
  checkEquals(names(tmp2), c("c1", "c2", "y1.a", "y2.a", "f2.a", "y1.b", "y2.b", "f2.b"))
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.wideByFactor.R ends here
