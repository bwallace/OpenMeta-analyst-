### runit.bindData.R
###------------------------------------------------------------------------
### What: Bind two data frames - unit tests
### $Id$
### Time-stamp: <2008-12-30 11:58:50 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ --- bindData ---

test.bindData <- function()
{
  ## 'x'/'y' must be a data.frame
  checkException(bindData(x=1:10, y=1:10))
  checkException(bindData(x=matrix(1:10), y=matrix(1:10)))
  
  n1 <- 6; n2 <- 12; n3 <- 4
  ## Single trait 1
  num <- c(5:n1, 10:13)
  tmp1 <- data.frame(y1=rnorm(n=n1),
                     f1=factor(rep(c("A", "B"), n1/2)),
                     ch=letters[num],
                     fa=factor(letters[num]),
                     nu=(num) + 0.5,
                     id=factor(num), stringsAsFactors=FALSE)

  ## Single trait 2 with repeated records, some subjects also in tmp1 
  num <- 4:9
  tmp2 <- data.frame(y2=rnorm(n=n2),
                     f2=factor(rep(c("C", "D"), n2/2)),
                     ch=letters[rep(num, times=2)],
                     fa=factor(letters[rep(c(num), times=2)]),
                     nu=c((num) + 0.5, (num) + 0.25),
                     id=factor(rep(num, times=2)), stringsAsFactors=FALSE)

  ## Single trait 3 with completely distinct set of subjects
  num <- 1:4
  tmp3 <- data.frame(y3=rnorm(n=n3),
                     f3=factor(rep(c("E", "F"), n3/2)),
                     ch=letters[num],
                     fa=factor(letters[num]),
                     nu=(num) + 0.5,
                     id=factor(num), stringsAsFactors=FALSE)

  ## Combine all datasets
  tmp12 <- bindData(x=tmp1, y=tmp2, common=c("id", "nu", "ch", "fa"))
  tmp123 <- bindData(x=tmp12, y=tmp3, common=c("id", "nu", "ch", "fa"))

  checkEquals(names(tmp123), c("id", "nu", "ch", "fa", "y1", "f1", "y2", "f2", "y3", "f3"))
  checkEquals(rbind(tmp1["id"], tmp2["id"], tmp3["id"]), tmp123["id"])
  checkEquals(rbind(tmp1["fa"], tmp2["fa"], tmp3["fa"]), tmp123["fa"])
  checkEquals(is.na(tmp123$y1), c(rep(FALSE, times=n1), rep(TRUE, times=n2+n3)))
  checkEquals(is.na(tmp123$f1), c(rep(FALSE, times=n1), rep(TRUE, times=n2+n3)))
  checkEquals(is.na(tmp123$y2), c(rep(TRUE, times=n1), rep(FALSE, times=n2), rep(TRUE, times=n3)))
  checkEquals(is.na(tmp123$f2), c(rep(TRUE, times=n1), rep(FALSE, times=n2), rep(TRUE, times=n3)))
  checkEquals(is.na(tmp123$y3), c(rep(TRUE, times=n1+n2), rep(FALSE, times=n3)))
  checkEquals(is.na(tmp123$f3), c(rep(TRUE, times=n1+n2), rep(FALSE, times=n3)))
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.bindData.R ends here
