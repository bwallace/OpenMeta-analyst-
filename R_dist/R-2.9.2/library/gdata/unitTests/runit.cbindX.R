### runit.cbindX.R
###------------------------------------------------------------------------
### What: Unit tests for cbindX
### $Id: runit.cbindX.R 1300 2008-08-05 11:47:18Z ggorjan $
### Time-stamp: <2008-08-05 13:40:49 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ --- cbindX ---

test.cbindX <- function()
{
  df1 <- data.frame(a=1:3, b=c("A", "B", "C"))
  df2 <- data.frame(c=as.character(1:5), a=5:1)

  ma1 <- matrix(as.character(1:4), nrow=2, ncol=2)
  ma2 <- matrix(1:6, nrow=3, ncol=2)

  df12test <- cbindX(df1, df2)
  df12stand <- data.frame(a=c(1:3, NA, NA),
                          b=c("A", "B", "C", NA, NA),
                          c=as.character(1:5),
                          a=5:1)
  names(df12stand)[4] <- "a"
  checkEquals(df12test, df12stand)

  ma12test <- cbindX(ma1, ma2)
  ma12stand <- matrix(as.character(c(1,   3, 1, 4,
                                     2,   4, 2, 5,
                                     NA, NA, 3, 6)), nrow=3, ncol=4, byrow=TRUE)
  checkEquals(ma12test, ma12stand)

  da11test <- cbindX(df1, ma1)
  da11stand <- data.frame(a=1:3,
                          b=c("A", "B", "C"),
                          as.character(c(1:2, NA)),
                          as.character(c(3:4, NA)))
  names(da11stand)[3:4] <- c("1", "2")
  checkEquals(da11test, da11stand)

  tmpTest <- cbindX(df1, df2, ma1, ma2)
  tmpStand <- data.frame(a=c(1:3, NA, NA),
                         b=c("A", "B", "C", NA, NA),
                         c=as.character(1:5),
                         a=5:1,
                         as.character(c(1:2, NA, NA, NA)),
                         as.character(c(3:4, NA, NA, NA)),
                         c(1:3, NA, NA),
                         c(4:6, NA, NA))
  names(tmpStand)[4:8] <- c("a", "1", "2", "1", "2")
  checkEquals(tmpTest, tmpStand)

  tmpTest <- cbindX(ma1, ma2, df1, df2)
  tmpStand <- data.frame(as.character(c(1:2, NA, NA, NA)),
                         as.character(c(3:4, NA, NA, NA)),
                         as.character(c(1:3, NA, NA)),
                         as.character(c(4:6, NA, NA)),
                         a=c(1:3, NA, NA),
                         b=c("A", "B", "C", NA, NA),
                         c=as.character(1:5),
                         a=5:1)
  names(tmpStand)[c(1:4, 8)] <- c("1", "2", "3", "4", "a")
  checkEquals(tmpTest, tmpStand)
}

### }}}
### {{{ Dear Emacs
### Local variables:
### folded-file: t
### end:
### }}}

###------------------------------------------------------------------------
### runit.cbindX.R ends here
