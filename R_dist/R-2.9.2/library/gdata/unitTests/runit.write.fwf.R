### runit.write.fwf.R
###------------------------------------------------------------------------
### What: Unit tests for write.fwf
### $Id: runit.write.fwf.R 1300 2008-08-05 11:47:18Z ggorjan $
### Time-stamp: <2008-08-05 11:58:50 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ --- write.fwf ---

test.write.fwf <- function()
{

  ## 'x' must be a data.frame or matrix
  checkException(write.fwf(1:10))
  checkException(write.fwf(list(1:10)))

  ## only single value is allowed in 'na'
  checkException(write.fwf(data.frame(1:10, letters[1:10]), na=c("", " ")))

  ## Example dataset
  num <- round(c(733070.345678, 1214213.78765456, 553823.798765678,
                 1085022.8876545678,  571063.88765456, 606718.3876545678,
                 1053686.6, 971024.187656, 631193.398765456, 879431.1),
               digits=3)

  testData <- data.frame(num1=c(1:10, NA),
                         num2=c(NA, seq(from=1, to=5.5, by=0.5)),
                         num3=c(NA, num),
                         int1=c(as.integer(1:4), NA, as.integer(4:9)),
                         fac1=factor(c(NA, letters[1:9], "hjh")),
                         fac2=factor(c(letters[6:15], NA)),
                         cha1=c(letters[17:26], NA),
                         cha2=c(NA, "longer", letters[25:17]),
                         stringsAsFactors=FALSE)
  levels(testData$fac1) <- c(levels(testData$fac1), "unusedLevel")
  testData$Date <- as.Date("1900-1-1")
  testData$Date[2] <- NA
  testData$POSIXt <- as.POSIXct(strptime("1900-1-1 01:01:01", format="%Y-%m-%d %H:%M:%S"))
  testData$POSIXt[5] <- NA

  ## --- output ---
  ## is tested with regular tests

  ## --- formatInfo ---

  ## default output
  formatInfoT <- data.frame(colname=c("num1", "num2"),
                            nlevels=c(0, 0),
                            position=c(1, 4),
                            width=c(2, 3),
                            digits=c(0, 1),
                            exp=c(0, 0),
                            stringsAsFactors=FALSE)
  formatInfo  <- write.fwf(testData[, c("num1", "num2")], formatInfo=TRUE)
  checkEquals(formatInfo, formatInfoT)

  ## scientific notation
  dd <- options("digits"); options(digits = 7)
  testData2 <- data.frame(a=123, b=pi, c=1e8, d=1e222)
  formatInfo <- write.fwf(x=testData2, formatInfo=TRUE)
  checkEquals(formatInfo$width, c(3, 8, 5, 6))
  checkEquals(formatInfo$digits, c(0, 6, 0, 0))
  checkEquals(formatInfo$exp, c(0, 0, 2, 3))
  options(dd) ## reset old options

  ## 'na' can either decrease or increase the width
  ## --> values of int1 have width 1 and using na="" should not increase
  ##     the width
  formatInfo  <- write.fwf(testData[, "int1", drop=FALSE], formatInfo=TRUE,
                           na="")
  checkEquals(formatInfo$width, 1)
  ## --> values of int1 have width 1 and using na="1234" should increase
  ##     the width to 4
  formatInfo  <- write.fwf(testData[, "int1", drop=FALSE], formatInfo=TRUE,
                           na="1234")
  checkEquals(formatInfo$width, 4)

  ## rowCol
  formatInfoTR <- data.frame(colname=c("row", "num1", "num2"),
                             nlevels=c(11, 0, 0),
                             position=c(1, 4, 7),
                             width=c(2, 2, 3),
                             digits=c(0, 0, 1),
                             exp=c(0, 0, 0),
                             stringsAsFactors=FALSE)
  formatInfoR <- write.fwf(testData[, c("num1", "num2")], formatInfo=TRUE,
                           rownames=TRUE, rowCol="row")
  checkEquals(formatInfoR, formatInfoTR)

  ## quoteInfo alone does not have any effect
  formatInfoI <- write.fwf(testData[, c("num1", "num2")], formatInfo=TRUE,
                           quoteInfo=TRUE)
  checkEquals(formatInfoI, formatInfoT)

  ## quote
  formatInfoQ <- write.fwf(testData[, c("num1", "num2")], formatInfo=TRUE,
                           quote=TRUE)
  formatInfoTQ <-  formatInfoT
  formatInfoTQ$position <- c(1, 6)
  formatInfoTQ$width <- c(4, 5)
  checkEquals(formatInfoQ, formatInfoTQ)

  ## quote without quoteInfo
  formatInfoQI <- write.fwf(testData[, c("num1", "num2")], formatInfo=TRUE,
                            quote=TRUE, quoteInfo=FALSE)
  formatInfoTQI <-  formatInfoT
  formatInfoTQI$position <- c(2, 6)
  checkEquals(formatInfoQI, formatInfoTQI)

  ## width
  ## --> default width for num1 is 2
  formatInfo <- write.fwf(testData[, "num1", drop=FALSE], width=10, formatInfo=TRUE)
  checkEquals(formatInfo$width, 10)

  ## too small value in width (this also tests recycling)
  ## --> proper width for num1 is 2, while for num2 it is 3
  checkException(write.fwf(testData[, c("num1", "num2")], width=2))
  checkException(write.fwf(testData[, c("num1", "num2")], width=c(2, 1)))
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.write.fwf.R ends here
