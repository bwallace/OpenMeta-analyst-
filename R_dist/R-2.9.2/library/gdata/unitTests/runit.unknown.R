### runit.unknown.R
###------------------------------------------------------------------------
### What: Tests for Change given unknown value to NA and vice versa methods
### $Id: runit.unknown.R 993 2006-10-30 17:10:08Z ggorjan $
### Time-stamp: <2006-10-30 17:46:21 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### {{{ --- Vectors ---

intUnk <- 9999
xInt        <- as.integer(c(NA,     1:2, NA,     5, 6, 7, 8, 9))
xIntUnk     <- as.integer(c(intUnk, 1:2, intUnk, 5, 6, 7, 8, 9))
xIntUnkTest <- xIntUnk %in% intUnk

numUnk <- 0
xNum        <- c(9999, NA, 1.5, NA, 5, 6, 7, 8, 9)
xNumUnk     <- c(9999, 0,  1.5, 0,  5, 6, 7, 8, 9)
xNumUnkTest <- xNumUnk %in% numUnk

chaUnk <- "notAvail"
chaUnk1 <- "-"
xCha        <- c("A", "B", NA,      "C", NA,      "-", "7", "8", "9")
xChaUnk     <- c("A", "B", chaUnk,  "C", chaUnk,  "-", "7", "8", "9")
xChaUnk1    <- c("A", "B", chaUnk1, "C", chaUnk1, "-", "7", "8", "9")
xChaUnkTest  <- xChaUnk %in% chaUnk
xChaUnk1Test <- xChaUnk %in% chaUnk1

facUnk <- "notAvail"
facUnk1 <- "NA"
xFac     <- factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", NA))
xFacUnk  <- factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", facUnk))
xFacUnk1 <- factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", facUnk1))
xFacUnkTest <-     c(0,   0,   0, 0,    0,    0,      0,      0,   1)
xFacUnkTest <- as.logical(xFacUnkTest)
xFacUnk1Test <-    c(0,   0,   0, 1,    1,    0,      0,      0,   1)
xFacUnk1Test <- as.logical(xFacUnk1Test)
xFac1    <- factor(c("A", "0", 0, NA, NA, intUnk, numUnk, "-", NA))

facLev <- "A"
xFacUnkLev <- factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", "A"))
xFacUnkLevTest <-    c(1,    0,  0, 0,    0,    0,      0,      0,   1)
xFacUnkLevTest <- as.logical(xFacUnkLevTest)

dateUnk <- as.Date("2006-08-14")
tmp <- as.Date("2006-08-15")
xDate     <- c(tmp, NA)
xDateUnk  <- c(tmp,   dateUnk)
xDateTest <- c(FALSE, TRUE)

xDate1Unk  <- c(tmp,   dateUnk, NA)
xDate1Test <- c(FALSE, TRUE,    FALSE)

POSIXltUnk <- strptime("2006-08-14", format="%Y-%m-%d")
tmp <- strptime("2006-08-15", format="%Y-%m-%d")
xPOSIXlt     <- c(tmp, NA)
xPOSIXltUnk  <- c(tmp,   POSIXltUnk)
xPOSIXltTest <- c(FALSE, TRUE)

xPOSIXlt1Unk  <- c(tmp,   POSIXltUnk, NA)
xPOSIXlt1Test <- c(FALSE, TRUE,       FALSE)

POSIXctUnk <- as.POSIXct(strptime("2006-08-14 01:01:01", format="%Y-%m-%d %H:%M:%S"))
tmp <- as.POSIXct(strptime("2006-08-15 01:01:01", format="%Y-%m-%d %H:%M:%S"))
xPOSIXct     <- c(tmp, NA)
xPOSIXctUnk  <- c(tmp, POSIXctUnk)
xPOSIXctTest <- xPOSIXltTest

xPOSIXct1Unk  <- c(tmp, POSIXctUnk, NA)
xPOSIXct1Test <- xPOSIXlt1Test

### }}}
### {{{ --- Lists and data.frames ---

xList <- list(xInt, xCha, xNum, xFac)
xListN <- list(int=xInt, cha=xCha, num=xNum, fac=xFac)
xListUnk <- list(xIntUnk, xChaUnk, xNumUnk, xFacUnk)
xListUnkTest <- list(xIntUnkTest, xChaUnkTest, xNumUnkTest, xFacUnkTest)
xListNUnk <- list(int=xIntUnk, cha=xChaUnk, num=xNumUnk, fac=xFacUnk)
xListNUnkTest <- list(int=xIntUnkTest, cha=xChaUnkTest, num=xNumUnkTest, fac=xFacUnkTest)

xDF <- as.data.frame(xListN)
xDF$cha <- as.character(xDF$cha)
xDFUnk <- as.data.frame(xListNUnk)
xDFUnk$cha <- as.character(xDFUnk$cha)
xDFUnkTest <- as.data.frame(xListNUnkTest)

unkC <- c(intUnk, chaUnk, numUnk, facUnk)
unkL <- list(intUnk, chaUnk, numUnk, facUnk)
unkLN <- list(num=numUnk, cha=chaUnk, fac=facUnk, int=intUnk) ## mixed as it is named
unkLMN <- list(cha=chaUnk, int=intUnk, num=c(intUnk, numUnk),
               fac=c(chaUnk1, facUnk))

xListMNUnkF <- list(int=as.integer(c(9999, 1, 2, 9999, 5, 6, 7, 8, 9)),
                   cha=c("A", "B", "notAvail", "C", "notAvail", "-", "7", "8", "9"),
                   num=c(9999, 0, 1.5, 0, 5, 6, 7, 8, 9),
                   fac=factor(c("A", "0", "0", "NA", "NA", 9999, "0", "-", "notAvail")))
xListMNUnkFTest <- list(int=c(1, 0, 0, 1, 0, 0, 0, 0, 0),
                        cha=c(0, 0, 1, 0, 1, 0, 0, 0, 0),
                        num=c(1, 1, 0, 1, 0, 0, 0, 0, 0),
                        fac=c(0, 0, 0, 0, 0, 0, 0, 1, 1))
xListMNUnkFTest <- lapply(xListMNUnkFTest, as.logical)
xListMNF <- list(int=as.integer(c(NA, 1, 2, NA, 5, 6, 7, 8, 9)),
                 cha=c("A", "B", NA, "C", NA, "-", "7", "8", "9"),
                 num=c(NA, NA, 1.5, NA, 5, 6, 7, 8, 9),
                 fac=factor(c("A", "0", "0", "NA", "NA", "9999", "0", NA, NA)))

xDFMUnkF <- as.data.frame(xListMNUnkF)
xDFMUnkF$cha <- as.character(xDFMUnkF$cha)
xDFMUnkFTest <- as.data.frame(xListMNUnkFTest)
xDFMF <- as.data.frame(xListMNF)
xDFMF$cha <- as.character(xDFMF$cha)

unk1 <- 555555
xListUnk1 <- list(as.integer(c(unk1, 1, 2, unk1, 5, 6, 7, 8, 9)),
                  c("A", "B", unk1, "C", unk1, "-", "7", "8", "9"),
                  c(9999, unk1, 1.5, unk1, 5, 6, 7, 8, 9),
                  factor(c("A", "0", "0", "NA", "NA", "9999", "0", "-", unk1)))
xListUnk1Test <- lapply(xListUnk1, function(x) x %in% unk1)
xListNUnk1 <- xListUnk1
names(xListNUnk1) <- c("int", "cha", "num", "fac")
xDFUnk1 <- as.data.frame(xListNUnk1)
xDFUnk1$cha <- as.character(xDFUnk1$cha)
xDFUnk1Test <- as.data.frame(xListUnk1Test)
names(xDFUnk1Test) <- names(xListNUnk1)

unkC2 <- c(0, "notAvail")
xListUnk2 <- list(as.integer(c(unkC2[1], 1, 2, unkC2[1], 5, 6, 7, 8, 9)),
                  c("A", "B", unkC2[2], "C", unkC2[2], "-", "7", "8", "9"),
                  c(9999, as.numeric(unkC2[1]), 1.5, as.numeric(unkC2[1]), 5, 6, 7, 8, 9),
                  factor(c("A", "0", "0", "NA", "NA", "9999", "0", "-", unkC2[2])))
xListNUnk2 <- xListUnk2
names(xListNUnk2) <- c("int", "cha", "num", "fac")
xDFUnk2 <- as.data.frame(xListNUnk2)
xDFUnk2$cha <- as.character(xDFUnk2$cha)

xListUnk2Test <- xListUnk2
xListUnk2Test[[1]] <- xListUnk2Test[[1]] %in% unkC2[1]
xListUnk2Test[[2]] <- xListUnk2Test[[2]] %in% unkC2[2]
xListUnk2Test[[3]] <- xListUnk2Test[[3]] %in% unkC2[1]
xListUnk2Test[[4]] <- xListUnk2Test[[4]] %in% unkC2[2]
xListNUnk2Test <- xListUnk2Test
names(xListNUnk2Test) <- names(xListNUnk2)
xDFUnk2Test <- as.data.frame(xListNUnk2Test)

unkL2 <- as.list(unkC2)
unkLN2 <- unkL2[c(2, 1)]
names(unkLN2) <- c("cha", "int")
xListUnk2a <- list(as.integer(c(NA, 1, 2, NA, 5, 6, 7, 8, 9)),
                  c("A", "B", unkLN2[[2]], "C", unkLN2[[2]], "-", "7", "8", "9"),
                  c(9999, NA, 1.5, NA, 5, 6, 7, 8, 9),
                  factor(c("A", "0", "0", "NA", "NA", "9999", "0", "-", unkLN2[[2]])))
xListUnk2aTest <- xListUnk2a
xListUnk2aTest[[1]] <- xListUnk2aTest[[1]] %in% unkLN2[1]
xListUnk2aTest[[2]] <- xListUnk2aTest[[2]] %in% unkLN2[2]
xListUnk2aTest[[3]] <- xListUnk2aTest[[3]] %in% unkLN2[1]
xListUnk2aTest[[4]] <- xListUnk2aTest[[4]] %in% unkLN2[2]

xList2a <- list(xListUnk2a[[1]],
                c("A", "B", NA, "C", NA, "-", "7", "8", "9"),
                xListUnk2a[[3]],
                factor(c("A", NA, NA, "NA", "NA", 9999, NA, "-", NA)))

### }}}
### {{{ --- Matrix ---

matUnk <- 9999
mat        <- matrix(1:25, nrow=5, ncol=5)
mat[1, 2] <- NA; mat[1, 4] <- NA; mat[2, 2] <- NA;
mat[3, 2] <- NA; mat[3, 5] <- NA; mat[5, 4] <- NA;
matUnk1       <- mat
matUnk1[1, 2] <- matUnk; matUnk1[1, 4] <- matUnk; matUnk1[2, 2] <- matUnk;
matUnk1[3, 2] <- matUnk; matUnk1[3, 5] <- matUnk; matUnk1[5, 4] <- matUnk;
matUnkTest <- matUnk1Test <- is.na(mat)

matUnk2Test <- matUnkTest | mat == 1

### }}}
### {{{ --- Use of unknown=list(.default=, ...) or similarly named vector ---

D1 <- "notAvail"
unkLND1 <- list(.default=D1)
xListUnkD1 <- list(as.integer(c(NA, 1:2, NA, 5, 6, 7, 8, 9)),
                   c("A", "B", D1, "C", D1, "-", "7", "8", "9"),
                   c(9999, NA, 1.5, NA, 5, 6, 7, 8, 9),
                   factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", D1)))
xListUnkD1Test <- lapply(xListUnkD1, function(x) x %in% D1)
xListD1 <- xList

xListNUnkD1     <- xListUnkD1
xListNUnkD1Test <- xListUnkD1Test
names(xListNUnkD1) <- names(xListNUnkD1Test) <- names(xListNUnk1)
xListND1 <- xListN

DSO2 <- c("notAvail", 5678)
unkLNDSO2 <- as.list(DSO2)
names(unkLNDSO2) <- c(".default", "someOther")
xListUnkDSO2 <- list(as.integer(c(NA, 1:2, NA, 5, 6, 7, 8, 9)),
                      c("A", "B", DSO2[1], "C", DSO2[1], "-", "7", "8", "9"),
                      c(9999, NA, 1.5, NA, 5, 6, 7, 8, 9),
                      factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", DSO2[2])))
xListUnkDSO2Test <- lapply(xListUnkDSO2, function(x) x %in% DSO2)

unkLND3 <- list(.default="notAvail", num=0, int=9999)
xListNUnkD3 <- list(int=as.integer(c(unkLND3[[3]], 1:2, unkLND3[[3]], 5, 6, 7, 8, 9)),
                    cha=c("A", "B", unkLND3[[1]], "C", unkLND3[[1]], "-", "7", "8", "9"),
                    num=c(9999, unkLND3[[2]], 1.5, unkLND3[[2]], 5, 6, 7, 8, 9),
                    fac=factor(c("A", "0", 0, "NA", "NA", intUnk, numUnk, "-", unkLND3[[1]])))
xListNUnkD3Test <- xListNUnkD3
xListNUnkD3Test$int <- xListNUnkD3Test$int %in% unkLND3[[3]]
xListNUnkD3Test$cha <- xListNUnkD3Test$cha %in% unkLND3[[1]]
xListNUnkD3Test$num <- xListNUnkD3Test$num %in% unkLND3[[2]]
xListNUnkD3Test$fac <- xListNUnkD3Test$fac %in% unkLND3[[1]]

unkLND2E <- list(.default="notAvail", 9999)

### }}}

### }}}
### {{{ --- isUnknown ---

test.isUnknown <- function()
{
  ## --- base methods for vectors ---

  ## base ...
  checkIdentical(isUnknown(xIntUnk, unknown=as.integer(intUnk)), xIntUnkTest)
  checkIdentical(isUnknown(xIntUnk, unknown=intUnk),             xIntUnkTest)
  checkIdentical(isUnknown(xNumUnk, unknown=numUnk),             xNumUnkTest)
  checkIdentical(isUnknown(xNumUnk, unknown=as.integer(numUnk)), xNumUnkTest)
  checkIdentical(isUnknown(xChaUnk, unknown=chaUnk),             xChaUnkTest)
  checkIdentical(isUnknown(xFacUnk, unknown=facUnk),             xFacUnkTest)

  ## multiple values are allowed for vector methods in vector or list form
  checkIdentical(isUnknown(xIntUnk, unknown=unkC), xIntUnkTest)
  checkIdentical(isUnknown(xIntUnk, unknown=unkL), xIntUnkTest)

  ## NA's in factors
  checkIdentical(isUnknown(xFacUnk1, unknown=facUnk1), xFacUnk1Test)
  facNA <- factor(c("0", 1, 2, 3, NA, "NA"))
  facNATest <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  checkIdentical(isUnknown(facNA), facNATest)

  ## Date-time classes
  checkIdentical(isUnknown(xDateUnk, unknown=dateUnk), xDateTest)
  checkIdentical(isUnknown(xDate1Unk, unknown=dateUnk), xDate1Test)
  checkIdentical(isUnknown(xPOSIXltUnk, unknown=POSIXltUnk), xPOSIXltTest)
  checkIdentical(isUnknown(xPOSIXlt1Unk, unknown=POSIXltUnk), xPOSIXlt1Test)
  checkIdentical(isUnknown(xPOSIXctUnk, unknown=POSIXctUnk), xPOSIXctTest)
  checkIdentical(isUnknown(xPOSIXct1Unk, unknown=POSIXctUnk), xPOSIXct1Test)

  ## --- lists and data.frames ---

  ## with vector of single unknown values
  checkIdentical(isUnknown(xListUnk, unknown=unkC), xListUnkTest)
  checkIdentical(isUnknown(xDFUnk, unknown=unkC),   xDFUnkTest)

  ## with list of single unknown values
  checkIdentical(isUnknown(xListUnk, unknown=unkL), xListUnkTest)
  checkIdentical(isUnknown(xDFUnk, unknown=unkL),   xDFUnkTest)

  ## with named list of single unknown values
  checkIdentical(isUnknown(xListNUnk, unknown=unkLN), xListNUnkTest)
  checkIdentical(isUnknown(xDFUnk, unknown=unkLN),    xDFUnkTest)

  ## with named list of multiple unknown values - valid here
  checkIdentical(isUnknown(xListMNUnkF, unknown=unkLMN), xListMNUnkFTest)
  checkIdentical(isUnknown(xDFMUnkF, unknown=unkLMN), xDFMUnkFTest)

  ## with single unknown value - recycling
  checkIdentical(isUnknown(xListUnk1, unknown=unk1), xListUnk1Test)
  checkIdentical(isUnknown(xDFUnk1,   unknown=unk1), xDFUnk1Test)

  ## with vector of two unknown values - recycling
  checkIdentical(isUnknown(xListUnk2, unknown=unkC2), xListUnk2Test)
  checkIdentical(isUnknown(xDFUnk2,   unknown=unkC2), xDFUnk2Test)

  ## with list of two unknown values - recycling
  checkIdentical(isUnknown(xListUnk2, unknown=unkL2), xListUnk2Test)
  checkIdentical(isUnknown(xDFUnk2,   unknown=unkL2), xDFUnk2Test)

  ## list(.default=)
  checkIdentical(isUnknown(x=xListUnkD1, unknown=unkLND1), xListUnkD1Test)
  ## list(.default=, someOther=) we do not know someOther, but should work
  ## as x is not named
  checkIdentical(isUnknown(x=xListUnkDSO2, unknown=unkLNDSO2), xListUnkDSO2Test)
  ## list(.default=) in named list
  checkIdentical(isUnknown(x=xListNUnkD1, unknown=unkLND1), xListNUnkD1Test)
  ## list(.default=, someOther=) OK if someOther is in the named list
  checkIdentical(isUnknown(x=xListNUnkD3, unknown=unkLND3), xListNUnkD3Test)
  ## list(.default=, 99) ERROR as we do not know where to apply 99
  checkException(isUnknown(x=xListNUnk, unknown=unkLND2E))

  ## --- matrix ---

  checkIdentical(isUnknown(x=mat, unknown=NA), matUnkTest)
  checkIdentical(isUnknown(x=matUnk1, unknown=matUnk), matUnkTest)
  checkIdentical(isUnknown(x=matUnk1, unknown=c(1, matUnk)), matUnk2Test)
}

### }}}
### {{{ --- unknownToNA ---

test.unknownToNA <- function()
{
  ## --- base methods for vectors ---

  ## base ...
  checkIdentical(unknownToNA(xIntUnk, as.integer(intUnk)), xInt)
  checkIdentical(unknownToNA(xIntUnk, intUnk),             xInt) ## with numeric
  checkIdentical(unknownToNA(xNumUnk, numUnk),             xNum)
  checkIdentical(unknownToNA(xNumUnk, as.integer(numUnk)), xNum)
  checkIdentical(unknownToNA(xChaUnk, chaUnk),             xCha)
  checkIdentical(unknownToNA(xChaUnk, chaUnk),             xCha)
  checkIdentical(unknownToNA(xFacUnk, facUnk),             xFac)

  ## multiple values are allowed for vector methods in vector or list form
  checkIdentical(unknownToNA(xIntUnk, unknown=unkC), xInt)
  checkIdentical(unknownToNA(xIntUnk, unknown=unkL), xInt)

  ## NA's in factors
  checkIdentical(unknownToNA(xFacUnk1, unknown=facUnk1), xFac1)
  facNA <- factor(c("0", 1, 2, 3, NA, "NA"))
  facNATest <- factor(c("0", 1, 2, 3, NA, NA))
  checkIdentical(unknownToNA(x=facNA, unknown="NA"), facNATest)

  ## Date-time classes
  checkIdentical(unknownToNA(xDateUnk, unknown=dateUnk), xDate)
  checkIdentical(unknownToNA(xPOSIXltUnk, unknown=POSIXltUnk), xPOSIXlt)
  checkIdentical(unknownToNA(xPOSIXctUnk, unknown=POSIXctUnk), xPOSIXct)

  ## --- lists and data.frames ---

  ## with vector of single unknown values
  checkIdentical(unknownToNA(xListUnk, unknown=unkC), xList)
  checkIdentical(unknownToNA(xDFUnk, unknown=unkC), xDF)

  ## with list of single unknown values
  checkIdentical(unknownToNA(xListUnk, unknown=unkL), xList)
  checkIdentical(unknownToNA(xDFUnk, unknown=unkL), xDF)

  ## with named list of single unknown values
  checkIdentical(unknownToNA(xListNUnk, unknown=unkLN), xListN)
  checkIdentical(unknownToNA(xDFUnk, unknown=unkLN), xDF)

  ## with names list of multiple unknown values - must be an error
  checkIdentical(unknownToNA(xListMNUnkF, unknown=unkLMN), xListMNF)
  checkIdentical(unknownToNA(xDFMUnkF, unknown=unkLMN), xDFMF)

  ## with single unknown value - recycling
  checkIdentical(unknownToNA(xListUnk1, unknown=unk1), xList)
  checkIdentical(unknownToNA(xDFUnk1,   unknown=unk1), xDF)

  ## with vector of two unknown values - recycling
  checkIdentical(unknownToNA(xListUnk2, unknown=unkC2), xList)
  checkIdentical(unknownToNA(xDFUnk2,   unknown=unkC2), xDF)

  ## with list of two unknown values - recycling
  checkIdentical(unknownToNA(xListUnk2, unknown=unkL2), xList)
  checkIdentical(unknownToNA(xDFUnk2,   unknown=unkL2), xDF)

  ## with named list of two unknown values but x is not named so named list
  ## does not have any effect --> error as we do not know how to recycle
  checkException(unknownToNA(xListUnk2a, unknown=unkLN2))

  ## but we should get some results with named x
  checkIdentical(unknownToNA(xListNUnk2, unknown=unkL2), xListN)
  ## not also necesarilly with recycling of names lists, as it is
  ## not clear how to properly recycle named lists (only names that match
  ## can be really properly recycled)
  checkException(unknownToNA(xListNUnk2, unknown=unkLN2))
  checkIdentical(unknownToNA(xDFUnk2, unknown=unkL2), xDF)
  checkException(unknownToNA(xDFUnk2, unknown=unkLN2))

  ## list(.default=)
  checkIdentical(unknownToNA(x=xListUnkD1, unknown=unkLND1), xListD1)
  ## list(.default=, someOther=) we do not know someOther, but should work
  ## as x is not named
  checkIdentical(unknownToNA(x=xListUnkDSO2, unknown=unkLNDSO2), xList)
  ## list(.default=) in named list
  checkIdentical(unknownToNA(x=xListNUnkD1, unknown=unkLND1), xListND1)
  ## list(.default=, someOther=) OK if someOther is in the named list
  checkIdentical(unknownToNA(x=xListNUnkD3, unknown=unkLND3), xListN)
  ## list(.default=, 99) ERROR as we do not know where to apply 99
  checkException(unknownToNA(x=xListNUnk, unknown=unkLND2E))

  ## --- matrix ---

  checkEquals(unknownToNA(x=matUnk1, unknown=matUnk), mat)
}

### }}}
### {{{ --- NAToUnknown ---

test.NAToUnknown <- function()
{
  ## --- base methods for vectors ---

  ## base ...
  checkIdentical(NAToUnknown(xInt, as.integer(intUnk)), xIntUnk)
  checkIdentical(NAToUnknown(xInt, intUnk),             xIntUnk) ## with numeric
  checkIdentical(NAToUnknown(xNum, numUnk),             xNumUnk)
  checkIdentical(NAToUnknown(xNum, as.integer(numUnk)), xNumUnk)
  checkIdentical(NAToUnknown(xCha, chaUnk),             xChaUnk)
  checkIdentical(NAToUnknown(xCha, chaUnk),             xChaUnk)
  checkIdentical(NAToUnknown(xFac, facUnk),             xFacUnk)

  ## only single values are allowed for vector methods
  checkException(NAToUnknown(xInt, unknown=unkC))
  checkException(NAToUnknown(xInt, unknown=unkL))

  ## and they should not already be in x unless force=TRUE
  checkException(NAToUnknown(xCha, unknown=chaUnk1))
  checkIdentical(NAToUnknown(xCha, unknown=chaUnk1, force=TRUE), xChaUnk1)

  checkException(NAToUnknown(xFac, unknown=facLev))
  checkIdentical(NAToUnknown(xFac, unknown=facLev, force=TRUE), xFacUnkLev)

  ## NA's in factors
  checkIdentical(NAToUnknown(xFac, unknown=facUnk1, force=TRUE), xFacUnk1)
  facNA <- factor(c("0", 1, 2, 3, NA, NA))
  facNATest <- factor(c("0", 1, 2, 3, "NA", "NA"))
  checkIdentical(NAToUnknown(x=facNA, unknown="NA"), facNATest)

  ## Date-time classes
  checkIdentical(NAToUnknown(xDate, unknown=dateUnk), xDateUnk)
  checkIdentical(NAToUnknown(xPOSIXlt, unknown=POSIXltUnk), xPOSIXltUnk)
  checkIdentical(NAToUnknown(xPOSIXct, unknown=POSIXctUnk), xPOSIXctUnk)

  ## --- lists and data.frames ---

  ## with vector of single unknown values
  checkIdentical(NAToUnknown(xList, unknown=unkC), xListUnk)
  checkIdentical(NAToUnknown(xDF, unknown=unkC), xDFUnk)

  ## with list of single unknown values
  checkIdentical(NAToUnknown(xList, unknown=unkL), xListUnk)
  checkIdentical(NAToUnknown(xDF, unknown=unkL), xDFUnk)

  ## with named list of single unknown values
  checkIdentical(NAToUnknown(xListN, unknown=unkLN), xListNUnk)
  checkIdentical(NAToUnknown(xDF, unknown=unkLN), xDFUnk)

  ## with names list of multiple unknown values - must be an error
  checkException(NAToUnknown(xListN, unknown=unkLMN))
  checkException(NAToUnknown(xDF, unknown=unkLMN))

  ## with single unknown value - recycling
  checkIdentical(NAToUnknown(xList, unknown=unk1), xListUnk1)
  checkIdentical(NAToUnknown(xDF,   unknown=unk1), xDFUnk1)

  ## with vector of two unknown values - recycling
  checkIdentical(NAToUnknown(xList, unknown=unkC2), xListUnk2)
  checkIdentical(NAToUnknown(xDF,   unknown=unkC2), xDFUnk2)

  ## with list of two unknown values - recycling
  checkIdentical(NAToUnknown(xList, unknown=unkL2), xListUnk2)
  checkIdentical(NAToUnknown(xDF,   unknown=unkL2), xDFUnk2)

  ## with named list of two unknown values but x is not named so named list
  ## does not have any effect --> error as we do not know how to recycle
  checkException(NAToUnknown(xList, unknown=unkLN2))

  ## but we should get some results with named x
  checkIdentical(NAToUnknown(xListN, unknown=unkL2), xListNUnk2)
  ## not also necesarilly with recycling of names lists, as it is
  ## not clear how to properly recycle named lists (only names that match
  ## can be really properly recycled)
  checkException(NAToUnknown(xListN, unknown=unkLN2))
  checkIdentical(NAToUnknown(xDF, unknown=unkL2), xDFUnk2)
  checkException(NAToUnknown(xDF, unknown=unkLN2))

  ## list(.default=)
  checkIdentical(NAToUnknown(x=xList, unknown=unkLND1), xListUnkD1)
  ## list(.default=, someOther=) we do not know someOther, but should work
  ## as x is not named
  checkIdentical(NAToUnknown(x=xList, unknown=unkLNDSO2), xListUnkDSO2)
  ## list(.default=) in named list
  checkIdentical(NAToUnknown(x=xListN, unknown=unkLND1), xListNUnkD1)
  ## list(.default=, someOther=) OK if someOther is in the named list
  checkIdentical(NAToUnknown(x=xListN, unknown=unkLND3), xListNUnkD3)
  ## list(.default=, 99) ERROR as we do not know where to apply 99
  checkException(NAToUnknown(x=xListN, unknown=unkLND2E))

  ## --- matrix ---

  checkEquals(NAToUnknown(x=mat, unknown=matUnk), matUnk1)
}

### }}}
### {{{ Dear Emacs
### Local variables:
### folded-file: t
### End:
### }}}

###------------------------------------------------------------------------
### runit.unknown.R ends here
