### runit.getDateTimeParts.R
###------------------------------------------------------------------------
### What: Extract date and time parts from ... - unit tests
### $Id$
### Time-stamp: <2008-12-30 22:41:18 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

num <- 1
cha <- "a"
fac <- factor(c("A"))

tYear <- as.character(c(2006, 1995, 1005, 3067))
tMonth <- c("01", "04", "06", "12")
tDay <- c("01", "12", "22", "04")
tDate <- paste( paste(tYear, tMonth, tDay, sep="-"), "GMT" )

tHour <- c("05", "16", "20", "03")
tMin <- c("16", "40", "06", "52")
tSec <- c("56", "34", "05", "15")
tTime <- paste(tHour, tMin, tSec, sep=":")

cDate <- as.Date(tDate)
cDatePOSIXct <- as.POSIXct(tDate)
cDatePOSIXlt <- as.POSIXlt(tDate)

### }}}
### {{{ --- getYear ---

test.getYear <- function()
{
  checkException(getYear(x=num))
  checkException(getYear(x=cha))
  checkException(getYear(x=fac))

  checkIdentical(getYear(x=cDate), tYear)
  checkIdentical(getYear(x=cDatePOSIXct), tYear)
  checkIdentical(getYear(x=cDatePOSIXlt), tYear)
}

### }}}
### {{{ --- getMonth ---

test.getMonth <- function()
{
  checkException(getMonth(x=num))
  checkException(getMonth(x=cha))
  checkException(getMonth(x=fac))

  checkIdentical(getMonth(x=cDate), tMonth)
  checkIdentical(getMonth(x=cDatePOSIXct), tMonth)
  checkIdentical(getMonth(x=cDatePOSIXlt), tMonth)
}

### }}}
### {{{ --- getDay ---

test.getDay <- function()
{
  checkException(getDay(x=num))
  checkException(getDay(x=cha))
  checkException(getDay(x=fac))

  checkIdentical(getDay(x=cDate), tDay)
  checkIdentical(getDay(x=cDatePOSIXct), tDay)
  checkIdentical(getDay(x=cDatePOSIXlt), tDay)
}

### }}}
### {{{ --- getHour ---

test.getHour <- function()
{
  checkException(getHour(x=num))
  checkException(getHour(x=cha))
  checkException(getHour(x=fac))

##  checkIdentical(getHour(x=cDate), tHour)
}

### }}}
### {{{ --- getMin ---

test.getMin <- function()
{
  checkException(getMin(x=num))
  checkException(getMin(x=cha))
  checkException(getMin(x=fac))

##  checkIdentical(getMin(x=cDate), tMin)
}

### }}}
### {{{ --- getSec ---

test.getSec <- function()
{
  checkException(getSec(x=num))
  checkException(getSec(x=cha))
  checkException(getSec(x=fac))

##  checkIdentical(getSec(x=cDate), tSec)
}

### }}}
### {{{ Dear Emacs
### Local variables:
### folded-file: t
### end:
### }}}

###------------------------------------------------------------------------
### runit.getDateTimeParts.R ends here
