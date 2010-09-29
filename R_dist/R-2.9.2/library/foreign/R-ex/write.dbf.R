### Name: write.dbf
### Title: Write a DBF File
### Aliases: write.dbf
### Keywords: file

### ** Examples

str(warpbreaks)
try1 <- paste(tempfile(), ".dbf", sep = "")
write.dbf(warpbreaks, try1, factor2char = FALSE)
in1 <- read.dbf(try1)
str(in1)
try2 <- paste(tempfile(), ".dbf", sep = "")
write.dbf(warpbreaks, try2, factor2char = TRUE)
in2 <- read.dbf(try2)
str(in2)
unlink(c(try1, try2))
## Don't show:
DF <- data.frame(a=c(1:3, NA), b=c(NA, rep(pi, 3)),
                 c=c(TRUE,NA, FALSE, TRUE), d=c("aa", "bb", NA, "dd"),
                 e=I(c("a1", NA, NA, "a4")))
DF$f <- as.Date(c("2001-01-01", NA, NA, "2004-10-26"))
str(DF)
write.dbf(DF, try2)
in2 <- read.dbf(try2)
str(in2)
unlink(try2)
## End Don't show


