### Name: read.systat
### Title: Obtain a Data Frame from a Systat File
### Aliases: read.systat
### Keywords: file

### ** Examples

summary(iris)
iris.s <- read.systat(system.file("files/Iris.syd", package="foreign")[1])
str(iris.s)
summary(iris.s)



