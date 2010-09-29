### Name: read.ENVI & write.ENVI
### Title: Read and Write Binary Data in ENVI Format
### Aliases: read.ENVI write.ENVI
### Keywords: file

### ** Examples

  X = array(1:60, 3:5)
  write.ENVI(X, "temp.nvi")
  Y = read.ENVI("temp.nvi")
  stopifnot(X == Y)
  readLines("temp.nvi.hdr")
  
  d = c(20,30,40)
  X = array(runif(prod(d)), d)
  write.ENVI(X, "temp.nvi", interleave="bil")
  Y = read.ENVI("temp.nvi")
  stopifnot(X == Y)
  readLines("temp.nvi.hdr")
  
  file.remove("temp.nvi")
  file.remove("temp.nvi.hdr")



