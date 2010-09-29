### Name: read.spss
### Title: Read an SPSS Data File
### Aliases: read.spss
### Keywords: file

### ** Examples

## Not run: 
##D read.spss("datafile")
##D ## don't convert value labels to factor levels
##D read.spss("datafile", use.value.labels = FALSE)
##D ## convert value labels to factors for variables with at most
##D ## ten distinct values.
##D read.spss("datafile", max.val.labels = 10)
## End(Not run)



