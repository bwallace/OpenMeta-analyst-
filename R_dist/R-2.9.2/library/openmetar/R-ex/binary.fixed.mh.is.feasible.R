### Name: binary.fixed.mh.is.feasible

### Aliases: binary.fixed.mh.is.feasible
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (binaryData) 
{
    length(binaryData@g1O1) == length(binaryData@g1O2) && length(binaryData@g1O2) == 
        length(binaryData@g2O1) && length(binaryData@g2O1) == 
        length(binaryData@g2O2) && length(binaryData@g1O1) > 
        0
  }



