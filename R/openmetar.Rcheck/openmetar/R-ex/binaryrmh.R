### Name: binaryrmh

### Aliases: binaryrmh
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (binaryData) 
{
    if (!("BinaryData" %in% class(binaryData))) 
        stop("Binary data expected.")
    res <- rma.mh(binaryData@g1O1, binaryData@g1O2, binaryData@g2O1, 
        binaryData@g2O2)
    pdf("forest.pdf")
    forest.rma(res)
    dev.off()
    res
  }



