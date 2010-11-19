### Name: check.1spell.res

### Aliases: check.1spell.res
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n, se) 
{
    succeeded <- TRUE
    comment <- ""
    if (!is.na(n)) {
        if (n <= 1) {
            comment <- "n<=1"
            succeeded <- FALSE
        }
    }
    if (se <= 0) {
        comment <- paste("se<=0", comment, sep = ", ")
        succeeded <- FALSE
    }
    return(list(succeeded = succeeded, comment = comment))
  }



