### Name: impute.bin.data

### Aliases: impute.bin.data
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bin.data) 
{
    a <- NULL
    b <- NULL
    c <- NULL
    d <- NULL
    if (is.null(bin.data$estimate)) {
        if (isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$control.n.no.outcome) & 
            isnt.null(bin.data$tx.n.outcome) & isnt.null(bin.data$tx.n.no.outcome)) {
            a <- bin.data$tx.n.outcome
            b <- bin.data$tx.n.outcome + bin.data$tx.n.no.outcome
            c <- bin.data$control.n.outcome
            d <- bin.data$control.n.outcome + bin.data$control.n.no.outcome
        }
        else if (isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$control.N) & 
            isnt.null(bin.data$tx.n.outcome) & isnt.null(bin.data$tx.N)) {
            a <- bin.data$tx.n.outcome
            b <- bin.data$tx.N
            c <- bin.data$control.n.outcome
            d <- bin.data$control.N
        }
        else if (isnt.null(bin.data$control.p.outcome) & isnt.null(bin.data$control.N) & 
            isnt.null(bin.data$tx.p.outcome) & isnt.null(bin.data$tx.N)) {
            a <- bin.data$tx.p.outcome * bin.data$tx.N
            b <- bin.data$tx.N
            c <- bin.data$control.p.outcome * bin.data$control.N
            d <- bin.data$control.N
        }
    }
    else {
        if (isnt.null(bin.data$estimate) & isnt.null(bin.data$l_ci) & 
            isnt.null(bin.data$u_ci) & isnt.null(bin.data$control.n.outcome) & 
            isnt.null(bin.data$tx.n.outcome)) {
            a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2
            b <- log(bin.data$estimate)
            c <- sqrt(a) * (ln(bin.data$u_ci) - b)/1.96
        }
        else if (isnt.null(bin.data$estimate) & isnt.null(bin.data$pval) & 
            isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)) {
            a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2
            b <- log(bin.data$estimate)
            c <- b * sqrt(a)/qnorm(bin.data$pval/2)
        }
        else if (isnt.null(bin.data$log.estimate) & isnt.null(bin.data$log.se) & 
            isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)) {
            a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2
            b <- bin.data$log.estimate
            c <- bin.data$log.se * sqrt(a)
        }
        else if (isnt.null(bin.data$log_estimate) & isnt.null(bin.data$pval) & 
            isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)) {
            a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2
            b <- log(bin.data$log.estimate)
            c <- b * sqrt(a)/qnorm(bin.data$pval/2)
        }
    }
    data.frame(a = a, b = b, c = c, d = d)
  }



