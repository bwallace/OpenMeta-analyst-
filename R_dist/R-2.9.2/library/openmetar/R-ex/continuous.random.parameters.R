### Name: continuous.random.parameters

### Aliases: continuous.random.parameters
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    cont_metrics <- c("MD", "SMD")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list(rm.method = rm_method_ls, measure = cont_metrics, 
        conf.level = "float", digits = "float")
    defaults <- list(rm.method = "DL", measure = "MD", conf.level = 95, 
        digits = 3)
    var_order <- c("rm.method", "measure", "conf.level", "digits")
    parameters <- list(parameters = params, defaults = defaults, 
        var_order = var_order)
  }



