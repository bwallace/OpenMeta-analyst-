### Name: binary.fixed.peto.parameters

### Aliases: binary.fixed.peto.parameters
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    apply_adjustment_to = c("only0", "all")
    params <- list(conf.level = "float", digits = "float", adjust = "float", 
        to = apply_adjustment_to)
    defaults <- list(conf.level = 95, digits = 3, adjust = 0.5)
    var_order = c("conf.level", "digits", "adjust", "to")
    parameters <- list(parameters = params, defaults = defaults, 
        var_order = var_order)
  }



