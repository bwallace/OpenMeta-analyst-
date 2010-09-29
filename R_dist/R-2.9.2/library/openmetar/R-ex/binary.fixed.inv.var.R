### Name: binary.fixed.inv.var

### Aliases: binary.fixed.inv.var
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (binaryData, params) 
{
    if (!("BinaryData" %in% class(binaryData))) 
        stop("Binary data expected.")
    res <<- rma.uni(yi = binaryData@y, sei = binaryData@SE, slab = binaryData@studyNames, 
        level = params$conf.level, digits = params$digits, method = "FE", 
        add = params$adjust, to = params$to)
    forest_path <- "./r_tmp/forest.png"
    png(forest_path)
    forest_plot <- forest.rma(res, digits = params$digits)
    dev.off()
    images <- c(`forest plot` = forest_path)
    plot_names <- c(`forest plot` = "forest_plot")
    results <- list(images = images, summary = res, plot_names = plot_names)
    results
  }



