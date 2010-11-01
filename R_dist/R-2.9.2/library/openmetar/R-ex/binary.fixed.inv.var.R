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
    results <- NULL
    if (length(binaryData@g1O1) == 1 || length(binaryData@y) == 
        1) {
        res <- get.res.for.one.binary.study(binaryData, params)
        results <- list(summary = res)
    }
    else {
        res <- rma.uni(yi = binaryData@y, sei = binaryData@SE, 
            slab = binaryData@studyNames, level = params$conf.level, 
            digits = params$digits, method = "FE", add = params$adjust, 
            to = params$to)
        degf <- res$k - res$p
        modelTitle <- paste("Fixed-Effects Model - Inverse Variance (k = ", 
            res$k, ")", sep = "")
        summaryDisp <- createSummaryDisp(res, params, degf, modelTitle)
        summaryDisp
        forest_path <- "./r_tmp/forest.png"
        plotData <- create.plot.data.binary(binaryData, params, 
            res)
        forest.plot(plotData, outpath = forest_path)
        images <- c(`forest plot` = forest_path)
        plot_names <- c(`forest plot` = "forest_plot")
        results <- list(images = images, summary = summaryDisp, 
            plot_names = plot_names)
    }
    results
  }



